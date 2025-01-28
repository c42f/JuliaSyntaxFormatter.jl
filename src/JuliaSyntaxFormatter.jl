module JuliaSyntaxFormatter

using JuliaSyntax
using JuliaLowering
using StyledStrings
using Colors

using JuliaSyntax: Kind, is_leaf, numchildren, children, is_infix_op_call, is_postfix_op_call, is_dotted, sourcetext, has_flags, Tokenize
using JuliaLowering: SyntaxTree, provenance

# Somewhat-hacky implementation of is_operator for strings ... this should be
# in JuliaSyntax when it's further tested and cleaned up.
function is_operator(str::AbstractString; allow_dot=false)
    has_dot = false
    i = 1
    if isempty(str)
        return false
    elseif str[i] == '.'
        has_dot = true
        i = 2
    end
    c = str[i]
    i = nextind(str, i)
    if Tokenize.is_operator_start_char(c)
        if i > lastindex(str)
            # Fast path for (maybe dotted) single-character operators
            return allow_dot || !has_dot || c == '.'
        end
    else
        # Fast path for obviously-not-operator cases
        return false
    end
    # Slow path. At this point we know the first token is longer than a
    # character and might be an operator (eg, things like `++`, `+₁₁`) The
    # rules are more complex here so defer to the full tokenizer for now.
    toks = Tokenize.tokenize(str)
    op_t = Tokenize.next_token(toks)
    return JuliaSyntax.is_operator(op_t) && kind(Tokenize.next_token(toks)) == K"EndMarker"
end

function is_operator(x)
    JuliaSyntax.is_operator(x)
end

function _register_kinds()
    JuliaSyntax.register_kinds!(JuliaSyntaxFormatter, 2, [
        "BEGIN_FORMATTING_KINDS"
            "WS?"    # Zero or more whitespace characters
            "WS+"    # One or more whitespace characters
            "WS??"   # Zero whitespace characters
            "WS_NL"  # Whitespace containing at least one newline
            "WS_NL-" # Whitespace containing at least one newline with following token indented at parent level
            "WS_NL;" # Whitespace containing at least one newline or single semicolon
        "END_FORMATTING_KINDS"
    ])
end

_register_kinds()

struct FormatToken
    kind::Kind
    text::String
    is_trivia::Bool
    style
end

struct FormatRange
    kind::Kind
    range::UnitRange{Int}
end

struct FormatContext
    tokens::Vector{FormatToken}
    ranges::Vector{FormatRange}
    node_stack::Vector{Tuple{SyntaxTree,Any,Int,Int}}
    format_token_str::Function
    format_style::Function
end

function FormatContext(format_token_str, format_style)
    FormatContext(Vector{FormatToken}(),
                  Vector{FormatRange}(),
                  Vector{Tuple{SyntaxTree,Int,Int}}(),
                  format_token_str, format_style)
end

function JuliaSyntax.sourcetext(ctx::FormatContext)
    join(t.text for t in ctx.tokens)
end

function Base.show(io::IO, ::MIME"text/plain", ctx::FormatContext)
    print(io, "# FormatContext ($(length(ctx.tokens)) tokens, $(length(ctx.ranges)) nodes):\n")
    print(io, sourcetext(ctx))
end

function emit(ctx::FormatContext, ex::SyntaxTree)
    format_tree(ctx, ex)
end

function emit(ctx::FormatContext, k::Kind)
    # Placeholder whitespace - filled in later!
    str = k == K"WS+"      ? " "  :
          k == K"WS?"      ? " "  :
          k == K"WS??"     ? ""   :
          k == K"WS_NL"    ? "\n" :
          k == K"WS_NL-"   ? "\n" :
          k == K"WS_NL;"   ? "\n" :
          JuliaSyntax.untokenize(k, unique=true)
    style = ctx.node_stack[end][2]
    emit(ctx, FormatToken(k, str, true, style))
end

function emit(ctx::FormatContext, ::Nothing)
end

function emit(ctx::FormatContext, tok::FormatToken) 
    push!(ctx.tokens, tok)
end

function emit(ctx::FormatContext, ex::SyntaxTree, as_str::AbstractString)
    style = ctx.format_style(ex)
    emit(ctx, FormatToken(kind(ex), as_str, false, style))
end

function emit(ctx::FormatContext, tok, toks...)
    emit(ctx, tok)
    emit(ctx, toks...)
end

function start_node(ctx::FormatContext, ex)
    push!(ctx.ranges, FormatRange(kind(ex), (-1:-1)))
    style = ctx.format_style(ex)
    push!(ctx.node_stack, (ex, style, length(ctx.ranges), length(ctx.tokens)+1))
    nothing
end

function end_node(ctx::FormatContext)
    ex,style,i,j = pop!(ctx.node_stack)
    ctx.ranges[i] = FormatRange(kind(ex), (j:length(ctx.tokens)))
    nothing
end

function format_join_arglist(ctx::FormatContext, exs)
    first = true
    for ex in exs
        if kind(ex) == K"parameters"
            emit(ctx, K"WS??", K";", K"WS?")
            format_join(ctx, children(ex), K"WS??", K",", K"WS?")
        else
            if !first
                emit(ctx, K"WS??", K",", K"WS?")
            end
            format_tree(ctx, ex)
        end
        first = false
    end
end

function format_join(ctx::FormatContext, exs, sep_kinds...)
    first = true
    for ex in exs
        if first
            first = false
        else
            emit(ctx, sep_kinds...)
        end
        format_tree(ctx, ex)
    end
end

function format_block_body(ctx::FormatContext, ex)
    if kind(ex) == K"block"
        format_join(ctx, children(ex), K"WS_NL")
    else
        emit(ctx, ex)
    end
end

function format_generator_body(ctx::FormatContext, ex)
    emit(ctx, ex[1], K"WS+", K"for", K"WS+")
    format_join(ctx, ex[2:end], K"WS+", K"for", K"WS+")
end

# Transform an expression tree into a stream of tokens and ranges
function format_tree(ctx::FormatContext, ex)
    k = kind(ex)
    if is_leaf(ex)
        str = ctx.format_token_str(ex)
        style = ctx.format_style(ex)
        emit(ctx, FormatToken(kind(ex), str, false, style))
        return
    end
    start_node(ctx, ex)
    # Surface syntax
    if k == K"="
        emit(ctx, ex[1], K"WS?", K"=", K"WS?", ex[2])
    elseif k == K"."
        if numchildren(ex) == 1
            emit(ctx, K"(", K".", ex[1], K")")
        else
            emit(ctx, ex[1], K".", ex[2])
        end
    elseif k == K"::"
        if numchildren(ex) == 1
            emit(ctx, K"::", K"WS??", ex[1])
        else
            emit(ctx, ex[1], K"WS??", K"::", K"WS??", ex[2])
        end
    elseif k == K"..."
        k1 = kind(ex[1])
        if k1 == K"Identifier" || k1 == K"tuple"
            emit(ctx, ex[1], K"WS??", K"...")
        else
            # TODO: Need precedence rules to determine whether parens are
            # actually necessary here :-/
            emit(ctx, K"(", K"WS??", ex[1], K"WS??", K")", K"WS??", K"...")
        end
    elseif k == K"$"
        emit(ctx, K"$", K"(", K"WS??")
        format_join(ctx, children(ex), K"WS??", K",", K"WS?")
        emit(ctx, K"WS??", K")")
    elseif k == K"op="
        @assert numchildren(ex) == 3
        dottok = is_dotted(ex) ? K"." : nothing
        emit(ctx, ex[1], K"WS?", dottok, ex[2], K"=", K"WS?", ex[3])
    elseif is_operator(k)
        format_join(ctx, children(ex), K"WS?", k, K"WS?")
    elseif k == K"block"
        emit(ctx, K"begin", K"WS_NL")
        format_join(ctx, children(ex), K"WS_NL")
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"call" || k == K"dotcall"
        dottok = k == K"dotcall" ? K"." : nothing
        if is_infix_op_call(ex)
            # TODO: Precedence - add parens
            emit(ctx, ex[1], K"WS?", dottok, ex[2], K"WS?", ex[3])
        elseif is_postfix_op_call(ex)
            # TODO: Precedence - make parens optional
            emit(K"(", ex[1], K")", dottok, ex[2])
        else
            if isnothing(dottok)
                emit(ctx, ex[1])
            else
                if is_operator(ex[1]) || (kind(ex[1]) == K"Identifier" && is_operator(ex[1].name_val))
                    emit(ctx, K"(", dottok, ex[1], K")")
                else
                    emit(ctx, ex[1], dottok)
                end
            end
            emit(ctx, K"(", K"WS??")
            format_join_arglist(ctx, ex[2:end])
            emit(ctx, K"WS??", K")")
        end
    elseif k == K"comprehension"
        emit(ctx, K"[")
        format_generator_body(ctx, ex[1])
        emit(ctx, K"]")
    elseif k == K"curly"
        emit(ctx, ex[1], K"WS??", K"{", K"WS??")
        format_join_arglist(ctx, ex[2:end])
        emit(ctx, K"WS??", K"}")
    elseif k == K"doc"
        emit(ctx, ex[1], K"WS_NL", ex[2])
    elseif k == K"filter"
        emit(ctx, ex[1], K"WS?", K"if", K"WS?", ex[2])
    elseif k == K"for"
        emit(ctx, K"for")
        iterspecs = ex[1]
        emit(ctx, K"WS+")
        format_join(ctx, children(iterspecs), K"WS??", K",", K"WS?")
        stmts = ex[2]
        if numchildren(stmts) != 0
            emit(ctx, K"WS_NL")
            format_block_body(ctx, ex[2])
        end
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"function"
        emit(ctx, K"function", K"WS+", ex[1])
        if numchildren(ex) > 1 && numchildren(ex[2]) != 0
            emit(ctx, K"WS_NL")
            format_block_body(ctx, ex[2])
        end
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"generator"
        emit(ctx, K"(")
        format_generator_body(ctx, ex)
        emit(ctx, K")")
    elseif k == K"if"
        emit(ctx, K"if", K"WS+", ex[1], K"WS_NL")
        format_block_body(ctx, ex[2])
        e = numchildren(ex) > 2 ? ex[3] : nothing
        while !isnothing(e) && kind(e) == K"elseif"
            emit(ctx, K"WS_NL-", K"elseif", K"WS+", e[1], K"WS_NL")
            format_block_body(ctx, e[2])
            e = numchildren(e) > 2 ? e[3] : nothing
        end
        if !isnothing(e)
            emit(ctx, K"WS_NL-", K"else", K"WS_NL")
            format_block_body(ctx, e)
        end
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"iteration"
        format_join(ctx, children(ex), K"WS??", K",", K"WS?")
    elseif k == K"juxtapose"
        @assert numchildren(ex) == 2
        emit(ctx, ex[1], ex[2])
    elseif k == K"local" || k == K"global" || k == K"const"
        emit(ctx, k, K"WS+")
        format_join(ctx, children(ex), K"WS??", K",", K"WS?")
    elseif k == K"let"
        emit(ctx, K"let")
        bindings = ex[1]
        if numchildren(bindings) != 0
            emit(ctx, K"WS+")
            format_join(ctx, children(bindings), K"WS??", K",", K"WS?")
            emit(ctx, K"WS_NL")
        end
        stmts = ex[2]
        if numchildren(stmts) != 0
            if numchildren(bindings) == 0
                emit(ctx, K"WS_NL")
            end
            format_join(ctx, children(stmts), K"WS_NL")
            emit(ctx, K"WS_NL")
        end
        if numchildren(bindings) == 0 && numchildren(stmts) == 0
            emit(ctx, K"WS?", K";", K"WS?")
        end
        emit(ctx, K"end")
    elseif k == K"macrocall"
        format_join(ctx, children(ex), K"WS+")
    elseif k == K"quote"
        if kind(ex[1]) == K"block"
            emit(ctx, K"quote", K"WS_NL")
            format_join(ctx, children(ex[1]), K"WS_NL")
            emit(ctx, K"WS_NL", K"end")
        else
            emit(ctx, K":", K"(", K"WS??", ex[1], K"WS??", K")")
        end
    elseif k == K"ref"
        emit(ctx, ex[1], K"WS??", K"[")
        format_join_arglist(ctx, ex[2:end])
        emit(ctx, K"WS??", K"]")
    elseif k == K"return"
        emit(ctx, K"return", K"WS+", ex[1])
    elseif k == K"string"
        delim = has_flags(ex, JuliaSyntax.TRIPLE_STRING_FLAG) ? K"\"\"\"" : K"\""
        emit(ctx, delim)
        for (i,c) in enumerate(children(ex))
            if kind(c) == K"String"
                emit(ctx, c, escape_string(c.value, "\"\$")) # TODO: Format preservation and `keep="\n"` etc??
            else
                if kind(c) == K"Identifier" && (i == numchildren(ex) || (kind(ex[i+1]) == K"String" && !JuliaSyntax.Tokenize.is_identifier_char(first(ex[i+1].value))))
                    emit(ctx, K"$", c)
                else
                    emit(ctx, K"$", K"(", c, K")")
                end
            end
        end
        emit(ctx, delim)
    elseif k == K"struct"
        if has_flags(ex, JuliaSyntax.MUTABLE_FLAG)
            emit(ctx, K"mutable", K"WS+")
        end
        emit(ctx, K"struct", K"WS+", ex[1])
        stmts = ex[2]
        if numchildren(stmts) != 0
            emit(ctx, K"WS_NL")
            format_block_body(ctx, stmts)
        end
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"tuple"
        emit(ctx, K"(", K"WS??")
        format_join_arglist(ctx, children(ex))
        if numchildren(ex) == 1 && kind(ex[1]) != K"parameters"
            emit(ctx, K"WS??", K",")
        end
        emit(ctx, K"WS??", K")")
    elseif k == K"typed_comprehension"
        emit(ctx, ex[1], K"[")
        format_generator_body(ctx, ex[2])
        emit(ctx, K"]")
    # Lowering stuff
    else
        style = ctx.node_stack[end][2]
        emit(ctx, FormatToken(kind(ex), string("---",kind(ex),"---"), true, style), K"WS_NL")
        format_join(ctx, children(ex), K"WS_NL")
        emit(ctx, K"WS_NL", K"end")
    end
    end_node(ctx)
    ctx
end

"""
Modify each WS_NL to include trailing indentation
"""
function format_indents(ctx::FormatContext)
    line_number = 1 .+ cumsum(t.kind==K"WS_NL" for t in ctx.tokens)

    indent = zeros(Int, length(ctx.tokens))
    for i = 1:length(ctx.ranges)
        r = ctx.ranges[i]
        has_newline = line_number[r.range[end]] > line_number[r.range[1]]
        if has_newline
            j = r.range[end]
            while ctx.tokens[j].kind != K"WS_NL"
                j -= 1
            end
            j -= 1
            indent[r.range[1]:j] .+= 1
        end
    end

    for (i,tok) in enumerate(ctx.tokens)
        if tok.kind == K"WS_NL" || tok.kind == K"WS_NL-" 
            ind = indent[i] - (tok.kind == K"WS_NL-")
            ctx.tokens[i] = FormatToken(tok.kind, "\n"*"    "^ind, true, tok.style)
        end
    end
end

function format_token_str_default(ex::SyntaxTree; include_var_id=false)
    @assert is_leaf(ex)
    # See also JuliaLowering._value_string
    k = kind(ex)
    str = k == K"Identifier" || k == K"MacroName" || is_operator(k) ? ex.name_val :
          k == K"Placeholder" ? ex.name_val :
          k == K"SSAValue"   ? "%"                   :
          k == K"BindingId"  ? "#"                   :
          k == K"label"      ? "label"               :
          k == K"core"       ? "Core.$(ex.name_val)" :
          k == K"top"        ? "Base.$(ex.name_val)" : # top === Base except for bootstrap
          k == K"Symbol"     ? ":$(ex.name_val)"     :
          k == K"globalref"  ? "$(ex.mod).$(ex.name_val)" :
          k == K"symbolic_label" ? "@label $(ex.name_val)" :
          k == K"symbolic_goto" ? "@goto $(ex.name_val)" :
          k == K"slot"       ? "slot"   :
          k == K"TOMBSTONE"  ? "🪦"     :
          k == K"SourceLocation" ? "SourceLocation:$(JuliaSyntax.filename(ex)):$(join(JuliaSyntax.source_location(ex), ':'))" :
          begin
              val = get(ex, :value, nothing)
              isnothing(val) ? "❓" : repr(val)
          end
    id = get(ex, :var_id, nothing)
    if isnothing(id)
        id = get(ex, :id, nothing)
    end
    if !isnothing(id) && (k == K"SSAValue" || k == K"BindingId" || k == K"label" || k == K"slot" || include_var_id)
        idstr = replace(string(id),
                        "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
                        "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
        str = "$(str)$idstr"
    end
    if k == K"slot" || k == K"BindingId"
        p = provenance(ex)[1]
        while p isa SyntaxTree
            if kind(p) == K"Identifier"
                str = "$(str)/$(p.name_val)"
                break
            end
            p = provenance(p)[1]
        end
    end
    return str
end

function _convert_col(c)
    u8(x) = UInt8(clamp(round(255*x), typemin(UInt8), typemax(UInt8)))
    StyledStrings.SimpleColor(u8(c.r), u8(c.g), u8(c.b))
end

function distinguishable_faces(n)
    cols = Colors.distinguishable_colors(n, [RGB(1,1,1)], lchoices=range(70, stop=100, length=15))
    [StyledStrings.Face(foreground=_convert_col(c)) for c in cols]
end

"""
    formatsrc(ex; include_var_id, format_token_str, color_by)

Format syntax tree `ex` as Julia source code. `include_var_id` includes the
`var_id` attribute in the names of identifiers, when present.

`format_token_str` is a function which extracts the
string representation of a leaf of the tree.

`color_by` extracts the "style" of a node which can be any value; unique style
values will be formatted as random but distinguishable colors when printing. If
`color_by` is a Symbol, that property of the node will be used; if it's a
function `color_by(ex)` will be called to extract the style of `ex`.
"""
function formatsrc(ex::SyntaxTree;
                   include_var_id=false,
                   format_token_str=(ex)->format_token_str_default(ex; include_var_id),
                   color_by=nothing)
    format_style = isnothing(color_by) ? e->nothing                   :
                   color_by isa Symbol ? e->get(e, color_by, nothing) :
                   color_by
    ctx = FormatContext(format_token_str, format_style)
    JuliaSyntaxFormatter.format_tree(ctx, ex)
    JuliaSyntaxFormatter.format_indents(ctx)

    styles = unique(tok.style for tok in ctx.tokens)
    style_map = Dict(zip(styles, distinguishable_faces(length(styles))))

    io = Base.AnnotatedIOBuffer()
    styled_keys = [
        let
            s = Base.AnnotatedString(string(style_key))
            Base.annotate!(s, firstindex(s):lastindex(s), :face=>style)
        end
        for (style_key, style) in style_map
        if !isnothing(style_key)
    ]

    if sum(length.(styled_keys), init=0) > 80
        for s in styled_keys
            println(io, "# ", s)
        end
    else
        print(io, "# ")
        for s in styled_keys
            print(io, s, " ")
        end
        println(io)
    end

    idx = 1
    text = Base.AnnotatedString(sourcetext(ctx))
    for tok in ctx.tokens
        n = lastindex(tok.text)
        if !isnothing(tok.style)
            Base.annotate!(text, idx:idx-1+n, :face=>style_map[tok.style])
        end
        idx += sizeof(tok.text)
    end
    println(io, text)
    seek(io, 0)
    read(io, Base.AnnotatedString)
end

function __init__()
    _register_kinds()
end

end
