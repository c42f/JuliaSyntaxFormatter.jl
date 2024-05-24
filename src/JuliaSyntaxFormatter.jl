module JuliaSyntaxFormatter

using JuliaSyntax
using JuliaLowering
using StyledStrings
using Colors

using JuliaSyntax: Kind, haschildren, numchildren, children, is_infix_op_call, is_postfix_op_call, sourcetext, is_operator
using JuliaLowering: SyntaxTree

function _insert_kinds()
    JuliaSyntax.insert_kinds!(JuliaSyntaxFormatter, 2, [
        "BEGIN_FORMATTING_KINDS"
            "WS?"    # Zero or more whitespace characters
            "WS+"    # One or more whitespace characters
            "WS??"   # Zero whitespace characters
            "WS_NL"  # Whitespace containing at least one newline
            "WS_NL;" # Whitespace containing at least one newline or single semicolon
        "END_FORMATTING_KINDS"
    ])
end

_insert_kinds()

struct FormatToken
    kind::Kind
    text::String
    is_trivia::Bool
    style
end

FormatToken(kind, text, is_trivia) = FormatToken(kind, text, is_trivia, nothing)

struct FormatRange
    kind::Kind
    range::UnitRange{Int}
end

struct FormatContext
    tokens::Vector{FormatToken}
    ranges::Vector{FormatRange}
    node_stack::Vector{Tuple{SyntaxTree,Int,Int}}
    token_formatter::Function
end

function FormatContext(token_formatter)
    FormatContext(Vector{FormatToken}(),
                  Vector{FormatRange}(),
                  Vector{Tuple{SyntaxTree,Int,Int}}(),
                  token_formatter)
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
          k == K"WS_NL;"   ? "\n" :
          JuliaSyntax.untokenize(k, unique=true)
    emit(ctx, FormatToken(k, str, true, nothing))
end

function emit(ctx::FormatContext, tok::FormatToken) 
    push!(ctx.tokens, tok)
end

function emit(ctx::FormatContext, tok, toks...)
    emit(ctx, tok)
    emit(ctx, toks...)
end

function start_node(ctx::FormatContext, ex)
    push!(ctx.ranges, FormatRange(kind(ex), (-1:-1)))
    push!(ctx.node_stack, (ex, length(ctx.ranges), length(ctx.tokens)+1))
    nothing
end

function end_node(ctx::FormatContext)
    ex,i,j = pop!(ctx.node_stack)
    ctx.ranges[i] = FormatRange(kind(ex), (j:length(ctx.tokens)))
    nothing
end

function format_token(ctx::FormatContext, ex)
    ctx.token_formatter(ex)
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

# Transform an expression tree into a stream of tokens and ranges
function format_tree(ctx::FormatContext, ex)
    if !haschildren(ex)
        str, style = format_token(ctx, ex)
        emit(ctx, FormatToken(kind(ex), str, false, style))
        return
    end
    k = kind(ex)
    start_node(ctx, ex)
    # Surface syntax
    if k == K"="
        emit(ctx, ex[1], K"WS?", K"=", K"WS?", ex[2])
    elseif k == K"."
        emit(ctx, ex[1], K".", ex[2])
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
    elseif k == K"block"
        emit(ctx, K"begin", K"WS_NL")
        format_join(ctx, children(ex), K"WS_NL")
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"quote"
        if kind(ex[1]) == K"block"
            emit(ctx, K"quote", K"WS_NL")
            format_join(ctx, children(ex[1]), K"WS_NL")
            emit(ctx, K"WS_NL", K"end")
        else
            emit(ctx, K":", K"(", K"WS??", ex[1], K"WS??", K")")
        end
    elseif k == K"call"
        if is_infix_op_call(ex)
            # TODO: Precedence - add parens
            emit(ctx, ex[1], K"WS?", ex[2], K"WS?", ex[3])
        elseif is_postfix_op_call(ex)
            # TODO: Precedence - make parens optional
            emit(K"(", ex[1], K")", ex[2])
        else
            emit(ctx, ex[1], K"(", K"WS??")
            format_join(ctx, ex[2:end], K"WS??", K",", K"WS?")
            emit(ctx, K"WS??", K")")
        end
    elseif k == K"function"
        emit(ctx, K"function", K"WS+", ex[1])
        if numchildren(ex[2]) != 0
            emit(ctx, K"WS_NL")
            format_join(ctx, children(ex[2]), K"WS_NL")
        end
        emit(ctx, K"WS_NL", K"end")
    elseif k == K"local" || k == K"global"
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
    elseif k == K"string"
        emit(ctx, K"\"")
        for (i,c) in enumerate(children(ex))
            if kind(c) == K"String"
                emit(ctx, FormatToken(K"String", c.value, false))
            else
                if kind(c) == K"Identifier" && (i == numchildren(ex) || (kind(ex[i+1]) == K"String" && !JuliaSyntax.Tokenize.is_identifier_char(first(ex[i+1].value))))
                    emit(ctx, K"$", c)
                else
                    emit(ctx, K"$", K"(", c, K")")
                end
            end
        end
        emit(ctx, K"\"")
    elseif k == K"tuple"
        emit(ctx, K"(", K"WS??")
        format_join(ctx, children(ex), K",", K"WS?")
        if numchildren(ex) == 1
            emit(ctx, K"WS??", K",")
        end
        emit(ctx, K"WS??", K")")
    # Lowering stuff
    elseif k == K"lambda"
        emit(ctx, K"(")
        format_join(ctx, ex.lambda_info.args, K",", K"WS?")
        emit(ctx, K")", K"->")
        format_tree(ctx, ex[1])
    else
        emit(ctx, FormatToken(kind(ex), string("---",kind(ex),"---"), true), K"WS_NL")
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
        if tok.kind == K"WS_NL"
            ctx.tokens[i] = FormatToken(tok.kind, "\n"*"    "^indent[i], true, nothing)
        end
    end
end

function reformat(ex)
    ctx = FormatContext{String}()
    format_tree(ctx, ex)
    format_indents(ctx)
    sourcetext(ctx)
end

function format_token_str_default(ex::SyntaxTree; include_var_id=false)
    @assert !haschildren(ex)
    # See also JuliaLowering._value_string
    k = kind(ex)
    str = k == K"Identifier" || k == K"MacroName" || is_operator(k) ? ex.name_val :
          k == K"SSAValue"   ? "ssa"                 :
          k == K"core"       ? "core.$(ex.name_val)" :
          k == K"top"        ? "top.$(ex.name_val)"  :
          k == K"Symbol"     ? ":$(ex.name_val)" :
          k == K"globalref"  ? "$(ex.mod).$(ex.name_val)" :
          k == K"slot"       ? "slot" :
          repr(get(ex, :value, nothing))
    id = get(ex, :var_id, nothing)
    if !isnothing(id) && (k == K"SSAValue" || include_var_id)
        idstr = replace(string(id),
                        "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
                        "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
        str = "$(str).$idstr"
    end
    if k == K"slot"
        srcex = SyntaxTree(ex.graph, ex.source)
        str = "$(str)/$(srcex.name_val)"
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
    format(ex; include_var_id, format_token_str, format_token_style)

Format syntax tree `ex` as Julia source code. `include_var_id` includes the
`var_id` attribute in the names of identifiers, when present.
`format_token_str` and `format_token_style` are functions which extract the
string representation of a leaf of the tree (a token in the source), and the
"style" of a token, respectively. The style is currently any value; unique
style values will be formatted as random but distinguishable colors when
printing.
"""
function format(ex::SyntaxTree;
                include_var_id=false,
                format_token_str=(ex)->format_token_str_default(ex; include_var_id),
                format_token_style=(ex)->nothing)
    ctx = FormatContext(ex->(format_token_str(ex), format_token_style(ex)))
    JuliaSyntaxFormatter.format_tree(ctx, ex)
    JuliaSyntaxFormatter.format_indents(ctx)

    text = Base.AnnotatedString(sourcetext(ctx))
    idx = 1
    styles = unique(tok.style for tok in ctx.tokens)
    style_map = Dict(zip(styles, distinguishable_faces(length(styles))))
    for tok in ctx.tokens
        n = sizeof(tok.text)
        if !isnothing(tok.style)
            Base.annotate!(text, idx:idx+n-1, :face=>style_map[tok.style])
        end
        idx += n
    end
    text
end

function __init__()
    _insert_kinds()
end

end
