using JuliaSyntaxFormatter
using Test
using JuliaSyntax, JuliaLowering

@testset "JuliaSyntaxFormatter.jl" begin

# Indentation is represented here as `~~` for ease of readability
stmt_tests = [
    # strings
    "\"hi\"" => "\"hi\"" 
    "\"h\\ni\"" => "\"h\\ni\"" 
    "\"h\\\"i\"" => "\"h\\\"i\"" 
    "\"h\\\$i\"" => "\"h\\\$i\"" 
    # block
    "begin end" => "begin\nend"
    "begin\nx\nend" => "begin\n~~x\nend"
    "begin\nx\ny\nend" => "begin\n~~x\n~~y\nend"
    # call
    "f()" => "f()"
    "f(x)" => "f(x)"
    "f(x,y)" => "f(x, y)"
    "f()()" => "f()()"
    # infix call
    "a + b" => "(a + b)"
    "a + b*c" => "(a + (b * c))"
    "(a + b)*c" => "((a + b) * c)"
    "a + b + c" => "(a + b + c)"
    "a ++ b ++ c" => "(a ++ b ++ c)"
    "a * b * c" => "(a * b * c)"
    "a / b / c" => "((a / b) / c)"
    "a - b - c" => "((a - b) - c)"
    # postfix call
    "a'" => "(a)'"
    "(a+b)'" => "((a + b))'"
    # do syntax
    "f(x, y) do a, b\nbody\nend" => "f(x, y) do a, b\n~~body\nend"
    "@f(x, y) do a, b\nbody\nend" => "@f(x, y) do a, b\n~~body\nend"
    # macrocall
    "@mac" => "@mac"
    "@mac a b c" => "@mac a b c"
    "strmac\"blah\"" => "strmac\"blah\""
    "strmac\"blah\"suffix" => "strmac\"blah\"suffix"
    "strmac\"blah\"1" => "strmac\"blah\"1"
    "strmac\"\\\\\"" => "strmac\"\\\\\""
    "strmac\"\\ \"" => "strmac\"\\ \""
    # for
    "for x in xs\nend" => "for x in xs\nend"
    "for x in xs\na\nend" => "for x in xs\n~~a\nend"
    "for x in xs\na\nb\nend" => "for x in xs\n~~a\n~~b\nend"
    "for x in xs, y in ys\na\nend" => "for x in xs, y in ys\n~~a\nend"
    # function
    "function f() end" => "function f()\nend"
    "function f()\nx\nend" => "function f()\n~~x\nend"
    # while
    "while cond\nend" => "while cond\nend"
    "while cond\na\nend" => "while cond\n~~a\nend"
    "while cond\na\nb\nend" => "while cond\n~~a\n~~b\nend"
    # let
    "let\nend" => "let\nend"
    "let x=1\nend" => "let x = 1\nend"
    "let x=1,y=2\nend" => "let x = 1, y = 2\nend"
    "let\nbody\nend" => "let\n~~body\nend"
    "let x=1\nbody\nend" => "let x = 1\n~~body\nend"
    # struct
    "struct X\nend" => "struct X\nend"
    "struct X\na\nend" => "struct X\n~~a\nend"
    "struct X\na\nb\nend" => "struct X\n~~a\n~~b\nend"
    # if else
    "if cond\nend" => "if cond\nend"
    "if cond\nx\nend" => "if cond\n~~x\nend"
    "if cond\nx\nelseif cond2\nend" => "if cond\n~~x\nelseif cond2\nend"
    "if cond\nx\nelseif cond2\ny\nend" => "if cond\n~~x\nelseif cond2\n~~y\nend"
    "if cond\nx\nelseif cond2\ny\nelse\nend" => "if cond\n~~x\nelseif cond2\n~~y\nelse\nend"
    "if cond\nx\nelseif cond2\ny\nelse\nz\nend" => "if cond\n~~x\nelseif cond2\n~~y\nelse\n~~z\nend"
    # tuple
    "()" => "()"
    "(x,)" => "(x,)"
    "(x, y)" => "(x, y)"
    "(; x)" => "(; x)"
    "(x,y; a)" => "(x, y; a)"
    "(x,y; a,b)" => "(x, y; a, b)"
    "(x,y; a,b; s)" => "(x, y; a, b; s)"
    # ref
    "a[i, j]" => "a[i, j]"
    # vect
    "[x,]" => "[x]"
    "[x, y]" => "[x, y]"
    # concatenation
    "[x y z]" => "[x y z]"
    "[x;y;z]" => "[\n~~x\n~~y\n~~z\n]"
    "[x y;z]" => "[\n~~x y\n~~z\n]"
    # braces
    "{x,}" => "{x}"
    "{x, y}" => "{x, y}"
    # quote
    "quote\nend" => "quote\nend"
    "quote\nbody\nend" => "quote\n~~body\nend"
    ":(x+y)" => ":((x + y))"
    ":(x)" => ":(x)"
]

@testset "$(repr(input))" for (input,ref_output) in stmt_tests
    ast = parsestmt(JuliaLowering.SyntaxTree, input)
    @test JuliaSyntaxFormatter.formatsrc(ast, indent_str="~~") == ref_output
end

# Indentation is represented here as `~~` for ease of readability
toplevel_tests = [
    # doc
    "\"docstr\"\nthing_to_doc" => "---toplevel---\n~~\"docstr\"\n~~thing_to_doc\nend"
]

@testset "$(repr(input))" for (input,ref_output) in toplevel_tests
    ast = parseall(JuliaLowering.SyntaxTree, input)
    @test JuliaSyntaxFormatter.formatsrc(ast, indent_str="~~") == ref_output
end

end
