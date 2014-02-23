% nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;

Nonterminals
    plural_rule
    math_expr
    ternary_op
    operand
    bin_expr
    un_expr
.

Terminals
    nplurals
    n
    plural
    integer '=' ';' '?' ':' '%' '!' '==' '!=' '+' '-' '*' '/'
    '||' '&&' '>=' '<=' '>' '<' '(' ')'
.

Rootsymbol plural_rule.

Unary 800 '!'.
Left 700 '*' '/' '%'.
Left 600 '+' '-'.
Left 500 '<' '<=' '>' '>='.
Left 400 '==' '!='.
Left 300 '&&'.
Left 200  '||'.
Right 100 ternary_op.


plural_rule -> nplurals '=' integer ';' plural '=' math_expr ';' : {plural_rule, value('$3'), '$7'}.

math_expr -> ternary_op : '$1'.
math_expr -> bin_expr : '$1'.
math_expr -> '(' math_expr ')' : '$2'.
math_expr -> un_expr : '$1'.
math_expr -> operand : '$1'.

ternary_op -> math_expr '?' math_expr ':' math_expr : {'if', '$1', '$3', '$5'}.

operand -> n : n.
operand -> integer : value('$1').

bin_expr -> math_expr '%' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '==' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '!=' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '+' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '-' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '/' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '*' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '||' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '&&' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '>=' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '<=' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '>' math_expr : {value('$2'), '$1', '$3'}.
bin_expr -> math_expr '<' math_expr : {value('$2'), '$1', '$3'}.

un_expr -> '!' math_expr : {value('$1'), '$2'}.

Erlang code.
value({Token, _Line}) ->
    Token;
value({_Token, _Line, Value}) ->
    Value.
