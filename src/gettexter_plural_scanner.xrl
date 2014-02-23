% nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;

Definitions.

Rules.

% keys
nplurals : {token, {'nplurals', TokenLine}}.
n : {token, {'n', TokenLine}}.
plural : {token, {'plural', TokenLine}}.

% values
[0-9]+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
\s+ : skip_token.
\; : {token, {';', TokenLine}}.

% ternary
\? : {token, {'?', TokenLine}}.
\: : {token, {':', TokenLine}}.

% math
\% : {token, {'%', TokenLine}}.
\== : {token, {'==', TokenLine}}.
\!= : {token, {'!=', TokenLine}}.
\! : {token, {'!', TokenLine}}.
\= : {token, {'=', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.

% logical
\|\| : {token, {'||', TokenLine}}.
&& : {token, {'&&', TokenLine}}.
\>= : {token, {'>=', TokenLine}}.
\<= : {token, {'<=', TokenLine}}.
\> : {token, {'>', TokenLine}}.
\< : {token, {'<', TokenLine}}.

% parens
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.

Erlang code.
