%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2014, Sergey
%%% @doc
%%% Gettext plural form rule compiler and evaluator.
%%% http://www.gnu.org/software/gettext/manual/html_node/Plural-forms.html
%%% <pre><code>
%%% Compiled = gettexter_plural:compile("nplurals=3; plural=...;"),
%%% % ...store `Compiled' somwhere and reuse...
%%% 3 = gettexter_plural:nplurals(Compiled),
%%% PluralFormNum = gettexter_plural:compile(N, Compiled),
%%% </code></pre>
%%%
%%% The most hard part here, is that in C true==1 and false==0.
%%% So, runtime dependency used for type casting (see runtime_mod option).
%%% You may use default value, which is absolutely ok, if runtime dependency
%%% from 'gettexter' is ok for you. But if you want to embed generated code
%%% in your own generated code, take a look at `to_erlang_ast/2' and `runtime_mod'
%%% option.
%%% @end
%%% Created : 24 Feb 2014 by Sergey <me@seriyps.ru>

-module(gettexter_plural).
%% API
-export([compile/2, nplurals/1, plural/2]).
%% Embedding
-export([to_erlang_ast/2, to_erlang_abstract/2, to_erlang_code/2]).
%% Runtime dependency
-export([to_boolean/1, to_integer/1]).

-type operand() :: non_neg_integer() | n.
-type un_expr() :: {atom(), math_expr()}.
-type bin_expr() :: {atom(), math_expr(), math_expr()}.
-type if_expr() :: {'if', math_expr(), math_expr(), math_expr()}.
-type math_expr() :: if_expr() | bin_expr() | un_expr() | operand().
-type plural_rule() :: {plural_rule, integer(), math_expr()}. %result, returned by parser

-type opts() :: [{runtime_mod, atom()}  %module used for runtime calls 'to_boolean/1' and 'to_integer/1'
                ].
-record(plural_rule_compiled, {nplurals :: non_neg_integer(), %result, returned by compile/2
                               abstract_form :: erl_parse:abstract_form()}).

-record(opts, {runtime_mod = ?MODULE}).
-define(ES, erl_syntax).

%% @doc
%% Return number of plural forms for language. Value of 'nplurals' from
%% plural rule.
nplurals(#plural_rule_compiled{nplurals=NPlurals}) ->
    NPlurals.

%% @doc
%% Calculate plural form number, using (compiled) plural form expression.
%% Usage:
%% <pre><code>
%% CompiledExpr = compile("nplurals=3; plural=...;", []),
%% % Compiled expr may be stored somewhere and reused
%% PluralFormNum = plural(N, CompiledExpr).
%% </code></pre>
plural(N, #plural_rule_compiled{nplurals=NPlurals, abstract_form=Tree}) when is_integer(N) ->
    {value, PluralFormN, _} = erl_eval:expr(Tree, [{'N', N}]),
    true = (PluralFormN =< NPlurals),           %assertion
    PluralFormN;
plural(N, StringExpr) when is_list(StringExpr) andalso is_integer(N) ->
    error_logger:warning_msg(
      "~p:~p(~p, \"~s\") called! This is very slow and should be used only for debugging! "
      "In production 2'nd argument should be return value of ~p:~p/2",
      [?MODULE, plural, N, StringExpr, ?MODULE, compile]),
    plural(N, compile(StringExpr, [])).

%% @doc
%% Compile plural expression like
%% <code>nplurals=3; plural=...;</code>
%% to internal representation, used for fast expression evaluation.
-spec compile(string(), opts()) -> #plural_rule_compiled{}.
compile(StringExpr, Opts) ->
    {ok, Tokens, _} = gettexter_plural_scanner:string(StringExpr),
    {ok, {plural_rule, NPlurals, _}=Tree} = gettexter_plural_parser:parse(Tokens),
    Abstract = to_erlang_abstract(Tree, Opts),
    #plural_rule_compiled{
       nplurals=NPlurals,
       abstract_form=Abstract}.


%% @doc
%% Converts output of `gettexter_plural_parser' to `erl_syntax' AST.
%% May be used for embedding to generated code.
%% May be wrapped to module AST and compiled to .beam file.
%% Note: generated code uses one free variable: 'N', and calls runtime dependencies:
%% `runtime_mod:to_boolean/1' and `runtime_mod:to_integer/1'.
%% `runtime_mod' is `?MODULE' by default.
-spec to_erlang_ast(plural_rule(), opts()) -> erl_syntax:syntaxTree().
to_erlang_ast({plural_rule, _, Expr}, Opts) ->
    O = #opts{},
    O1 = #opts{
            runtime_mod=proplists:get_value(runtime_mod, Opts, O#opts.runtime_mod)
           },
    ?ES:application(
       ?ES:atom(O1#opts.runtime_mod),
       ?ES:atom(to_integer),
       [to_erl_math_expr(Expr, O1)]).

%% @doc
%% Converts output of `gettexter_plural_parser' to `erl_parse' abstract form.
%% You, most probably, shouldn't call this from your programs.
%% Return value may be interpreted by `erl_eval:expr/2' (see `plural/2').
%% May be printed as erlang code snippet by `erl_prettypr` (see `to_erlang_code/2`).
-spec to_erlang_abstract(plural_rule(), opts()) -> erl_parse:abstract_form().
to_erlang_abstract({plural_rule, _, _}=Rule, Opts) ->
    erl_syntax:revert(to_erlang_ast(Rule, Opts)).

%% @doc
%% For debugging - return human-readable generated Erlang code.
to_erlang_code({plural_rule, _, _}=Rule, Opts) ->
    erl_prettypr:format(to_erlang_abstract(Rule, Opts)).

%%
%% internal
%%

%% expression runtime
to_boolean(true) -> true;
to_boolean(false) -> false;
to_boolean(N) when N > 0 -> to_boolean(true);
to_boolean(N) when N == 0 -> to_boolean(false).

to_integer(true) -> to_integer(1);
to_integer(false) -> to_integer(0);
to_integer(N) when is_integer(N) -> N.

%% converter from math_expr() to erl_syntax:syntaxTree()
to_erl_math_expr({'if', Cond, Then, Else}, Opts) ->
    %% Cond ? Then : Else
    %% to
    %% case to_boolean(Cond) of true -> Then; false -> Else end.
    ?ES:case_expr(
       ?ES:application(
          ?ES:atom(Opts#opts.runtime_mod),
          ?ES:atom('to_boolean'),
          [to_erl_math_expr(Cond, Opts)]),
       [?ES:clause(
           [?ES:atom(true)],
           none,
           [to_erl_math_expr(Then, Opts)]),
        ?ES:clause(
           [?ES:atom(false)],
           none,
           [to_erl_math_expr(Else, Opts)])]
      );
to_erl_math_expr({'!', Expr}, Opts) ->
    %% !(Expr)
    %% to
    %% erlang:not(to_boolean(Expr))
    ?ES:application(
       ?ES:atom(erlang),
       ?ES:atom('not'),
       [?ES:application(
          ?ES:atom(Opts#opts.runtime_mod),
          ?ES:atom(to_boolean),
          [to_erl_math_expr(Expr, Opts)])]
     );
to_erl_math_expr(n, _) ->
    ?ES:variable("N");
to_erl_math_expr(Integer, _) when is_integer(Integer) andalso (Integer >= 0) ->
    ?ES:integer(Integer);
to_erl_math_expr({Op, L, R}, Opts) when is_atom(Op) ->
    {InfOp, ArgType} = to_erl_bin_expr(Op),
    Convert = fun(A) ->
                      %% TODO: optimization - don't apply to constants, like
                      %% to_integer(5) or to_integer(N)
                      ?ES:application(
                         ?ES:atom(Opts#opts.runtime_mod),
                         ?ES:atom(ArgType),
                         [to_erl_math_expr(A, Opts)])
              end,
    ?ES:infix_expr(
       Convert(L),
       erl_syntax:operator(InfOp),
       Convert(R)
      ).

to_erl_bin_expr('||') -> {'orelse', to_boolean} ;
to_erl_bin_expr('&&') -> {'andalso', to_boolean} ;
to_erl_bin_expr('==') -> {'==', to_integer};
to_erl_bin_expr('!=') -> {'=/=', to_integer} ;
to_erl_bin_expr('%') -> {'rem', to_integer};
to_erl_bin_expr('+') -> {'+', to_integer};
to_erl_bin_expr('-') -> {'-', to_integer};
to_erl_bin_expr('/') -> {'/', to_integer};
to_erl_bin_expr('*') -> {'*', to_integer};
to_erl_bin_expr('>=') -> {'>=', to_integer};
to_erl_bin_expr('<=') -> {'=<', to_integer};
to_erl_bin_expr('>') -> {'>', to_integer};
to_erl_bin_expr('<') -> {'<', to_integer}.
