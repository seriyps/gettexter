%% For documentation see gettexter.hrl.

%% ?NO_(Text) - noop

%% Implicit locale

%%% Implicit domain, no default
-ifndef(GETTEXT_DOMAIN).

    -define(GETTEXT_DOMAIN, undefined).

    -define(_(Text), gettexter:gettext(Text)).

    -define(N_(Singular, Plural, N), gettexter:ngettext(Singular, Plural, N)).

    -define(P_(Context, Text), gettexter:pgettext(Context, Text)).

    -define(NP_(Context, Singular, Plural, N),
                gettexter:npgettext(Context, Singular, Plural, N)).
%%% Implicit domain, default set
-else.
    -define(_(Text), ?D_(?GETTEXT_DOMAIN, Text)).
    -define(N_(Singular, Plural, N),
                ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N)).
    -define(P_(Context, Text), ?DP_(?GETTEXT_DOMAIN, Context, Text)).
    -define(NP_(Context, Singular, Plural, N),
                ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N)).
-endif.

%%% Explicit domain
-define(D_(Domain, Text), gettexter:dgettext(Domain, Text)).
-define(DN_(Domain, Singular, Plural, N),
            gettexter:dngettext(Domain, Singular, Plural, N)).
-define(DP_(Domain, Context, Text),
            gettexter:dpgettext(Domain, Context, Text)).
-define(DNP_(Domain, Context, Singular, Plural, N),
            gettexter:dnpgettext(Domain, Context, Singular, Plural, N)).

%% Explicit locale

%%% Implicit domain
-define(_(Text, Locale), ?D_(?GETTEXT_DOMAIN, Text, Locale)).
-define(N_(Singular, Plural, N, Locale),
        ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N, Locale)).
-define(P_(Context, Text, Locale), ?DP_(?GETTEXT_DOMAIN, Context, Text, Locale)).
-define(NP_(Context, Singular, Plural, N, Locale),
        ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N, Locale)).

%%% Explicit domain
-define(D_(Domain, Text, Locale),
            gettexter:dgettext(Domain, Text, Locale)).
-define(DN_(Domain, Singular, Plural, N, Locale),
            gettexter:dngettext(Domain, Singular, Plural, N, Locale)).
-define(DP_(Domain, Context, Text, Locale),
            gettexter:dpgettext(Domain, Context, Text, Locale)).
-define(DNP_(Domain, Context, Singular, Plural, N, Locale),
            gettexter:dnpgettext(Domain, Context, Singular, Plural, N, Locale)).

%% gettext noop
-define(NO_(Text), Text).
