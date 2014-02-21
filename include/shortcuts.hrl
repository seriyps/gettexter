-ifndef(GETTEXT_DOMAIN).
  %% regular gettext call
  -define(_(String), gettexter:gettext(String)).

  %% ngettext - plural
  -define(N_(Singular, Plural, N), gettexter:ngettext(Singular, Plural, N)).

  %% pgettext (with msgctx)
  -define(P_(Context, String), gettexter:pgettext(Context, String)).
  -define(NP_(Context, Singular, Plural, N), gettexter:npgettext(Context, Singular, Plural, N)).
-else.
  -define(_(String), ?D_(?GETTEXT_DOMAIN, String)).
  -define(N_(Singular, Plural, N), ?DN_(?GETTEXT_DOMAIN, Singular, Plural, N)).
  -define(P_(Context, String), ?DP_(?GETTEXT_DOMAIN, Context, String)).
  -define(NP_(Context, Singular, Plural, N), ?DNP_(?GETTEXT_DOMAIN, Context, Singular, Plural, N)).
-endif.

%% d*gettext - all the same, but with domain specified (should be used for library localization)
-define(D_(Domain, String), gettexter:dgettext(Domain, String)).
-define(DN_(Domain, Singular, Plural, N), gettexter:dngettext(Domain, Singular, Plural, N)).
-define(DP_(Domain, Context, String), gettexter:dpgettext(Domain, Context, String)).
-define(DNP_(Domain, Context, Singular, Plural, N), gettexter:dnpgettext(Domain, Context, Singular, Plural, N)).

%% gettext noop
-define(NO_(String), String).
