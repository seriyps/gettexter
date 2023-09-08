gettexter - gettext for erlang
==============================

Goals of this project is:

* To be more or less compatible with GNU gettext (API and tools).
* To be as fast as possible in concurrent environment.

You may use this app to translate libraries as well as your own business apps
and use them in single installation with no conflicts.
Libraries may ship their own translations inside their distribution.

Quick Start
-----------

You definitely should be familiar with [GNU gettext](http://www.gnu.org/software/gettext/manual/gettext.html)
before start using this library.

In Erlang code include `shortcuts.hrl` header file.

```erlang
-include_lib("gettexter/include/shortcuts.hrl").
```

Mark your translatable strings with:

* `?_("")` - `gettext` (regular)
* `?N_("", "", N)` - `ngettext` (plural)
* `?P_(Ctx, "")` - `pgettext` (respecting msgctx)
* `?NP_(Ctx, "", "", N)` - `npgettext` (ngettext + pgettext)
* `?D_(Domain, "")` - `dgettext` (domain/namespace lookups, plus `?DN_`, `?DP_`, `?DNP_`)
* `?NO_("")` - gettext noop


```erlang
% file: my_app/src/my_module.erl
-module(my_module).
-define(GETTEXT_DOMAIN, my_app).
-include_lib("gettexter/include/shortcuts.hrl").

main(Name, What, N) ->
    gettexter:bindtextdomain(?GETTEXT_DOMAIN, "/../priv/locales"), % from where load locales
    gettexter:textdomain(?GETTEXT_DOMAIN), % domain for current process
    gettexter:setlocale(lc_messages, "en"),  % locale for current process

    Question = case What of
                 sleep -> ?NO_("Wanna sleep?");
                 eat -> ?NO_("Wanna eat?")
               end,
    Time = io_lib:format(?N_("It's ~p hour", "It's ~p hours", N), [N]),
    %% /* Translators: this is hello message */
    io:format(?_("Hello, ~p! ~ts. ~ts"), [Name, Time, ?_(Question)]).
```

Extract messages from sources to .pot file by `xgettext` command. Take note
that this only work for string literals and not binaries.

```bash
export APP=my_app

xgettext -o priv/locale/${APP}.pot --package-name=${APP} -d ${APP} --sort-by-file -L C \
    --keyword='NO_' --keyword='_' --keyword='N_:1,2' \
    --keyword='P_:1c,2' --keyword='NP_:1c,2,3' \
    --keyword='D_:2' --keyword='DN_:2,3' --keyword='DP_:2c,3' --keyword='DNP_:2c,3,4' \
    --add-comments=Translators \
    src/my_module.erl src/*.erl
```

Initialize new locale's .po file by `msginit`

```bash
mkdir -p priv/locale/ru/LC_MESSAGES/
msginit -i priv/locale/${APP}.pot -o priv/locale/ru/LC_MESSAGES/${APP}.po --locale=ru
```

Or actualize existing locale's .po file by `msgmerge`

```bash
msgmerge -U priv/locale/ru/LC_MESSAGES/${APP}.po priv/locale/${APP}.pot
```

When translations are finished, generate locale's binary .mo files by `msgfmt`

```bash
msgfmt --check -o priv/locale/ru/LC_MESSAGES/${APP}.mo priv/locale/ru/LC_MESSAGES/${APP}.po
```
It's **strongly recommended** to not add .mo files to your repository! So, add
`*.mo` to .gitignore / .hgignore and generate them in compile-time (by rebar
post-compile hook or so).

API
---

Api tries to be compatible with [GNU gettext API](http://www.gnu.org/software/gettext/manual/gettext.html#gettext).
If you find some discrepancy (not explicitly documented) - please report.

### Gettext lookups
All lookup functions are able to take both binaries or strings. They will
return what is given to them. Mixed textual types is not supported. 

Each lookup function and macros has it's arity + 1 companion, which accept
explicit locale as last argument. So, `gettexter:gettext(text())` has
`gettexter:gettext(text(), locale())`, `?_(text())` has `?_(text(), locale())`
and so on.

For more information see the documentation.

```erlang
gettexter:gettext(text()) -> text().  % '?_' macro
```
Main gettext call. Uses locale, activated by `setlocale/2`.

```erlang
gettexter:ngettext(Singular :: text(), Plural :: text(),
                   Count :: integer()) -> text().  % '?N_' macro
```
Plural gettext call.

```erlang
gettexter:pgettext(Context :: text(), text()) -> text().  % '?P_'
gettexter:pngettext(Context :: text(), text(), text(),
                    integer()) -> text().  % '?PN_'
```
Gettext calls with respect to `msgctx` ('p' means 'particular').

```erlang
gettexter:dgettext(Domain :: atom(), text()) -> text().  % '?D_'
% and other 'gettexter:d{n,p,pn}gettext' plus '?D*_' macroses
```
Gettext calls, which will search in a specific domain (namespace).
(See `bindtextdomain`).

### Gettext configuration

```erlang
gettexter:bindtextdomain(Domain :: atom(), LocaleDir :: file:filename()) -> ok.
```
Setup directory from which .mo files will be loaded like
`${LocaleDir}/${Locale}/LC_MESSAGES/${Domain}.mo`.
Domain **MUST** be specified for library applications (see `dgettext`).

By default, `Domain` is `application:get_application()` and `LocaleDir` is
`filename:absname(filename:join(code:priv_dir(Domain), "locale"))`.
If `LocaleDir` is relative, absolute path will be calculated, unlike original
`gettext` does (relative to cwd), but relative to `code:priv_dir(Domain)`.

This function don't reload already loaded locales for `Domain`, so, should be called
before `setlocale` or `ensure_loaded` calls.

This function usualy called only once at application initialization/configuration phase.

```erlang
gettexter:setlocale(lc_messages, Locale :: text()) -> ok.
gettexter:getlocale(lc_messages) -> text() | undefined.
```
Get / Set default locale for **current process**. It also loads .mo files for
`Locale` from the `LocaleDir`s for each `Domain` (if not loaded yet).
1'st argumet currently has only one value - `lc_messages` atom.
*Note: `getlocale` is not standard GNU gettext function.*

```erlang
gettexter:textdomain() -> atom() | undefined.
gettexter:textdomain(Domain :: atom()) -> ok.
```
Get/Set default domain (namespace) for **current process**.
If you call it like `textdomain(my_app)`, then `gettext(Key)` calls will be
converted to `dgettext(my_app, Key)`.
XXX: it's highly preferable to use `dgettext` directly and don't use this
API function, if localized strings can be rendered in different processes.

```erlang
gettexter:bind_textdomain_codeset(Domain :: atom(), Charset :: string()) -> ok.
```
Set encoding, on which all `gettexter:*gettext` responses should be converted.
This add significant performance overhead, and require 'iconv' dependency, if
.po file's `Content-Type` and `Charset` isn't the same.

XXX: Note, that `gettexter:gettext(Key)` call will be finally converted to
`gettexter:dpgettext(undefined, undefined, Key)`, which will try to extract
`Domain` and `Locale` from current process dictionary.

XXX: When developing library, you may want to re-define `?_`, `?N_`, `?P_` macroses
to call `dgettext` and use your module's domain by default.
You can do this by following trick:

```erlang
-define(GETTEXT_DOMAIN, my_domain).
-include_lib("gettexter/include/shortcuts.hrl").
```
This will modify macroses (except `?_D*`) to use `d*gettext(my_domain, ...)` by default.

### Proprietary APIs

This apis has no GNU gettext equiualents, but may be useful in Erlang apps.

```erlang
gettexter:which_domains(Locale) -> [atom()].
```
Which domains are loaded from .mo files to gettext server for `Locale`.

```erlang
gettexter:which_locales(Domain) -> [locale()].
```
Which locales are loaded from .mo files to gettext server for `Domain`.

```erlang
gettexter:ensure_loaded(TextDomain, lc_messages, Locale) ->
    {ok, already} | {ok, MoFile :: file:filename()} | {error, term()}.
```
Ensure, that locale is loaded from .mo file to gettexter server. If locale
isn't loaded, all `gettext` lookups to it will return default value `Msgid`.
This function may be called at application start-up or configuration time,
once for each supported locale.
In case of error, all data for this combination of `Domain` and `Locale` will
be removed from gettexter server to avoid incomplete/broken data.

```erlang
reload(Domain) -> list().

reload(Domain, [locale()]) -> [{locale(), ok, file:filename()} |
                               {locale(), error, any()}].
```
Re-reads the phrases from `.mo` files bound by `bindtextdomain/2` (eg, if those files changed).
Function `reload/1` reloads ALL the locales of the domain `Domain`.

```erlang
gettexter:reset() -> ok.
```
Remove all gettext stuff, added by `setlocale` or `textdomain`, from process
dictionary (but not from locale data storage). May be used to reuse process
for the next client.

Glossary
--------

* Domain: namespace for translations. In practice, the name of .po/.mo file, which
          in most cases, named as your OTP application.
* Locale: not strictly speaking, just name of translation's language, like "en",
          "en_GB", "ru", "pt_BR", etc. Usualy Locale contains also rules of
          plural form calculation, date/time/number formatting, currency etc.
* LC_MESSAGES: locale category, which contains translated application's strings (in
          .mo/.po format).

Use with ErlyDTL
----------------

[Erlydtl](http://github.com/erlydtl/erlydtl) >= 0.9.4 supports plural forms and contexts.
So, you can use all gettexter features with it. To enable translation features of
erlydtl, you should wrap all translatable strings in `{%trans%}`, `{%blocktrans%}` and `_()`,
generate .po and .mo files and then pass `translation_fun` and `locales` in compile-time, or
`translation_fun` and `locale` in render-time.

Example `translation_fun`:

```erlang
TransFun = fun({Str, {StrPlural, N}}, {Locale, Ctx}) ->
               gettexter:pngettext(Ctx, Str, StrPlural, N, Locale);
              ({Str, {StrPlural, N}}, Locale) ->
               gettexter:ngettext(Str, StrPlural, N, Locale);
              (Str, {Locale, Ctx}) ->
               gettexter:pgettext(Ctx, Str, Locale);
              (Str, Locale) ->
               gettexter:gettext(Str, Locale)
           end.
```

Template:
```django
{# file: src/tpl.dtl #}
{%trans "Hello"%}
{%trans "Hello" context "ctx"%}
{% blocktrans count cnt=cnt %}Apple{%plural%}Apples{%endblocktrans%}
```
.po file
```po
#file: priv/locale/ru/LC_MESSAGES/test_app.po + compiled .mo
msgid ""
msgstr ""
"Content-Type: text/plain; charset=utf-8\n"
"Plural-Forms: nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n"
"%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;\n"

msgid "Hello"
msgstr "Привет"

msgctxt "ctx"
msgid "Hello"
msgstr "Контекстный привет"

msgid "Apple"
msgid_plural "Apples"
msgstr[0] "Яблоко"
msgstr[1] "Яблока"
msgstr[2] "Яблок"
```
Code:
```erlang
1> application:start(gettexter).
2> Domain = test_app.
3> % next step may be skipped, if `code:priv_dir(Domain)` is ok (if Domain is appname)
3> gettexter:bindtextdomain(Domain, </path/to/app/priv> ++ "/locale").
4> gettexter:textdomain(Domain).
5> erlydtl:compile_file("src/tpl.dtl", t).
6> TransFun = '....'. % see above
7> {ok, R} = t:render(),  io:format("~ts~n", [iolist_to_binary(R)]).
Hello
Hello

Apple
8> {ok, R1} = t:render([{cnt, 2}], [{locale, "ru"}, {translation_fun, TransFun}]).
9> io:format("~ts~n", [iolist_to_binary(R1)]).
Привет
Контекстный привет

Яблока

```

TODO
----

### Binary keys extraction

Expression, `?_(<<"...">>)` is not well handled by `xgettext`, so, variants:

* Write own extractor like xgettext, but for erlang code. Also, `.pot` serializer
  will be needed then.

* Send patches to GNU gettext.

### Custom locale loaders

Maybe allow `bindtextdomain/2` 2'nd argument be a `{M, F, A}` or `fun M:F/N` to
allow locale loading customization?

### Generate .beam module with compiled-in locales for extra-fast access.

Since ETS lookups require heap copying, smth like static .beam module with
compiled-in phrases and plural rules (!!!) may  be generated.
Pros: extra fast access speed; no memory copying; compiled plural rules.
Cons: slow update; hackish.
