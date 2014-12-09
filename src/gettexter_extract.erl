-module(gettexter_extract).

-export([extract/0, extract/2, extract_to_ets/2, parse_transform/2]).

-define(DEFAULT_OUT_DIR, ".").
-define(DEFAULT_VERBOSE, false).
-define(DEFAULT_COMMENT_TAG, "Translators:").

%% @doc Extract strings from command-line
%%
%% Options:
%%     -o OutDir     - set output directory
%%     -c CommentTag - set the comment tag
%%     -v            - verbose
%%     -f Files      - a list of files to extract from
%%
%% Example:
%% - Include gettexter as a dependency to your application.
%% - Make sure the dependency directory is reachable by erl (set ERL_LIBS=deps)
%% - Run the command:
%%       erl -run gettexter_extract extract -- -o priv/pots
%% - This will extract all strings from the files in the src/ directory into
%%   priv/pots.
extract() ->
    OutDir = case init:get_argument(o) of
                 error     -> ?DEFAULT_OUT_DIR;
                 {ok, [[Dir]]} -> Dir
             end,

    CommentTag = case init:get_argument(c) of
                     error     -> ?DEFAULT_COMMENT_TAG;
                     {ok, [[Tag]]} -> Tag
                 end,
    Verbose = case init:get_argument(v) of
                  error      -> ?DEFAULT_VERBOSE;
                  {ok, [[]]} -> true
              end,
    Files = case init:get_argument(f) of
                %% If no files are specified we try to extract all erl-files
                %% in the src/ directory
                error               -> filelib:wildcard("src/*.erl");
                {ok, [SourceFiles]} -> SourceFiles
            end,
    Options = [{out_dir, OutDir},
               {comment_tag, CommentTag},
               {verbose, Verbose}],
    extract(Files, Options),
    init:stop().

%% @doc Function to extract strings from Files and write the PO-file.
%%
%% Options:
%%     %% Directory to output the PO-files.
%%     %% Default: "."
%%     {out_dir, OutDir :: file:filename()}
%%
%%     %% The tag for comments to the translator
%%     %% Default: "Translators: "
%%     {comment_tag, CommentTag :: gettexter:text()}
%%
%%     %% If the function should be verbose
%%     %% Default: false
%%     {verbose, Verbose :: boolean()}
%%
extract(Files, Options) ->
    %% Fix output directory
    OutDir = proplists:get_value(out_dir, Options, ?DEFAULT_OUT_DIR),
    filelib:ensure_dir(OutDir),

    %% Extract files
    Tab = extract_to_ets(Files, Options),

    %% Write PO-files
    gettexter_po_writer:write(Tab, OutDir),

    %% Cleanup the ets table
    ets:delete(Tab).

%% @doc Extract to an ETS-table.
%% For more informations see, extract/2.
-spec extract_to_ets([file:filename()], [{atom(), term()}]) -> ets:tab().
extract_to_ets(Files, Options) ->
    %% Create the ets-table for the dump
    Tab = ets:new(?MODULE, [public, ordered_set]),

    %% Construct parse transform options
    CompilerOptions = [
        {parse_transform, gettexter_extract}, binary, {i, "include"},
        {gettexter, [
            {comment_tag, proplists:get_value(comment_tag, Options, ?DEFAULT_COMMENT_TAG)},
            {verbose,     proplists:get_value(verbose,     Options, ?DEFAULT_VERBOSE)},
            {tab,         Tab}
        ]}
    ],

    %% Extract everything
    [{ok, _Module, _Beam} = compile:file(File, CompilerOptions) || File <- Files],
    Tab.

%% @doc Parse transform for extracting strings and binaries for translation.
%%
%% Options:
%%     {gettexter, [
%%
%%         %% Configure what tag to mark translator comments with. Defaults to "Translators:"
%%         {comment_tag, Tag :: string() | binary()}
%%
%%         %% Choose output table. This is not optional.
%%         {tab, Tab :: ets:tab()}
%%
%%         %% Set the transform to be verbose
%%         {verbose,  Verbose :: boolean()}
%%      ]}
%%
%% The transform will traverse the file and check for calls to gettexter. These will
%% be dumped into the provided ets table. When it encounters a function call it tries
%% to extract the arguments which may contain strings that should be translatable.
%%
%% Comments are also extracted if possible. For this to work the comment needs
%% to be immediatly above the function call to a gettexter-function.
%%
%% Additional percent signs and whitespaces after those percent signs are
%% removed. After that the beginning of the first comment line is matched
%% against the option comment_tag. If there is a match then the tag is removed
%% along additional whitespaces and the remaning comment is extracted. If there
%% are any empty lines in the beginning of the comment we remove those. If it
%% does not match we just ignore the comment.
%%
%% The entries in the ets table will have the format:
%%     {Key, MetaData}
%%      Key      :: {Domain, Context, Singular, Plural}
%%      MetaData :: [{Filename, LineNr, Comment}]
%%
%% This dump can then be used with gettexter_po_writer to convert the dump
%% into gettext PO-files.
%%
-spec parse_transform(Forms :: [erl_parse:abstract_form()], Options :: list()) ->
    [erl_parse:abstract_form()].
parse_transform(Forms, CompilerOptions) ->
    %% Get options
    Options = proplists:get_value(gettexter, CompilerOptions, []),

    %% Get the table
    Tab = proplists:get_value(tab, Options, ?MODULE),

    %% Get current file
    File = parse_trans:get_file(Forms),

    %% Get all comments from the file
    CommentTag = proplists:get_value(comment_tag, Options, ?DEFAULT_COMMENT_TAG),
    Comments = [{Start, Start + length(Comment) - 1, Comment} ||
                {Start, _, _, Comment} <- erl_comment_scan:file(File)],

    %% Should we be verbose?
    Verbose = proplists:get_value(verbose, Options, ?DEFAULT_VERBOSE),

    %% Configure the extraction
    Config = [{file, File}, {comment_tag, CommentTag}, {comments, Comments},
              {verbose, Verbose}],
    ets:insert(Tab, Config),

    %% Extract
    Extract = fun(Form) -> do_extract(Form, Tab) end,
    parse_trans:plain_transform(Extract, Forms),

    %% Clean up ets table of configuration
    [ets:delete(Tab, ConfigKey) || {ConfigKey, _} <- Config],
    Forms.

%% @doc Entry point for the parse transform.
do_extract({call, LineNr, {remote, _,{atom, _, gettexter}, {atom, _, F}}, AstArgs}, Tab) ->
    %% Try to extract the arguments
    case extract_args(F, AstArgs) of
        undefined ->
            ok;
        Args ->
            %% Check if there is a comment that ends on the line above
            Comment = extract_comment(LineNr, Tab),

            %% Dump the data to the ets-table
            dump(Tab, LineNr, Comment, Args)
    end,
    continue;
do_extract(_Form, _Tab) ->
    continue.

%% @doc Extract a comment from the ets table
extract_comment(LineNr, Tab) ->
    [{comments, Comments0}] = ets:lookup(Tab, comments),
    [{comment_tag, CommentTag}] = ets:lookup(Tab, comment_tag),
    case get_comment(LineNr, Comments0) of
        undefined ->
            undefined;
        Comment ->
            % Strip comments, remove all preceding % then spaces, a tag and
            % whitespaces after the tag. The rest is extracted as the actual comment.
            Strip = fun(Line) ->
                            case re:run(Line, "^\%*\s*") of
                                {match, [{0, Length}]} ->
                                    string:substr(Line, Length + 1);
                                _ ->
                                    Line
                            end
                    end,
            [FirstLine | RestLines]  = lists:map(Strip, Comment),

            %% Match the first line of the comment against the comment_tag with optional
            %% whitespaces and if it is a match. Remove it and extract this comment. We
            %% remove all empty lines from the head. This is then our extracted comment to
            %% the translator.
            %% Otherwise just return undefined.
            case re:run(FirstLine, ["^", CommentTag, "\s*"]) of
                {match, [{0, Length}]} ->
                    % Remove tag and whitespaces
                    StrippedComment = [string:substr(FirstLine, Length + 1) | RestLines],

                    %% Drop empty lines
                    lists:dropwhile(fun (Line) -> Line == [] end, StrippedComment);
                nomatch ->
                    undefined
            end
    end.

%% @doc Retrive a comment that lies before a given line
get_comment(_LineNr, []) ->
    undefined;
%% Comment should end on row above
get_comment(LineNr, [{Start, Stop, Comment} | _])
  when Start =< LineNr andalso Stop + 1 == LineNr ->
    Comment;
get_comment(LineNr, [{Start, _, _} | _]) when Start > LineNr ->
    undefined;
get_comment(LineNr, Rest) ->
    get_comment(LineNr, tl(Rest)).

%% @doc Normalize the arguments from all functions to a common format
%% The resulting tuple looks like this:
%%     {Domain, Context, Singular, Plural}

%% gettext/1,2
extract_args(gettext, [Text | _]) ->
    {undefined, undefined, Text, undefined};
%% ngettext/3,4
extract_args(ngettext, [Singular, Plural | _]) ->
    {undefined, undefined, Singular, Plural};
%% pgettext/2,3
extract_args(pgettext, [Context, Text | _]) ->
    {undefined, Context, Text, undefined};
%% npgettext/4,5
extract_args(npgettext, [Context, Singular, Plural | _]) ->
    {undefined, Context, Singular, Plural};
%% dgettext/2,3
extract_args(dgettext, [Domain, Text | _]) ->
    {Domain, undefined, Text, undefined};
%% dngettext/4,5
extract_args(dngettext, [Domain, Singular, Plural | _]) ->
    {Domain, undefined, Singular, Plural};
%% dpgettext/3,4
extract_args(dpgettext, [Domain, Context, Text | _]) ->
    {Domain, Context, Text, undefined};
%% dnpgettext/5,6
extract_args(dnpgettext, [Domain, Context, Singular, Plural | _]) ->
    {Domain, Context, Singular, Plural};
%% N/A
extract_args(_Function, _Args) ->
    undefined.

%% @doc Dump an entry into the ets table
%% Append function/line-number to the existing entry if one exist
-spec dump(Tab :: reference(), LineNr :: integer(), Comment :: [string()],
           Args :: {any(), any(), any(), any()}) -> ok.
dump(Tab, LineNr, Comment, Args) ->
    %% Try to extract strings from arguments
    case extract_strings(Args) of
        error ->
            %% Control output via flag
            [{verbose, Verbose}] = ets:lookup(Tab, warnings),
            Verbose andalso error_logger:warning_msg("Extraction failed.~n"
                                                     "LineNr: ~p~n"
                                                     "Args: ~p~n",
                                                     [LineNr, Args]);
        Key ->
            [{_, FileName}] = ets:lookup(Tab, file),
            FileInfo = case ets:lookup(Tab, Key) of
                           []           -> [];
                           [{_, FInfo}] -> FInfo
                       end,
            MetaData = {FileName, LineNr, Comment},
            ets:insert(Tab, {Key, [MetaData | FileInfo]})
    end.

%% @doc Extract the actual strings or return error
extract_strings({{atom, _, Domain}, Context, Singular, Plural}) ->
    extract_strings({Domain, Context, Singular, Plural});
extract_strings({Domain, Context, Singular, Plural}) ->
    try
        {Domain, to_string(Context), to_string(Singular), to_string(Plural)}
    catch
        _:_ -> error
    end.

%% @doc Turn the ast into strings (or undefined)
%% If this crashes the extraction will fail.
to_string(undefined) -> undefined;
to_string({atom, _, undefined}) -> undefined;
to_string({string, _, String}) -> String;
to_string({bin, _, Elements}) ->
    ToString = fun({bin_element, _, {string, _, String}, default, default}) ->
                       String
               end,
    lists:concat(lists:map(ToString, Elements)).
