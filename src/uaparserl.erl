-module(uaparserl).

-export([parse/1, reload/0, update/0]).

-ifdef(TEST).

-compile(export_all).

-endif.

-define(RESOURCE_URL,
        "https://raw.githubusercontent.com/ua-parser/uap-core/master/").
-define(RESOURCE_FILES,
        ["regexes.yaml",
         "tests/test_device.yaml",
         "tests/test_os.yaml",
         "tests/test_ua.yaml"]).
-define(PATTERNS_FILE, "regexes.yaml").
-define(PATTERNS_MODULE, uaparserl_patterns).
-define(UNKNOWN, "Other").

%% public functions

parse(UAString) ->
    Sanitized = sanitize(UAString),
    [UAPatterns, OSPatterns, DevicePatterns] = patterns(),
    UAMatch = find_match(useragent, UAPatterns, Sanitized),
    OSMatch = find_match(os, OSPatterns, Sanitized),
    DeviceMatch = find_match(device, DevicePatterns, Sanitized),
    [UAMatch, OSMatch, DeviceMatch].

reload() ->
    code:purge(?PATTERNS_MODULE),
    [_ | _] = patterns(),
    ok.

update() ->
    ok = download_resources(),
    reload().

%% private functions

find_match(Category, [], _UAString) ->
    {Category, zip(category_keys(Category), [?UNKNOWN])};
find_match(Category, [Rule | T], UAString) ->
    Regex = proplists:get_value(compiled, Rule),
    case re:run(UAString, Regex, [global, {capture, all, list}]) of
      {match, Results} ->
          [_FullMatch | Matches] = hd(Results),
          format_result(Category, Matches, Rule);
      nomatch ->
          find_match(Category, T, UAString);
      Unknown ->
          erlang:display({unknown, Unknown})
    end.

%% lookup functions

category_keys(useragent) ->
    [family, major, minor, patch];
category_keys(os) ->
    [family, major, minor, patch, patch_minor];
category_keys(device) ->
    [family, brand, model].

default_replacements(useragent) ->
    [{family_replacement, "$1"},
     {v1_replacement, "$2"},
     {v2_replacement, "$3"},
     {v3_replacement, "$4"}];
default_replacements(os) ->
    [{os_replacement, "$1"},
     {os_v1_replacement, "$2"},
     {os_v2_replacement, "$3"},
     {os_v3_replacement, "$4"},
     {os_v4_replacement, "$5"}];
default_replacements(device) ->
    [{device_replacement, "$1"},
     {brand_replacement, "$2"},
     {model_replacement, "$1"}].

%% formatters

format_result(Category, Matches, Rule) ->
    Lookup = build_lookup(Matches),
    Replacements = get_replacements(Category, Rule),
    Result = [apply_replacements(Item, Lookup) || Item <- Replacements],
    Keys = category_keys(Category),
    Zipped = zip(Keys, Result),
    {Category, format_values(Zipped)}.

format_values([_ | _] = Values) ->
    lists:map(fun format_value/1, Values).

format_value({Key, Value}) ->
    {Key, format_value(Value)};
format_value(Value)
    when Value == "$1";
         Value == "$2";
         Value == "$3";
         Value == "$4";
         Value == "$5";
         Value == "$6";
         Value == "$7";
         Value == "$8";
         Value == "$9" ->
    undefined;
format_value([] = _Value) ->
    undefined;
format_value(Value) ->
    Value.

format_index(Value) ->
    "$" ++ integer_to_list(Value).

sanitize(String) ->
    string:trim(String).

%% utility functions

build_lookup(Values) ->
    build_lookup(lists:seq(1, 9), Values, []).

build_lookup([Key | Keys] = _, [Val | Values] = _, Acc) ->
    build_lookup(Keys, Values, [{format_index(Key), Val} | Acc]);
build_lookup([Key | Keys] = _, [] = Values, Acc) ->
    build_lookup(Keys, Values, [{format_index(Key), ""} | Acc]);
build_lookup([] = _, [] = _, Acc) ->
    lists:reverse(Acc).

get_replacements(Category, Rule) ->
    get_replacements(default_replacements(Category), Rule, []).

get_replacements([{Key, Position} | Replacements] = _, Rule, Acc) ->
    Value = proplists:get_value(Key, Rule, Position),
    get_replacements(Replacements, Rule, [{Key, Value} | Acc]);
get_replacements([] = _, _Rule, Acc) ->
    lists:reverse(Acc).

apply_replacements({Key, Value}, [{Placeholder, Replacement} | T]) ->
    Result = string:replace(Value, Placeholder, Replacement, all),
    apply_replacements({Key, lists:flatten(Result)}, T);
apply_replacements({Key, Value}, []) ->
    {Key, string:trim(Value)}.

%% BIF alternatives

zip(Left, Right) when is_list(Left), is_list(Right) ->
    zip(Left, Right, []).

zip([HR | TR], [{_HLK, HL} | TL], Acc) ->
    zip(TR, TL, [{HR, HL} | Acc]);
zip([HR | TR], [HL | TL], Acc) ->
    zip(TR, TL, [{HR, HL} | Acc]);
zip([HR | TR], [], Acc) ->
    zip(TR, [], [{HR, undefined} | Acc]);
zip([], [], Acc) ->
    lists:reverse(Acc).

%% setup functions

load_patterns() ->
    application:ensure_started(yamerl),

    FilePath = filename:join(priv_dir(), ?PATTERNS_FILE),
    FileData = case file:read_file(FilePath) of
                 {ok, Data} ->
                     Data;
                 {error, _Error} ->
                     throw({error,
                            "lookup resources missing, use uaparserl:update/0 to collect them"})
               end,
    YAMLData = yamerl_constr:string(FileData),
    PatternSets = extract_pattern_sets(YAMLData),
    NormalizedSets = normalize_pattern_sets(PatternSets),
    CompiledSets = compile_pattern_sets(NormalizedSets),

    CompiledSets.

extract_pattern_sets([PatternData] = _FileData) ->
    UAParsers = proplists:get_value("user_agent_parsers", PatternData, []),
    OSParsers = proplists:get_value("os_parsers", PatternData, []),
    DeviceParsers = proplists:get_value("device_parsers", PatternData, []),
    [UAParsers, OSParsers, DeviceParsers].

normalize_pattern_sets([{_K, _V} | _] = PatternSet) ->
    lists:map(fun normalize_pattern/1, PatternSet);
normalize_pattern_sets([[_H | _T]] = PatternSets) ->
    lists:map(fun normalize_pattern_sets/1, PatternSets);
normalize_pattern_sets(PatternSets) when is_list(PatternSets) ->
    lists:map(fun normalize_pattern_sets/1, PatternSets).

normalize_pattern({Key, Value}) ->
    {list_to_atom(Key), Value}.

compile_pattern_sets([{_K, _V} | _] = PatternSet) ->
    compile_pattern_set(PatternSet);
compile_pattern_sets([[_H | _T]] = PatternSets) ->
    lists:map(fun compile_pattern_sets/1, PatternSets);
compile_pattern_sets(PatternSets) when is_list(PatternSets) ->
    lists:map(fun compile_pattern_sets/1, PatternSets).

compile_pattern_set(PatternSet) ->
    Regex = get_regex(PatternSet),
    FoundFlags = get_regex_flags(PatternSet),
    DefaultFlags = default_regex_flags(),
    Compiled = re:compile(Regex, DefaultFlags ++ FoundFlags),
    handle_compile_pattern(Compiled, PatternSet).

handle_compile_pattern({ok, Compiled}, PatternSet) ->
    add_regex(PatternSet, Compiled);
handle_compile_pattern({error, _Error}, _PatternSet) ->
    [].

add_regex(PatternSet, Compiled) ->
    [{compiled, Compiled}] ++ PatternSet.

get_regex(PatternSet) ->
    {regex, Regex} = lists:keyfind(regex, 1, PatternSet),
    Regex.

default_regex_flags() ->
    [unicode].

get_regex_flags([_ | _] = PatternSet) ->
    get_regex_flags(lists:keyfind(regex_flag, 1, PatternSet));
get_regex_flags({regex_flag, "i"}) ->
    [caseless];
get_regex_flags(_) ->
    [].

priv_dir() ->
    code:priv_dir(?MODULE).

download_resources() ->
    IsOK = fun (Value) ->
                   Value == ok
           end,
    Result = lists:map(fun download_resource/1, ?RESOURCE_FILES),
    lists:all(IsOK, Result) andalso ok.

download_resource(Path) ->
    Target = ?RESOURCE_URL ++ Path,
    Outfile = filename:join(priv_dir(), filename:basename(Path)),
    Tmpfile = Outfile ++ "-tmp",
    {ok, saved_to_file} = httpc:request(get,
                                        {Target, []},
                                        [],
                                        [{stream, Tmpfile}]),
    ok = file:rename(Tmpfile, Outfile),
    ok.

patterns() ->
    Module = load_patterns_module(),
    Module:patterns().

load_patterns_module() ->
    load_patterns_module(code:ensure_loaded(?PATTERNS_MODULE)).

load_patterns_module({module, Mod}) ->
    Mod;
load_patterns_module({error, _Reason}) ->
    Mod = ?PATTERNS_MODULE,
    ModFile = atom_to_list(Mod) ++ ".erl",
    Patterns = load_patterns(),
    Bin = compile(Mod, Patterns),
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, ModFile, Bin),
    Mod.

compile(Module, T) ->
    Forms = forms(Module, T),
    {ok, Module, Bin} = compile:forms(Forms, [verbose, report_errors]),
    Bin.

forms(Module, T) ->
    [erl_syntax:revert(X) || X <- term_to_abstract(Module, patterns, T)].

term_to_abstract(Module, Getter, T) ->
    [%% -module(Module).
     erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
     %% -export([Getter/0]).
     erl_syntax:attribute(erl_syntax:atom(export),
                          [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(Getter),
                                                                       erl_syntax:integer(0))])]),
     %% Getter() -> T.
     erl_syntax:function(erl_syntax:atom(Getter),
                         [erl_syntax:clause([],
                                            none,
                                            [erl_syntax:abstract(T)])])].
