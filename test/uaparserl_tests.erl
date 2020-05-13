-module(uaparserl_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMEOUT, 60.0).

%% fixtures

-define(EXAMPLE_UA,
        [[{"regex", "(ESPN)[%20| ]+Radio/(\\d+)\\.(\\d+)\\.(\\d+) CFNetwork"}],
         [{"regex", "(Antenna)/(\\d+) CFNetwork"},
          {"family_replacement", "AntennaPod"}],
         [{"regex", "(TopPodcasts)Pro/(\\d+) CFNetwork"}]]).
-define(EXAMPLE_OS,
        [[{"regex", "HbbTV/\\d+\\.\\d+\\.\\d+ \\( ;(LG)E ;NetCast 4.0"},
          {"regex_flag", "i"}]]).
-define(EXAMPLE_DEVICE,
        [[{"regex",
           "(?:(?:iPhone|Windows CE|Windows Phone|Android).*(?:(?:Bot|Yeti)-Mobi"
           "le|YRSpider|BingPreview|bots?/\\d|(?:bot|spider)\\.html)|AdsBot-Goog"
           "le-Mobile.*iPhone)"},
          {"regex_flag", "i"},
          {"device_replacement", "Spider"},
          {"brand_replacement", "Spider"},
          {"model_replacement", "Smartphone"}]]).
-define(EXAMPLE_FULL,
        [[{"user_agent_parsers", ?EXAMPLE_UA},
          {"os_parsers", ?EXAMPLE_OS},
          {"device_parsers", ?EXAMPLE_DEVICE}]]).

%% tests

private_patterns_funs_test_() ->
    [{"extract_pattern_sets/1 returns a flatterned list",
      fun () ->
              ?assertMatch([[_ | _], [_ | _], [_ | _]],
                           uaparserl:extract_pattern_sets(?EXAMPLE_FULL))
      end},
     {"normalize_pattern/1 returns expected output",
      fun () ->
              ?assertEqual({test_atom, "string"},
                           uaparserl:normalize_pattern({"test_atom",
                                                        "string"}))
      end},
     {"normalize_pattern_sets/1 returns expected output]",
      fun () ->
              ?assertMatch([[{regex, _}],
                            [{regex, _}, {family_replacement, _}],
                            [{regex, _}]],
                           uaparserl:normalize_pattern_sets(?EXAMPLE_UA))
      end},
     {"get_regex_flags/1 finds flag when set",
      fun () ->
              ?assertEqual([caseless],
                           uaparserl:get_regex_flags([{regex_flag, "i"}]))
      end},
     {"get_regex_flags/1 returns empty list when regex_flag is missing",
      fun () ->
              ?assertEqual([], uaparserl:get_regex_flags([]))
      end},
     {"compile_pattern_set/1 successfully compiles with flag",
      fun () ->
              Result = uaparserl:compile_pattern_set([{regex,
                                                       "(WebTV)/(\d+).(\d+)"},
                                                      {regex_flag, "i"}]),
              Regex = proplists:get_value(compiled, Result),

              ?assertMatch({re_pattern, _, _, _, _}, Regex)
      end},
     {"compile_pattern_set/1 successfully compiles without flag",
      fun () ->
              Result = uaparserl:compile_pattern_set([{regex,
                                                       "(WebTV)/(\d+).(\d+)"}]),
              Regex = proplists:get_value(compiled, Result),

              ?assertMatch({re_pattern, _, _, _, _}, Regex)
      end},
     {"compile_pattern_sets/1 successfully compiles all regexes",
      fun () ->
              PatternSets = uaparserl:extract_pattern_sets(?EXAMPLE_FULL),
              NormalizedSets = uaparserl:normalize_pattern_sets(PatternSets),
              CompiledSets = uaparserl:compile_pattern_sets(NormalizedSets),
              [assert_is_regex(proplists:get_value(compiled, hd(Result)))
               || Result <- CompiledSets]
      end}].

private_parse_funs_test_() ->
    [{"format_result/4 formats text with placeholders correctly",
      fun () ->
              _UserAgentString =
                  "Mozilla/5.0 (Windows; Windows NT 5.1; rv:2.0b3pre) Gecko/20100727 "
                  "Minefield/4.0.1pre",
              ?assertEqual({useragent,
                            [{family, "Firefox (Minefield)"},
                             {major, "4"},
                             {minor, "0"},
                             {patch, "1pre"}]},
                           uaparserl:format_result(useragent,
                                                   ["Minefield",
                                                    "4",
                                                    "0",
                                                    "1pre"],
                                                   [{family_replacement,
                                                     "Firefox ($1)"}]))
      end},
     {"format_result/4 formats reordered results correctly",
      fun () ->
              _UserAgentString = "Apple Mac OS X Maverics 10.4.2.111",
              ?assertEqual({os,
                            [{family, "Mac OS X"},
                             {major, "10"},
                             {minor, "4"},
                             {patch, "2"},
                             {patch_minor, undefined}]},
                           uaparserl:format_result(os,
                                                   ["10", "4", "2"],
                                                   [{os_replacement,
                                                     "Mac OS X"},
                                                    {os_v1_replacement, "$1"},
                                                    {os_v2_replacement, "$2"},
                                                    {os_v3_replacement,
                                                     "$3"}]))
      end},
     {"format_result/4 formats multiple placeholders correctly",
      fun () ->
              _UserAgentString =
                  "Mozilla/5.0 (Linux; U; Android 4.2.2; de-de; PEDI_PLUS_W Build/JDQ39"
                  ") AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30",
              ?assertEqual({device,
                            [{family, "Odys PEDI PLUS W"},
                             {brand, "Odys"},
                             {model, "PEDI PLUS W"}]},
                           uaparserl:format_result(device,
                                                   ["PEDI", "PLUS", "W"],
                                                   [{device_replacement,
                                                     "Odys $1 $2 $3"},
                                                    {brand_replacement,
                                                     "Odys"},
                                                    {model_replacement,
                                                     "$1 $2 $3"}]))
      end},
     {"parse/1 parses device placeholders correctly",
      fun () ->
              UserAgentString =
                  "Mozilla/5.0 (Linux; Android 4.2.2; ZP998 Special Build/JDQ39) "
                  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.170 "
                  "Mobile Safari/537.36",
              ?assertEqual([{useragent,
                             [{family, "Chrome Mobile"},
                              {major, "33"},
                              {minor, "0"},
                              {patch, "1750"}]},
                            {os,
                             [{family, "Android"},
                              {major, "4"},
                              {minor, "2"},
                              {patch, "2"},
                              {patch_minor, undefined}]},
                            {device,
                             [{family, "ZP998 Special"},
                              {brand, "Zopo"},
                              {model, "ZP998 Special"}]}],
                           uaparserl:parse(UserAgentString))
      end}].

%% comprehensive tests from upstream

ua_cases_test_() ->
    {setup, fun load_ua_cases/0, fun test_ua_cases/1}.

os_cases_test_() ->
    {setup, fun load_os_cases/0, fun test_os_cases/1}.

device_cases_test_() ->
    {setup, fun load_device_cases/0, fun test_device_cases/1}.

%% UA test cases

load_ua_cases() ->
    application:start(yamerl),

    FileName = "test_ua.yaml",
    FilePath = filename:join(uaparserl:priv_dir(), FileName),
    FileData = yamerl_constr:file(FilePath),
    [[{"test_cases", Cases}]] = FileData,
    uaparserl:normalize_pattern_sets(Cases).

test_ua_cases(Cases) ->
    {inparallel,
     [{proplists:get_value(user_agent_string, Case),
       {timeout, ?TEST_TIMEOUT, test_ua_case(Case)}}
      || Case <- Cases]}.

test_ua_case(Case) ->
    fun () ->
            UAString = proplists:get_value(user_agent_string, Case),
            Keys = [family, major, minor, patch],
            Example = [{K, proplists:get_value(K, Case)} || K <- Keys],
            Expected = sanitize_values(Example),
            Parsed = uaparserl:parse(UAString),
            Result = proplists:get_value(useragent, Parsed),
            ?assertEqual(Expected, Result)
    end.

%% OS test cases

load_os_cases() ->
    application:start(yamerl),

    FileName = "test_os.yaml",
    FilePath = filename:join(uaparserl:priv_dir(), FileName),
    FileData = yamerl_constr:file(FilePath),
    [[{"test_cases", Cases}]] = FileData,
    uaparserl:normalize_pattern_sets(Cases).

test_os_cases(Cases) ->
    {inparallel,
     [{proplists:get_value(user_agent_string, Case),
       {timeout, ?TEST_TIMEOUT, test_os_case(Case)}}
      || Case <- Cases]}.

test_os_case(Case) ->
    fun () ->
            UAString = proplists:get_value(user_agent_string, Case),
            Keys = [family, major, minor, patch, patch_minor],
            Example = [{K, proplists:get_value(K, Case)} || K <- Keys],
            Expected = sanitize_values(Example),
            Parsed = uaparserl:parse(UAString),
            Result = proplists:get_value(os, Parsed),
            ?assertEqual(Expected, Result)
    end.

%% device test cases

load_device_cases() ->
    application:start(yamerl),

    FileName = "test_device.yaml",
    FilePath = filename:join(uaparserl:priv_dir(), FileName),
    FileData = yamerl_constr:file(FilePath),
    [[{"test_cases", Cases}]] = FileData,
    uaparserl:normalize_pattern_sets(Cases).

test_device_cases(Cases) ->
    {inparallel,
     [{proplists:get_value(user_agent_string, Case),
       {timeout, ?TEST_TIMEOUT, test_device_case(Case)}}
      || Case <- Cases]}.

test_device_case(Case) ->
    fun () ->
            UAString = proplists:get_value(user_agent_string, Case),
            Keys = [family, brand, model],
            Example = [{K, proplists:get_value(K, Case)} || K <- Keys],
            Expected = sanitize_values(Example),
            Parsed = uaparserl:parse(UAString),
            Result = proplists:get_value(device, Parsed),
            ?assertEqual(Expected, Result)
    end.

%% custom assertions

assert_is_regex(Regex) ->
    ?assertMatch({re_pattern, _, _, _, _}, Regex).

%%  helpers

sanitize_values([_ | _] = Items) ->
    lists:map(fun sanitize_value/1, Items).

sanitize_value({K, null}) ->
    {K, undefined};
sanitize_value({K, V}) ->
    {K, V}.
