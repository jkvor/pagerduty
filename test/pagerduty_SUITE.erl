-module(pagerduty_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [{group, application_start}, {group, api_calls}].

groups() ->
    [{application_start,
      [],
      [start,
       start_with_retry,
       start_with_os,
       start_with_os_retry]},
     {api_calls,
      [],
      [trigger,
       acknowledge,
       resolve]}].

init_per_suite(Config) ->
    [{endpoint, "https://events.pagerduty.com/generic/2010-04-15/create_event.json"} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(application_start, Config) ->
    Config;
init_per_group(api_calls, Config) ->
    application:set_env(pagerduty, service_key, "TEST_API_CALLS"),
    ok = a_start(pagerduty, temporary),
    Config.
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    application:unset_env(pagerduty, service_key),
    application:unset_env(pagerduty, retry_wait),
    os:unsetenv("PAGERDUTY_SERVICE_KEY"),
    os:unsetenv("PAGERDUTY_RETRY_WAIT"),
    meck:unload().

a_start(App, Type) ->
        start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
        ok = a_start(Dep, Type),
            a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
        erlang:error({app_start_failed, App, Reason}).

start(_Config) ->
    application:set_env(pagerduty, service_key, "TEST"),
    a_start(pagerduty, temporary),
    {ok, "TEST"} = pagerduty:service_key(),
    {ok, 5000} = pagerduty:retry_wait(),
    application:stop(pagerduty).

start_with_retry(_Config) ->
    application:set_env(pagerduty, service_key, "TEST_WITH_RETRY"),
    application:set_env(pagerduty, retry_wait, 3000),
    a_start(pagerduty, temporary),
    {ok, "TEST_WITH_RETRY"} = pagerduty:service_key(),
    {ok, 3000} = pagerduty:retry_wait(),
    application:stop(pagerduty).

start_with_os(_Config) ->
    os:putenv("PAGERDUTY_SERVICE_KEY", "TEST_WITH_OS"),
    a_start(pagerduty, temporary),
    {ok, "TEST_WITH_OS"} = pagerduty:service_key(),
    {ok, 5000} = pagerduty:retry_wait(),
    application:stop(pagerduty).

start_with_os_retry(_Config) ->
    os:putenv("PAGERDUTY_SERVICE_KEY", "TEST_WITH_OS_RETRY"),
    os:putenv("PAGERDUTY_RETRY_WAIT", "3000"),
    a_start(pagerduty, temporary),
    {ok, "TEST_WITH_OS_RETRY"} = pagerduty:service_key(),
    {ok, 3000} = pagerduty:retry_wait(),
    application:stop(pagerduty).

trigger(Config) ->
    event_page(trigger, Config).

acknowledge(Config) ->
    event_page(acknowledge, Config).

resolve(Config) ->
    event_page(resolve, Config).

event_page(EventType, Config) ->
    Endpoint = ?config(endpoint, Config),
    GoodResp = {ok, {{"", 200, ""}, "", ""}},
    RetryResp1 = {ok, {{"", 403, ""}, "", ""}},
    RetryResp2 = {ok, {{"", 500, ""}, "", ""}},
    BadResp = {ok, {{"", 400, ""}, "", <<"{\"status\":\"invalid\",\"message\":\"an invalid response\",\"errors\":[\"error 1\",\"error 2\"]}">>}},
    meck:new(httpc),
    meck:expect(httpc, request, [{[post, {Endpoint, [], "application/json", '_'}, [], []], GoodResp}]),
    {ok, sent} = pagerduty:call(EventType, "Test 200 Response", "Tesing pagerduty erlang module", undefined),

    meck:expect(httpc, request, [{[post, {Endpoint, [], "application/json", '_'}, [], []], RetryResp1}]),
    {ok, retry} = pagerduty:call(EventType, "Test 403 Response", "Tesing pagerduty erlang module", undefined),

    meck:expect(httpc, request, [{[post, {Endpoint, [], "application/json", '_'}, [], []], RetryResp2}]),
    {ok, retry} = pagerduty:call(EventType, "Test 500 Response", "Tesing pagerduty erlang module", undefined),

    meck:expect(httpc, request, [{[post, {Endpoint, [], "application/json", '_'}, [], []], BadResp}]),
    {error, Reason} = pagerduty:call(EventType, "Test 400 Response", "Tesing pagerduty erlang module", undefined),
    {<<"status">>, <<"invalid">>} = lists:keyfind(<<"status">>, 1, Reason),
    ok.

