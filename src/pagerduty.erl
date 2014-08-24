%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(pagerduty).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([trigger/2, trigger/3,
         acknowledge/2, acknowledge/3,
         resolve/2, resolve/3,
         service_key/0, retry_wait/0,
         call/4, cast/4]).

-record(state, {service_key=undefined :: string(),
                retry_wait=5000 :: pos_integer(),
                timer_ref=undefined :: reference()}).

%% API functions
start_link(ServiceKey, RetryWait) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServiceKey, RetryWait], []).

trigger(IncidentKey, Description) ->
    trigger(IncidentKey, Description, undefined).

trigger(IncidentKey, Description, Details) ->
    cast(trigger, IncidentKey, Description, Details).

acknowledge(IncidentKey, Description) ->
    trigger(IncidentKey, Description, undefined).

acknowledge(IncidentKey, Description, Details) ->
    cast(acknowledge, IncidentKey, Description, Details).

resolve(IncidentKey, Description) ->
    trigger(IncidentKey, Description, undefined).

resolve(IncidentKey, Description, Details) ->
    cast(resolve, IncidentKey, Description, Details).

call(EventType, IncidentKey, Description, Details) ->
    gen_server:call(?MODULE, {EventType, IncidentKey, Description, Details}, 10000).

cast(EventType, IncidentKey, Description, Details) ->
    gen_server:cast(?MODULE, {EventType, IncidentKey, Description, Details}).

service_key() ->
    gen_server:call(?MODULE, service_key).

retry_wait() ->
    gen_server:call(?MODULE, retry_wait).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init([ServiceKey, RetryWait]) ->
    {ok, #state{service_key=ServiceKey, retry_wait=parse_retry_wait(RetryWait)}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call(service_key, _From, State) ->
    {reply, {ok, State#state.service_key}, State};
handle_call(retry_wait, _From, State) ->
    {reply, {ok, State#state.retry_wait}, State};
handle_call({EventType, IncidentKey, Description, Details}, _From, #state{service_key=ServiceKey}=State) ->
    Result = build_and_post(EventType, ServiceKey, IncidentKey, Description, Details),
    {reply, Result, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(Incident={EventType, IncidentKey, Description, Details}, #state{service_key=ServiceKey}=State) ->
    case build_and_post(EventType, ServiceKey, IncidentKey, Description, Details) of
        {ok, sent} ->
            io:format("posted ~p event type to pagerduty: ~p~n", [EventType, IncidentKey]),
            {noreply, State};
        {ok, retry} ->
            io:format("retrying post ~p event type to pagerduty: ~p~n", [EventType, IncidentKey]),
            {noreply, start_timer(Incident, State)};
        Err ->
            io:format("error posting ~p event type to pagerduty: ~p~n", [EventType, Err]),
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info({timeout, TimerRef, {retry, Incident}}, State=#state{ timer_ref=TimerRef }) ->
    handle_cast(Incident, State);
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_and_post(EventType, ServiceKey, IncidentKey, Description, Details) ->
    case catch jsx:encode([{<<"service_key">>, to_bin(ServiceKey)},
                           {<<"incident_key">>, to_bin(IncidentKey)},
                           {<<"event_type">>, to_bin(EventType)},
                           {<<"description">>, to_bin(Description)}]
                          ++ [{<<"details">>, proplist_to_bin(Details)} || Details =/= undefined]) of
        {'EXIT', Err} ->
            Err;
        Json ->
            handle(post(Json))
    end.

post(Json) ->
    httpc:request(post,
                  {"https://events.pagerduty.com/generic/2010-04-15/create_event.json", [], "application/json", Json},
                  [],
                  []).
handle({ok,{{_,200,_},_,_}}) ->
    {ok, sent};
handle({ok,{{_,400,_},_,Body}}) ->
    try jsx:decode(Body) of
        Resp ->
            {error, Resp}
    catch
        badarg ->
            {error, bad_response}
    end;
handle({ok,{{_,Code,_},_,_}})
  when Code =:= 403;
       Code >= 500 andalso Code < 600 ->
    {ok, retry};
handle(Err) ->
    {error, Err}.


proplist_to_bin(PropList) ->
    [ {to_bin(Key), to_bin(Val)} || {Key, Val} <- PropList ].

to_bin(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_bin(X) when is_list(X) ->
    list_to_binary(X);
to_bin(X) when is_binary(X) ->
    X.

parse_retry_wait(Time) when is_list(Time) ->
    parse_retry_wait(list_to_integer(Time));
parse_retry_wait(Time) when is_integer(Time), Time > 0 ->
    Time;
parse_retry_wait(_) ->
    5000.

start_timer(Incident, State=#state{ retry_wait=Time }) ->
    TimerRef = erlang:start_timer(Time, ?MODULE, {retry, Incident}),
    State#state{ timer_ref=TimerRef }.
