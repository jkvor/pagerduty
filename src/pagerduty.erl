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
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([trigger/2, trigger/3, call/3, cast/3, build_json/1]).

-record(state, {service_key}).

%% API functions
start_link(ServiceKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServiceKey], []).

trigger(IncidentKey, Description) ->
    trigger(IncidentKey, Description, undefined).

trigger(IncidentKey, Description, Details) ->
    cast(IncidentKey, Description, Details).

call(IncidentKey, Description, Details) ->
    gen_server:call(?MODULE, {trigger, IncidentKey, Description, Details}, 10000).

cast(IncidentKey, Description, Details) ->
    gen_server:cast(?MODULE, {trigger, IncidentKey, Description, Details}).

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
init([ServiceKey]) ->
    {ok, #state{service_key=ServiceKey}}.

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
handle_call({trigger, IncidentKey, Description, Details}, _From, #state{service_key=ServiceKey}=State) ->
    case (catch build_json([
        {"service_key", ServiceKey},
        {"incident_key", IncidentKey},
        {"event_type", "trigger"},
        {"description", Description}] ++ [{"details", Details} || Details =/= undefined])) of
        {'EXIT', Err} ->
            {reply, Err, State};
        Json ->
            case post(Json) of
                {ok,{{_,200,_},_,_}} ->
                    {reply, ok, State};
                Err1 ->
                    {reply, Err1, State}
            end
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({trigger, IncidentKey, Description, Details}, #state{service_key=ServiceKey}=State) ->
    case (catch build_json([
        {"service_key", ServiceKey},
        {"incident_key", IncidentKey},
        {"event_type", "trigger"},
        {"description", Description}] ++ [{"details", Details} || Details =/= undefined])) of
        {'EXIT', Err} ->
            io:format("failed building json (~p, ~p, ~p): ~p~n", [IncidentKey, Description, Details, Err]);
        Json ->
            case post(Json) of
                {ok,{{_,200,_},_,_}} -> ok;
                Err1 ->
                    io:format("failed posting to pagerduty: ~p~n", [Err1])
            end
    end, 
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
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
build_json(Props) ->
    build_json(lists:reverse(Props), 0, []).

build_json([], _, Acc) ->
    iolist_to_binary([<<"{">>] ++ Acc ++ [<<"}">>]);

build_json([{Key, Value}|Tail], Index, Acc) ->
    build_json(Tail, Index+1, [
        [<<"'">>, Key, <<"': ">>,
            case Value of
                Atom when is_atom(Atom) -> [<<"'">>, atom_to_list(Atom), <<"'">>];
                [C|_]=List when is_integer(C) -> [<<"'">>, List, <<"'">>];
                Bin when is_binary(Bin) -> [<<"'">>, Bin, <<"'">>];
                Int when is_integer(Int) -> integer_to_list(Int);
                Float when is_float(Float) -> float_to_list(Float);
                [Tuple|_]=List when is_tuple(Tuple) -> build_json(List);
                Other -> io_lib:format("'~p'", [Other])
            end] ++
        [<<", ">> || Index > 0]|Acc]).

post(Json) ->
    httpc:request(post, {"https://events.pagerduty.com/generic/2010-04-15/create_event.json", [], "application/json", Json}, [], []).
