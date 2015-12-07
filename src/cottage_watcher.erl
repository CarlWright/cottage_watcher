%%%-------------------------------------------------------------------
%%% @author root <root@load>
%%% @copyright (C) 2015, root
%%% @doc
%%%
%%% @end
%%% Created :  5 Dec 2015 by root <root@load>
%%%-------------------------------------------------------------------
-module(cottage_watcher).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 minute_measures/1,
	 min_temp/1,
	 max_temp/1,
	 min_pressure/1,
	 max_pressure/1,
	 avg_temp/1,
	 avg_pressure/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sensor_pid}).

%%%===================================================================
%%% API
%%%===================================================================
minute_measures(PID) ->
    gen_server:call(PID,{a_minute_of_measurements}, 90 * 1000).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    PID = erlang:whereis(bmp085),
    {ok, #state{sensor_pid = PID}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({a_minute_of_measurements}, _From, State) ->
    Reply =  sixty_seconds_measure(State#state.sensor_pid),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sixty_seconds_measure(Sensor) ->
    sixty_seconds_measure(Sensor,[],60).

sixty_seconds_measure(_Sensor,Result, 0) ->
    lists:reverse(Result);
sixty_seconds_measure(Sensor,Result, N) ->
    {ok, Time, Temp, Pressure} = one_measurement(Sensor, 950),
    sixty_seconds_measure(Sensor, [ {Time, Temp, Pressure} | Result], N-1).

one_measurement(Sensor, Pause) ->
    timer:sleep(Pause),
    {ok, _Celsius, Fahrenheit} = bmp085:read_temp(Sensor),
    {ok,Pressure} = bmp085:read_pressure(Sensor, standard),
    {ok, erlang:localtime(), Fahrenheit, Pressure}.

%% Extract characteristics of a list of measurements

min_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    extreme(TempList, fun(X,Smallest) -> if X < Smallest -> X; true -> Smallest end end).

max_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    extreme(TempList, fun(X,Smallest) -> if X < Smallest -> Smallest; true -> X end end).

min_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    extreme(TempList, fun(X,Smallest) -> if X < Smallest -> X; true -> Smallest end end).

max_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    extreme(TempList, fun(X,Smallest) -> if X < Smallest -> Smallest; true -> X end end).

extreme(List, Fun) ->
    lists:foldl(Fun, lists:last(List), List). 

avg_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    lists:foldl(fun( X, Sum) -> X + Sum end, 0, TempList) / length(TempList).

avg_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    lists:foldl(fun( X, Sum) -> X + Sum end, 0, TempList) / length(TempList).
