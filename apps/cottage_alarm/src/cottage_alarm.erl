%%%-------------------------------------------------------------------
%%% @author Carl A. Wright <wright@servicelevel.net>
%%% @copyright (C) 2015, Carl A. Wright
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2015 by Carl A. Wright <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(cottage_alarm).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([process_measurement/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SPEC_FILE,"alarms_spec.txt").

-record(state, {min, max, hot_temps, cold_temps}).

%%%===================================================================
%%% API
%%%===================================================================

process_measurement( ServerRef, Temperature) ->
    gen_server:cast(ServerRef, {evaluate, Temperature}).
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
    {ok,Specs} = file:consult(?SPEC_FILE),
    Minimum = proplists:get_value(min, Specs),
    Maximum = proplists:get_value(max, Specs),
    {ok, #state{min = Minimum, 
		max = Maximum, 
		cold_temps = [],
		hot_temps = []}
    }.

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
handle_cast({evaluate, Temperature}, State) ->
    NewState = evaluate_temp(Temperature, State),
    {noreply, NewState}.

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
evaluate_temp(Temp, State) ->
    {_, Measurement} = Temp,
    if Measurement < State#state.min -> too_low(Temp, State);
       Measurement > State#state.max -> too_high(Temp, State)
    end.


too_low(Temp, State) ->
    io:format("Too low = ~p  State = ~p~n",[Temp, State]),

    State#state{cold_temps = [Temp | State#state.cold_temps]}.

too_high(Temp, State) ->
    io:format("Too high = ~p  State = ~p~n",[Temp, State]),

    State#state{hot_temps = [Temp | State#state.hot_temps]}.
