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
-export([process_measurement/2,
	 reset_parameters/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("cottage_alarm.hrl").

-define(SERVER, ?MODULE).
-define(MINUTE, 60).



-record(state, {min, 
		max,
		forget_period, 
		first_escalation, 
		second_escalation, 
		hot_temps, 
		cold_temps}).

%%%===================================================================
%%% API
%%%===================================================================

process_measurement( ServerRef, Temperature) ->
    gen_server:cast(ServerRef, {evaluate, Temperature}).

reset_parameters( ServerRef, Spec_file_string) ->
    gen_server:cast(ServerRef, {reset, Spec_file_string}).
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
    NewState = set_parameters( ?SPEC_FILE, #state{}),
    {ok,NewState#state{
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
handle_cast({reset, Spec_file_string}, State) ->
    NewState = set_parameters(Spec_file_string, State),
    {noreply, NewState};
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
    if Measurement < State#state.min -> too_low(Temp,
						State);
       Measurement > State#state.max -> too_high(Temp,					       
						 State);
       true -> ok
    end.


too_low(Temp,  State) ->
    New_temps = too_something( Temp, "COLD", 
			       State#state.cold_temps, 
			       State),
    State#state{cold_temps = New_temps }.


too_high(Temp,  State) ->
    New_temps = too_something( Temp, "HOT", 
			       State#state.hot_temps, 
			       State),
    State#state{hot_temps = New_temps }.

too_something(Temp, Type, Temp_list, State) ->
    
%%    io:format("Too ~s = ~p  State = ~p~n",[Type, Temp, State]),

    NewTemps = [Temp | forget_old_data(Temp_list, State) ],
    Escalation = State#state.first_escalation,
    Serious =  State#state.second_escalation,
    RecentTemps = which_measurements_during_last( Serious + 1, NewTemps ),
    case Count = length(RecentTemps) of
	Count when Count == 1 -> call_for_help( Temp, 
						Type, 
						?REPORT_DESTINATION, 
						State#state.min);
	Count when Count == 0 -> ok;
	Count when Count > Serious -> call_for_help( Temp, 
						lists:concat(["SERIOUS, URGENT ",Type]), 
						?REPORT_DESTINATION, 
						     State#state.min);
	Count when Count > Escalation -> call_for_help( Temp, 
						lists:concat(["URGENT ",Type]), 
						?REPORT_DESTINATION, 
						State#state.min);
	_Count -> ok

    end,
NewTemps.



forget_old_data( List, 
		 State) ->
    forget_old_data( List, 
		     State, 
		     []).

forget_old_data([], 
		_State, 
		NewList) ->
    lists:reverse(NewList);
forget_old_data([Item | List], 
		State, 
		NewList) ->
    case too_old(Item, State) of
	true ->
	    forget_old_data( List, State, NewList);
	false ->
	    forget_old_data( List, State, [Item | NewList])
    end.


too_old( Item, State) ->
    Forget_duration = State#state.forget_period * ?MINUTE,
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    {Item_datetime, _} = Item,
    DateTime = calendar:datetime_to_gregorian_seconds(Item_datetime),
    case Now - DateTime - Forget_duration of
	Difference when Difference > 0 ->
	    true;
	_Difference -> false
    end.



set_parameters( Source_file, State) ->
    case  file:consult( Source_file) of
	{ok,Specs} ->
	    Minimum = proplists:get_value(min, Specs),
	    Maximum = proplists:get_value(max, Specs),
	    Forget_period = proplists:get_value(minutes_to_forget, Specs),
	    First_escalation = proplists:get_value(minutes_to_watch_and_escalate, Specs),
	    Second_escalation = proplists:get_value(minutes_to_watch_and_escalate_further, Specs),
	    State#state{min = Minimum, 
			max = Maximum, 
			forget_period = Forget_period,
			first_escalation = First_escalation,
			second_escalation = Second_escalation};
	{error,_} -> 
	    io:format("Could not open ~p to load parameters~n",
		      [Source_file]),
	    State#state{min = 45,
			max = 90, 
			forget_period = 20,
			first_escalation = 4,
			second_escalation = 12}
    end.



which_measurements_during_last( Minutes, List) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Breakpoint = Now - (Minutes * ?MINUTE),
    lists:filter(fun(X) -> {Datetime, _} = X, 
Value =   calendar:datetime_to_gregorian_seconds(Datetime),
			Value  > Breakpoint  end, List).



call_for_help( Temp, Type,  To_string, Limit) ->
    %% deliver results via email
    {Datetime, Measurement} = Temp,

    Content = io_lib:format(" ~s temperature alert at ~p~n. Termperature is measured at ~p.~n The limit is ~p.~n",[Type,
														   Datetime,
														   Measurement,
														   Limit]),
    Title = io_lib:format(" ~s temperature alert on ~s",[Type, format_datetime(Datetime)]),
    ok = file:write_file(?TMPFILE, Content),
    case 
	os:cmd(lists:concat(["mutt -s \"", Title,"\"  "," --  ", To_string," < ", ?TMPFILE, "\n"])) of
	[] ->
	    ok;
	error ->
	    error
    end.

format_datetime(Datetime) ->
    {{Year, Month, Day},{Hour, Minute, Second}} = Datetime,
    io_lib:format("~b-~2..0b-~2..0b ~b:~2..0b:~2..0b",[Year, Month, Day,Hour, Minute, Second]).
