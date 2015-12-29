%%%-------------------------------------------------------------------
%%% @author Carl A. Wright <wright@servicelevel.net>
%%% @copyright (C) 2015, Carl A. Wright
%%% @doc
%%%
%%% @end
%%% Created :  5 Dec 2015 by Carl A. Wright <wright@servicelevel.net>
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Carl A. Wright <wright@servicelevel.net>
%%% @copyright (C) 2015, Carl A. Wright
%%% @doc
%%%
%%% @end
%%% Created :  5 Dec 2015 by Carl A. Wright <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(cottage_watcher).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 cw_collect/3,
	 send_temp_report/2,
	 send_pressure_report/2,
	 write_CSV/2,
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
-define(TMPFILE, "body.txt").
-define(ATTACHMENT_FILE, "temps.txt").
-define(EMAIL_TEMPS_SUBJECT, "A day of temperature data").
-define(EMAIL_PRESSURES_SUBJECT, "A day of pressure data").
-define(EMAIL_CONTENT, "The data is in the attachment").
-define(MEASUREMENT_INTERVAL, 60 * 1000).

-record(state, {sensor_pid, temps, pressures}).

%%%===================================================================
%%% API
%%%===================================================================


%% collect for a period of time

cw_collect(PID, Number, minutes) ->
    gen_server:call(PID, {minutes, Number}).


send_temp_report( PID, Address_string ) ->
    gen_server:cast(PID, {report_temps, Address_string}).

send_pressure_report( PID, Address_string ) ->
    gen_server:cast(PID, {report_pressures, Address_string}).


%% get a list of measurements

minute_measures(PID) ->
    gen_server:call(PID,{a_minute_of_measurements}, 90 * 1000).

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

avg_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    lists:foldl(fun( X, Sum) -> X + Sum end, 0, TempList) / length(TempList).

avg_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    lists:foldl(fun( X, Sum) -> X + Sum end, 0, TempList) / length(TempList).


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
    erlang:send_after(?MEASUREMENT_INTERVAL, self(),take_measurement),
    {ok, #state{sensor_pid = PID, temps = [], pressures = []}}.

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
handle_call({minutes, _Number}, _From, State) ->
						%    Reply = lists:reverse(lists:sublist(State#state.temps, Number)),
    Reply = State#state.temps,
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
handle_cast({report_temps, Address}, State) ->
    Measurements = State#state.temps,
    write_CSV( Measurements, ?ATTACHMENT_FILE),
    email( Address, ?EMAIL_TEMPS_SUBJECT, ?EMAIL_CONTENT, ?ATTACHMENT_FILE),
    {noreply, State};
handle_cast({report_pressures, Address}, State) ->
    Measurements = State#state.pressures,
    write_CSV( Measurements, ?ATTACHMENT_FILE),
    email( Address, ?EMAIL_PRESSURES_SUBJECT, ?EMAIL_CONTENT, ?ATTACHMENT_FILE),
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
handle_info(take_measurement, State) ->

    {ok,Datetime, Temp, Pressure} = one_measurement( State#state.sensor_pid, 10),

    Temps_list = State#state.temps,
    NewTemps_list = update_list(Temps_list,{Datetime, round(Temp ,2)}, 1440),

    Pressures_list = State#state.pressures,
    NewPressures_list = update_list(Pressures_list,{Datetime, round(Pressure ,2)}, 1440),
    NewState = State#state{temps = NewTemps_list, pressures = NewPressures_list},

    erlang:send_after(?MEASUREMENT_INTERVAL, self(),take_measurement),
    {noreply, NewState}.

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


%% a function used to drive smallest and largest value extractions

extreme(List, Fun) ->
    lists:foldl(Fun, lists:last(List), List). 

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

%% write a list of measurements to a CSV file


write_CSV( List, File_path ) ->
    case file:open(File_path,[write]) of
	{ok,F} ->
	    io:format(F, '"date","temp"~n',[]),
	    write_CSV(item, F, List);
	{error, Reason} -> {error, Reason}
    end.

write_CSV(item, F, []) ->
    file:close(F);
write_CSV(item, F, [Item | List]) ->
    {{{Year, Month, Day},{Hour, Minute, Second}}, Temp} = Item,

    %% an example of the output for the line below is 
    %% 2015-12-27  18:45:00,72.26
    %% It is the date , time and then temperature 

    io:format(F, "~b-~2..0b-~2..0b  ~2..0b:~2..0b:~2..0b,~p~n",[Year, Month, Day,Hour, Minute,Second, Temp]),
    write_CSV(item, F, List).


%% adds a new value ot the beginning of a reversed list and trims to maximum length
%%
update_list(List,NewVal, Length) ->
    lists:sublist( [NewVal | List], Length).

%% deliver results via email

%%email(To, Title, Content) ->
%%
						%    email(To, Title, Content, []).

email(To, Title, Content, []) ->
    ok = file:write_file(?TMPFILE, Content),
    case 
	os:cmd(lists:concat(["mutt -s \"", Title,"\"  --  ", To," < ",?TMPFILE, "\n"])) of
	[] ->
	    ok;
	error ->
	    error
    end;
email(To, Title, Content, Attachment_path) ->
    ok = file:write_file(?TMPFILE, Content),
    case 
	os:cmd(lists:concat(["mutt -s \"", Title,"\" -a ",Attachment_path, " --  ", To," < ", ?TMPFILE, "\n"])) of
	[] ->
	    ok;
	error ->
	    error
    end.
