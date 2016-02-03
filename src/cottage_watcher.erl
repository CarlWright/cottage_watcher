
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
	 send_temp_report/2,
	 send_pressure_report/2,
	 reset_parameters/2,
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
-include( "cottage_watcher.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(MEASUREMENT_INTERVAL, 60 * 1000).

-record(state, {sensor_pid,
		alarm_pid, 
		beacon_pid,
		temps, 
		pressures}).

%%%===================================================================
%%% API
%%%===================================================================


%% collect for a period of time


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
    min_measurement(TempList).

max_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    max_measurement(TempList).

min_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    min_measurement(TempList).

max_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    max_measurement(TempList).

avg_temp(List) ->
    TempList = lists:map(fun(X) -> {_,Y,_} = X, Y end ,List),
    avg_measurement(TempList).

avg_pressure(List) ->
    TempList = lists:map(fun(X) -> {_,_,Y} = X, Y end ,List),
    avg_measurement(TempList).


%% reset the parameters from a System Specitifcation file

reset_parameters(PID, Filename) ->
    gen_server:cast( PID, {reset_parameters, Filename}).

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
    {ok, #state{sensor_pid = PID, 
		alarm_pid = 0, 
		temps = [], 
		pressures = []}}.

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
    Measurements = lists:reverse(State#state.temps),
    {Datetime,_} = lists:nth(1, Measurements),
    report_dispatch(temperature,
		    Measurements, 
		    Address, 
		    format_subject( Datetime, 
				    "temperature"),
		    ?TEMPS_ATTACHMENT_FILE),
    {noreply, State};
handle_cast({report_pressures, Address}, State) ->
    Measurements = lists:reverse(State#state.pressures),
    {Datetime,_} = lists:nth(1, Measurements),
    report_dispatch(pressure, 
		    Measurements, 
		    Address, 
		    format_subject( Datetime, 
				    "pressure"),
		    ?PRESSURES_ATTACHMENT_FILE),
    {noreply, State};
handle_cast({reset_parameters, Filename}, State) ->
    cottage_alarm:reset_parameters(State#state.alarm_pid, 
				   Filename),
    cottage_beacon:reset_parameters(State#state.beacon_pid, 
				    Filename).



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
    NewTemps_list = update_list(Temps_list,
				{Datetime, round(Temp ,2)}, 
				1440),

    Pressures_list = State#state.pressures,
    NewPressures_list = update_list(Pressures_list,
				    {Datetime, round(Pressure ,2)}, 
				    1440),


    erlang:send_after(?MEASUREMENT_INTERVAL, self(),take_measurement),

    Alarm_PID = case State#state.alarm_pid of
		    0 -> erlang:whereis(cottage_alarm);
		    _Other ->  State#state.alarm_pid
		end,

    cottage_alarm:process_measurement(Alarm_PID, {Datetime, Temp}),

    Beacon_PID = case State#state.beacon_pid of
		    0 -> erlang:whereis(cottage_beacon);
		    _Thing ->  State#state.beacon_pid
		end,

    cottage_beacon:post_data(Beacon_PID, Temp, Pressure),

    %% Check if we should produce a daily report because a new day started

    send_daily_reports(?DEFAULT_ADDRESS, State, Datetime, State#state.temps),

    NewState = State#state{temps = NewTemps_list, 
			   pressures = NewPressures_list,
			   alarm_pid = Alarm_PID},

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
	    io:format(F, '"date","amount"~n',[]),
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

    io:format(F, "~b-~2..0b-~2..0b  ~2..0b:~2..0b:~2..0b,~9.2f~n",[Year, Month, Day,Hour, Minute,Second, Temp]),
    write_CSV(item, F, List).


%% adds a new value to the beginning of a reversed list and trims to maximum length
%%
update_list(List,NewVal, Length) ->
    lists:sublist( [NewVal | List], Length).

%% deliver results via email

%%email(To, Title, Content) ->
%%
email(To, Title, Content, Plot_Attachment_path, Data_Attachment_path) ->
    ok = file:write_file(?TMPFILE, Content),
    case 
	os:cmd(lists:concat(["mutt -s \"", Title,"\" -a ",Plot_Attachment_path, " -a ",Data_Attachment_path, " --  ", To," < ", ?TMPFILE, "\n"])) of
	[] ->
	    ok;
	error ->
	    error
    end.

report_dispatch(temperature, Measurements, Address, Subject, Attachment_location) ->
    write_CSV( Measurements, 
	       Attachment_location),
    clear_certain_files("temperature-plot"),
    Priv_dir = code:priv_dir(cottage_watcher),
    _Stuff = os:cmd(lists:concat([Priv_dir,
				  "/graphscript_temp.R ", 
				  Attachment_location])),
    %% Get the files in the directory
    {ok, Files} = file:list_dir("./"),
    %% filter out any files that aren't a line plot
    Graphs = lists:filter(fun (X) -> string:left(X, 16) == "temperature-plot"  end,
			  Files),
    Plot_Attachment = case length(Graphs) of
			  1 -> Graphs;
			  _Other_number -> []
		      end,
    Min = min_measurement(Measurements),
    Max = max_measurement(Measurements),
    Email_content = io_lib:format("The lowest termperature was ~9.2f~nThe highest was ~9.2f~n",[Min, Max]),

    email( Address, 
	   Subject, 
	   Email_content, 
	   Plot_Attachment, 
	   Attachment_location),
    file:delete(Plot_Attachment);

report_dispatch(pressure, Measurements, Address, Subject, Attachment_location) ->
    write_CSV( Measurements, 
	       Attachment_location),
    clear_certain_files("pressure-plot"),
    Priv_dir = code:priv_dir(cottage_watcher),
    _Stuff = os:cmd(lists:concat([Priv_dir,
				  "/graphscript_pressure.R ", 
				  Attachment_location])),
    %% Get the files in the directory
    {ok, Files} = file:list_dir("./"),
    %% filter out any files that aren't a line plot
    Graphs = lists:filter(fun (X) -> string:left(X, 13) == "pressure-plot"  end,
			  Files),
    Plot_Attachment = case length(Graphs) of
			  1 -> Graphs;
			  _Other_number -> []
		      end,
    Min = min_measurement(Measurements),
    Max = max_measurement(Measurements),
    Email_content = io_lib:format("The lowest pressure was ~9.2f~nThe highest was ~9.2f~n",[Min, Max]),

    email( Address, 
	   Subject, 

	   Email_content,
	   Plot_Attachment,
	   Attachment_location),
    file:delete(Plot_Attachment).

send_daily_reports( Address , State, Datetime, Temp_list) when Temp_list /= [] ->
    {{_,_,Todays_day},_} = Datetime,
    {{{_,_,Last_day},_},_} =  lists:nth(1, Temp_list),
    case Todays_day - Last_day of
	0 ->
	    ok;
	_Difference_found ->

	    Measurements = State#state.temps,
	    report_dispatch(temperature, 
			    Measurements, 
			    Address, 
			    format_subject( Datetime, 
					    "temperature"),
			    ?TEMPS_ATTACHMENT_FILE),

	    Pressures = State#state.pressures,
	    report_dispatch(pressure, 
			    Pressures, 
			    Address, 
			    format_subject( Datetime, 
					    "pressure"),
			    ?PRESSURES_ATTACHMENT_FILE)
    end;
send_daily_reports( _Address , _State, _Datetime, _Temp_list) ->
    ok.

format_subject(Datetime, Type) ->
    lists:concat([ Type, " data for ",format_date(Datetime)]).

format_date(Datetime) ->
    {{Year, Month, Day},_} = Datetime,
    io_lib:format("~b-~2..0b-~2..0b",[Year, Month, Day]).



min_measurement(List) ->
    JustData = strip_down(List),
    extreme(JustData, fun(X,Smallest) -> if X < Smallest -> X; true -> Smallest end end).

max_measurement(List) ->
    JustData = strip_down(List),
    extreme(JustData, fun(X,Smallest) -> if X < Smallest -> Smallest; true -> X end end).

avg_measurement(List) ->
    JustData = strip_down(List),
    lists:foldl(fun( X, Sum) -> X + Sum end, 0, JustData) / length(JustData).


%% takes a list of {{{date},{time}},measurement} and turns it into a list of measurements
strip_down(List) ->
    lists:map(fun(X) ->
		      {_,Y} = X, Y end,List).

%% make certain that there aren't any lingering plot files of a certain type

clear_certain_files(String) ->
    %% Get the files in the directory
    {ok, Files} = file:list_dir("./"),
    %% filter out any files that aren't a line plot
    case lists:filter(fun (X) -> string:left(X, 16) == String  end,
		      Files) of
	[] ->
	    ok;
	List ->
	    lists:map(fun(X) ->
			      file:delete(X) end, List),
	    ok
    end.
%%
%% Tests
%%
strip_down_test() -> [1, 2, 3] = strip_down( [ {"aa",1}, 
					       {abc, 2}, 
					       {{{12,13,2015},{13,16,55}},3} ] ).


avg_measurement_test() ->
    A =  [
	  {{{12,13,2015},{13,16,55}},1},
	  {{{12,13,2015},{13,16,55}},2},
	  {{{12,13,2015},{13,16,55}},3} ],
    2.0 = avg_measurement(A).

max_measurement_test() ->
    A =  [
	  {{{12,13,2015},{13,16,55}},1},
	  {{{12,13,2015},{13,16,55}},2},
	  {{{12,13,2015},{13,16,55}},3} ],
    3 = max_measurement(A).

min_measurement_test() ->
    A =  [
	  {{{12,13,2015},{13,16,55}},1},
	  {{{12,13,2015},{13,16,55}},2},
	  {{{12,13,2015},{13,16,55}},3} ],
    1 = min_measurement(A).

format_date_test() ->
    "2016-02-13" = lists:flatten(format_date({{2016,2,13},{13,13,13}})).

format_subject_test() ->
    "Pressure data for 2015-12-15" = lists:flatten(format_subject( {{2015,12,15}, {8,12,56}}, "Pressure")).

update_list_test() ->
    [4,3,2] = update_list( [3,2,1],4,3).

round_1_test() ->
    3.15 = round(3.145623, 2).

round_2_test() ->
    3.0 = round(3.145623, 0).
