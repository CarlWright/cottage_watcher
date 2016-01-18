%%%-------------------------------------------------------------------
%%% @author Carl A. Wright <wright@servicelevel.net>
%%% @copyright (C) 2015, Carl A. Wright
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2015 by Carl A. Wright <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(cottage_beacon).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([post_data/3,
	 reset_parameters/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("cottage_beacon.hrl").

-define(SERVER, ?MODULE).

-record(state, {url,
		priv_dir}).

%%%===================================================================
%%% API
%%%===================================================================

post_data( ServerRef, Temperature, Pressure) ->
    gen_server:cast(ServerRef, {send, Temperature, Pressure}).

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
%%    case inets:start() of
%%	ok -> ok;
%%	{error,{already_started, inets}} -> ok
%%    end,
    {ok,NewState}.

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
handle_cast({send, Temperature, Pressure}, State) ->
    post_http_data(Temperature,Pressure, State),
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

set_parameters( Source_file, State) ->
    Priv_dir = code:priv_dir(cottage_watcher),
    Filename = lists:concat([Priv_dir, 
			     "/",
			     Source_file]),
    {ok,Specs} = file:consult( Filename),
    URL = proplists:get_value(url, Specs),
    State#state{url = URL, 
		priv_dir = Priv_dir}.

post_http_data(Temperature, Pressure, State) ->
    URL = State#state.url,
    Datetime = format_datetime(erlang:localtime()),
    Encoded = io_lib:format("temp=~p&pressure=~p&datetime=~p",
			    [Temperature, Pressure, Datetime]),
    httpc:request(post,
		  {URL, [],
		   "application/x-www-form-urlencoded",
		   Encoded}, 
		  [], 
		  [{receiver,fun receiver_func/1},{sync, false}]).  

format_datetime(Datetime) ->
    {{Year, Month, Day},{Hour, Minute, Second}} = Datetime,
    io_lib:format("~b-~2..0b-~2..0b ~b:~2..0b:~2..0b",[Year, Month, Day,Hour, Minute, Second]).

receiver_func(HTTP_result) ->
    io:format("~p~n",[HTTP_result]),
    ok.
