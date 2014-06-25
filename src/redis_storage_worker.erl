%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc gen_server process for listening to casts and calls from
%% redis_storage_exchange
%% @end

-module(redis_storage_worker).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% -------------------------
% Worker Startup
% -------------------------

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  rabbit_log:info("starting redis-storage-exchange worker.~n"),
  register(?MODULE, self()),
  {ok, dict:new()}.

% -------------------------
% Gen Server Implementation
% -------------------------

code_change(_, State, _) ->
  {ok, State}.

handle_call({delete, X, _Bs}, _From, State) ->
  {ok, NewState} = redis_storage_lib:close(X, State),
  {reply, ok, NewState};

handle_call({route, Exchange, Delivery}, _From, State) ->
  case redis_storage_lib:process_message(Exchange, Delivery, State) of
    {ok, NewState} -> {reply, ok, NewState};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call({validate, X}, _From, State) ->
  case redis_storage_lib:validate_connection(X) of
    ok             -> {reply, ok, State};
    {error, Error} -> {reply, {error, Error}, State}
  end;

handle_call(_Msg, _From, State) ->
  {noreply, unknown_command, State}.

handle_cast(Cast, State) ->
  rabbit_log:error("redis_storage_worker unknown_cast: ~p, ~p~n", [Cast, State]),
  {noreply, State}.

handle_info(Message, State) ->
  rabbit_log:error("redis_storage_worker unknown_info: ~p~n", Message),
  {noreply, State}.

terminate(_,_) ->
  ok.
