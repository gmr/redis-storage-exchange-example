%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc Supervisor for the Redis Storage Exchange worker
%% @end

-module(redis_storage_sup).

-behaviour(mirrored_supervisor).

-define(WORKER, redis_storage_worker).

-rabbit_boot_step({?MODULE,
                   [{description, "Redis Storage Worker Supervisor"},
                    {mfa,         {rabbit_sup, start_child, [?MODULE]}},
                    {requires,    kernel_ready},
                    {cleanup,     {?MODULE, stop, []}},
                    {enables,     recovery}]}).

-export([init/1, start_link/0, stop/0]).

-include_lib("rabbit_common/include/rabbit.hrl").

start_link() ->
   rabbit:maybe_insert_default_data(),
   mirrored_supervisor:start_link(
     {local, ?MODULE}, ?MODULE,
     fun rabbit_misc:execute_mnesia_transaction/1,
     ?MODULE, []).

stop() ->
    ok = mirrored_supervisor:terminate_child(?MODULE),
    ok = mirrored_supervisor:delete_child(?MODULE).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{?WORKER,
           {gen_server, start_link, [{global, ?WORKER}, ?WORKER, [], []]},
           permanent, ?MAX_WAIT, worker, [?WORKER]}]}}.
