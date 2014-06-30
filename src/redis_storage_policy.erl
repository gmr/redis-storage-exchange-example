%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc define the runtime parameters and validators to setup policies
%% @end

-module(redis_storage_policy).

-behaviour(rabbit_policy_validator).

-export([register/0,
         unregister/0,
         validate_policy/1]).

-define(POLICY_PARAMETERS,
        [{policy_validator,  <<"redis-storage-host">>},
         {policy_validator,  <<"redis-storage-port">>}]).

-rabbit_boot_step({?MODULE,
                   [{description, "Redis Storage Exchange Policy Validation"},
                    {mfa, {?MODULE, register, []}},
                    {requires, rabbit_registry},
                    {cleanup, {?MODULE, unregister, []}},
                    {enables, recovery}]}).

register() ->
  [rabbit_registry:register(Class, Name, ?MODULE) ||
    {Class, Name} <- ?POLICY_PARAMETERS],
  ok.

unregister() ->
  [rabbit_registry:unregister(Class, Name) ||
    {Class, Name} <- ?POLICY_PARAMETERS],
  ok.

validate_policy(KeyList) ->
  Host = proplists:get_value(<<"redis-storage-host">>, KeyList, none),
  Port = proplists:get_value(<<"redis-storage-port">>, KeyList, none),
  case gen_server:call(redis_storage_worker, {validate_policy, Host, Port}) of
    ok -> ok;
    {error, Error} ->
      {error,
       "Could not connect to Redis at ~s port ~p: ~p", 
       [Host, Port, Error]}
  end.
