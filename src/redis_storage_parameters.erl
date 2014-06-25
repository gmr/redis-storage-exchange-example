%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc define the runtime parameters and validators to setup policies
%% @end

-module(redis_storage_parameters).

-behaviour(rabbit_policy_validator).

-export([register/0,
         unregister/0,
         validate_policy/1]).

-define(RUNTIME_PARAMETERS,
        [{policy_validator,  <<"redis-storage-host">>},
         {policy_validator,  <<"redis-storage-port">>}]).

-rabbit_boot_step({?MODULE,
                   [{description, "Redis Storage Exchange Parameter Validation"},
                    {mfa, {?MODULE, register, []}},
                    {requires, rabbit_registry},
                    {cleanup, {?MODULE, unregister, []}},
                    {enables, recovery}]}).

register() ->
  [rabbit_registry:register(Class, Name, ?MODULE) ||
    {Class, Name} <- ?RUNTIME_PARAMETERS],
  ok.

unregister() ->
  [rabbit_registry:unregister(Class, Name) ||
    {Class, Name} <- ?RUNTIME_PARAMETERS],
  ok.

validate_policy(KeyList) ->
  Host       = proplists:get_value(<<"redis-storage-host">>, KeyList, none),
  Port       = proplists:get_value(<<"redis-storage-port">>, KeyList, none),
  Validation = [redis_storage_lib:validate_host(Host),
                redis_storage_lib:validate_port(Port)],
  case Validation of
    [ok, ok]                   -> ok;
    [{error, Error, Args}, _]  -> {error, Error, Args};
    [ok, {error, Error, Args}] -> {error, Error, Args}
  end.
