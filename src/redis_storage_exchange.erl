%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc redis-storage-exchange example plugin
%% @end
-module(redis_storage_exchange).

-behaviour(rabbit_exchange_type).

-export([add_binding/3,
         assert_args_equivalence/2,
         create/2,
         description/0,
         delete/3,
         policy_changed/2,
         recover/2,
         route/2,
         remove_bindings/3,
         serialise_events/0,
         validate/1,
         validate_binding/2]).

-define(X_TYPE, <<"x-redis-storage">>).
-define(X_DESC, <<"Redis Storage Exchange">>).

-rabbit_boot_step({?MODULE,
                  [{description, ?X_DESC},
                   {mfa,         {rabbit_registry, register,
                                  [exchange, ?X_TYPE, ?MODULE]}},
                   {requires,    rabbit_registry},
                   {cleanup,     {rabbit_registry, unregister,
                                  [exchange, ?X_TYPE]}},
                   {enables,     recovery}]}).

-include_lib("rabbit_common/include/rabbit.hrl").

add_binding(Tx, X, B) ->
  rabbit_exchange_type_topic:add_binding(Tx, X, B).

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).

create(Tx, X) ->
  rabbit_exchange_type_topic:create(Tx, X).

delete(Tx, X, Bs) ->
  gen_server_call({delete, X, Bs}),
  rabbit_exchange_type_topic:delete(Tx, X, Bs).

description() ->
  [{name, ?X_TYPE}, {description, ?X_DESC}].

policy_changed(OldX, _NewX) ->
  gen_server_call({delete, X, []}),
  ok.

recover(Tx, X) ->
  rabbit_exchange_type_topic:recover(Tx, X).

remove_bindings(Tx, X, Bs) ->
  rabbit_exchange_type_topic:remove_bindings(Tx, X, Bs).

route(X, Delivery) ->
  case gen_server_call({route, X, Delivery}) of
    ok ->
      rabbit_exchange_type_topic:route(X, Delivery)
  end.

serialise_events() ->
  false.

validate(X) ->
  gen_server_call({validate, X}).

validate_binding(_X, _B) ->
  ok.

%% @private
%% @spec get_server_call(Args) -> Reply
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Reply        = ok|{error, Reason}
%% @doc Wrap the gen_server:call behavior to shutdown the channel with an
%%      exception if an error bubbles up from the worker.
%% @end
%%
gen_server_call(Args) ->
  case gen_server:call(redis_storage_worker, Args) of
    ok -> ok;
    {error, Reason} ->
      rabbit_misc:protocol_error(resource_error,
                                 "redis_storage_worker failure (~p)",
                                 [Reason]),
      error
  end.
