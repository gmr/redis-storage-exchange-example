%%==============================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @end
%%==============================================================================

%% @doc Abstract away the validation and message processing from the worker
%% @end

-module(redis_storage_lib).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([close/2,
         process_message/3,
         validate_connection/1,
         validate_host/1,
         validate_port/1]).

-define(DEFAULT_HOST,     <<"localhost">>).
-define(DEFAULT_PORT,     6379).

%% @spec close(X, State) -> Result
%% @where
%%       X      = rabbit_types:exchange()
%%       State  = dict()
%%       Result = {ok, dict()}
%% @doc Close an open connection to redis when an exchange is delete
%% @end
%%
close(X, State) ->
  {Host, Port} = get_connection_args(X),
  case dict:find({Host, Port}, State) of
    {ok, C} ->
      reddy_conn:close(C),
      {ok, dict:erase({Host, Port}, State)};
    error ->
      {ok, State}
  end.

%% @spec process_message(X, D, State) -> Result
%% @where
%%       X      = rabbit_types:exchange()
%%       D      = rabbit_types:delivery()
%%       State  = dict()
%%       Result = {ok, dict()}|{error, Error}
%% @doc Process an inbound delivery, setting it in redis, connecting if
%%      not already connected
%% @end
%%
process_message(X,
                #delivery{message=#basic_message{routing_keys=Keys,
                                                 content=Content}},
                State) ->
  case get_connection(X, State) of
    {ok, C, NewState} ->
      Key = list_to_binary(lists:append([binary_to_list(K) || K <- Keys])),
      case reddy_strings:set(C, Key, get_payload(Content)) of
        ok    -> {ok, NewState};
        Error -> {error, Error}
      end;
    {error, Error} -> {error, Error}
  end.

%% @spec validate_connection(X) -> Result
%% @where
%%       Value  = rabbit_type:exchange()
%%       Result = ok|{error, Error}
%% @doc Validate the connection information for the exchange
%% @end
%%
validate_connection(X) ->
  {Host, Port} = get_connection_args(X),
  case reddy_conn:connect(Host, Port) of
    {ok, C} ->
      reddy_conn:close(C),
      ok;
    {error, Error} -> {error, Error}
  end.

%% @spec validate_host(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified hostname is a binary or none
%% @end
%%
validate_host(Value) ->
  validate_binary_or_none("redis-host", Value).

%% @spec validate_port(Value) -> Result
%% @where
%%       Value  = integer()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified port is an integer value or none
%% @end
%%
validate_port(Value) ->
  validate_number_or_none("redis-port", Value).

%% ---------------
%% Private Methods
%% ---------------

%% @private
%% @spec get_connection(X, State) -> Result
%% @where
%%       X       = rabbit_types:exchange()
%%       State   = dict()
%%       Result  = {ok, pid}|{error, Error}
%% @doc Retrieve a Redis connection from the State dict if present, otherwise
%%      connect and add it to the State dict
%% @end
%%
get_connection(X, State) ->
  {Host, Port} = get_connection_args(X),
  case dict:find({Host, Port}, State) of
    {ok, C} -> {C, State};
    error ->
      case reddy_conn:connect(Host, Port) of
        {ok, C}        -> {ok, C, dict:store({Host, Port}, C, State)};
        {error, Error} -> {error, Error}
      end
  end.

%% @private
%% @spec get_env(EnvVar, DefaultValue) -> Value
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Return the environment variable defined for listen returning the
%%      value if the variable is found, otherwise return the passed in default
%% @end
%%
get_env(EnvVar, DefaultValue) ->
  case application:get_env(listen, EnvVar) of
    undefined -> DefaultValue;
    {ok, V}   -> V
  end.

%% @private
%% @spec get_url(X) -> Result
%% @where
%%       X      = rabbit_types:exchange()
%%       Result = tuple()
%% @doc Return a tuple of Host, Port
%% @end
%%
get_connection_args(X) ->
  Host     = get_param(X, "host", ?DEFAULT_HOST),
  Port     = get_number(get_param(X, "port", ?DEFAULT_PORT)),
  {Host, Port}.

%% @private
%% @spec get_parm(X, Name, DefaultValue) -> Value
%% @where
%%       X            = rabbit_types:exchange()
%%       Name         = list()|atom()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Returns the configuration value for an exchange, first by checking to
%% see if a policy value is set for the exchange, then by checking arguments in
%% the exchange, then checking environment defined overrides (config), and
%% finally by returning the passed in default value
%% @end
%%
get_param(X, Name, DefaultValue) when is_atom(Name) ->
  get_param(X, atom_to_list(Name), DefaultValue);
get_param(X=#exchange{arguments=Args}, Name, DefaultValue) when is_list(Name) ->
  case rabbit_policy:get(list_to_binary("redis-" ++ Name), X) of
    undefined -> get_param_value(Args, Name, DefaultValue);
    Value     ->
      case is_binary(Value) of
        true  -> binary_to_list(Value);
        false -> Value
      end
  end.

%% @private
%% @spec get_param_env_value(Name, DefaultValue) -> Value
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Return the value specified in the config/environment for the passed in
%% key Name, returning DefaultValue if it's not specified
%% @end
%%
get_param_env_value(Name, DefaultValue ) ->
  get_env(list_to_atom(Name), DefaultValue).

%% @private
%% @spec get_param_list_value(Value) -> list()
%% @where
%%       DefaultValue = binary()|integer()|list()
%% @doc Cast Value to a list if it is binary or an integer
%% @end
%%
get_param_list_value(Value) when is_binary(Value) ->
  binary_to_list(Value);
get_param_list_value(Value) when is_integer(Value) ->
  integer_to_list(Value);
get_param_list_value(Value) when is_list(Value) ->
  Value.

%% @private
%% @spec get_param_value(Args, Name, DefaultValue) -> Value
%% @where
%%       Args         = rabbit_framing:amqp_table()
%%       Name         = list()
%%       DefaultValue = binary()|integer()|list()
%% @doc Return the value of Name from the Args table, falling back to returning
%% the configuration specified env value, or the DefaultValue if it not present
%% in either Args or the config environment.
%% @end
%%
get_param_value(Args, Name, DefaultValue) ->
  case lists:keyfind(list_to_binary("x-" ++ Name), 1, Args) of
    {_, _, V} -> get_param_list_value(V);
            _ -> get_param_list_value(get_param_env_value(Name, DefaultValue))
  end.

%% @private
%% @spec get_payload(Value) -> list()
%% @where
%%       Value = tuple()#content
%% @doc Extract the reverse list of binary payload segments and order it
%%      correctly, converting the binary to list to return the full message
%%      body as a list.
%% @end
%%
get_payload(#content{payload_fragments_rev=Payload}) ->
  lists:append(lists:reverse([binary_to_list(V) || V <- Payload])).

%% @private
%% @spec get_number(Value) -> integer()
%% @where
%%       Value = list()|integer()|none
%% @doc Return the value passed in as an integer if it is a list anf the value
%% if it is an integer
%% @end
%%
get_number(Value) when is_list(Value) ->
  list_to_integer(Value);
get_number(Value) when is_number(Value) ->
  Value.

%% @private
%% @spec validate_binary_or_none(Name, Value) -> Result
%% @doc Validate the user specified value is a binary() or none
%% @where
%%       Name   = list()
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @end
%%
validate_binary_or_none(_, none) -> ok;
validate_binary_or_none(_, Value) when is_binary(Value) -> ok;
validate_binary_or_none(Name, Value) ->
  {error, "~s should be binary, actually was ~p", [Name, Value]}.

%% @private
%% @spec validate_binary_or_none(Name, Value) -> Result
%% @doc Validate the user specified value is a number() or none
%% @where
%%       Name   = list()
%%       Value  = number()|none
%%       Result = ok|{error, Error}
%% @end
%%
validate_number_or_none(_, none) -> ok;
validate_number_or_none(_, Value) when is_number(Value) -> ok;
validate_number_or_none(Name, Value) ->
  {error, "~s should be a number, actually was ~p", [Name, Value]}.
