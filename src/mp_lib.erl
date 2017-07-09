-module(mp_lib).

%% API
-export([server_id/1, unknown_msg/2]).

server_id(ID) ->
  list_to_atom([$P | integer_to_list(ID)]).

%%--------------------------------------------------------------------

-spec(unknown_msg(Type :: call | cast | info | term(), Msg :: any()) -> ok).
unknown_msg(Type, Msg) ->
  error_logger:warning_msg("received unknown ~p: ~p~n", [Type, Msg]).