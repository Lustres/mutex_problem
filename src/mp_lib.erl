-module(mp_lib).

%% API
-export([server_id/1]).

server_id(ID) ->
  list_to_atom([$P | integer_to_list(ID)]).
