-module(mp_process_test).

-include_lib("eunit/include/eunit.hrl").

-record(state, {queue :: ordsets:ordset({non_neg_integer(), pos_integer()}),
                time  :: non_neg_integer(),
                id    :: non_neg_integer()}).

%%====================================================================
%% Test fixtures
%%====================================================================

start_test_() ->
  {setup, fun start/0,
          fun stop/1,
          fun is_started/1}.

tick_test_() ->
  {foreach, fun() -> rand:uniform(100) end,
            fun(_) -> ok end,
            [fun tick_time/1,
             fun tick_with_other/1]}.

%%====================================================================
%% Test functions
%%====================================================================

is_started({ID, Pid}) ->
  R = is_process_alive(Pid),
  Name = list_to_atom([$P | integer_to_list(ID)]),
  ?_assert((R) and (Pid =:= whereis(Name))).

tick_time(Time) ->
  S = mp_process:tick(#state{time = Time}),
  ?_assertEqual(Time + 1, S#state.time).

tick_with_other(Time) ->
  Other = rand_another_time(Time),
  S = mp_process:tick(#state{time = Time}, Other),
  ?_assertEqual(max(Time, Other) + 1, S#state.time).

%%====================================================================
%% Internal functions
%%====================================================================

start() ->
  ID = unique_id(),
  {ok, Pid} = mp_process:start_link(ID),
  {ID, Pid}.

stop({_, Pid}) ->
  gen_server:stop(Pid).

get_state(Name) ->
  R = sys:get_state(Name),
  {status, _Pid, _Mod,
    [_Parent, _Status, _PPid, _LogEvents, [_Head, _Info, State]]} = R,
  {data,[{_State_Name, S}]} = State,
  S.

unique_id() ->
  erlang:abs(erlang:unique_integer()).

rand_another_time(T) ->
  T + erlang:floor(rand:uniform() * 6).
