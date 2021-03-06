-module(mp_process_test).

-include_lib("eunit/include/eunit.hrl").

-type time_stamp() :: {pos_integer(), pos_integer()}.

-record(state, {id    :: pos_integer(),
                state :: ready | wait | busy,
                time  :: non_neg_integer(),
                queue :: ordsets:ordset(time_stamp())}).

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

candidate_test_() ->
  {foreachx, fun candidate_ans/1, fun(_, _) -> ok end, [
    {{wait,   1, []},       fun candidate_assert/2},
    {{wait,   2, [{0, 2}]}, fun candidate_assert/2},
    {{wait,   3, [{3, 0}]}, fun candidate_assert/2},

    {{ready,  4, []},       fun candidate_assert/2},
    {{ready,  5, [{0, 5}]}, fun candidate_assert/2},
    {{ready,  6, [{6, 0}]}, fun candidate_assert/2},

    {{busy,   7, []},       fun candidate_assert/2},
    {{busy,   8, [{0, 8}]}, fun candidate_assert/2},
    {{busy,   9, [{9, 0}]}, fun candidate_assert/2}
  ]}.

%%====================================================================
%% Test functions
%%====================================================================

is_started({ID, Pid}) ->
  R = is_process_alive(Pid),
  Name = list_to_atom([$P | integer_to_list(ID)]),
  ?_assert((R) and (Pid =:= whereis(Name))).

tick_time(Time) ->
  NewTime = mp_process:tick(Time),
  ?_assertEqual(Time + 1, NewTime).

tick_with_other(Time) ->
  Other = rand_another_time(Time),
  NewTime = mp_process:tick(Time, Other),
  ?_assertEqual(max(Time + 1, Other), NewTime).

candidate_assert({wait, ID, Queue}, Ans) ->
  R = mp_process:is_candidate(#state{state = {wait, 0}, id = ID, queue = Queue}),
  ?_assertEqual(Ans, R);

candidate_assert({State, ID, Queue}, Ans) ->
  R = mp_process:is_candidate(#state{state = State, id = ID, queue = Queue}),
  ?_assertEqual(Ans, R).

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

candidate_ans({wait, ID, [{_, ID} | _]}) -> true;
candidate_ans(_) -> false.

unique_id() ->
  erlang:abs(erlang:unique_integer()).

rand_another_time(T) ->
  T + erlang:trunc(rand:uniform() * 6).
