-module(mutex_problem_test).

-include("mp_test_header.hrl").


%%====================================================================
%% Test fixtures
%%====================================================================

start_test_() ->
  ?setup(fun is_started/1).

stable_test_() ->
  ?setup(fun(_) -> test_gen(5) end).

%%====================================================================
%% Test functions
%%====================================================================

is_started(_) ->
  R = application:ensure_started(mutex_problem),
  ?_assertEqual(ok, R).

test_gen(N) ->
  {generator,
    fun() ->
      if N > 0 ->
        [?_test(no_crash()) | test_gen(N - 1)];
        true -> []
      end
    end}.

no_crash() ->
  timer:sleep(1000),
  ok = application:ensure_started(mutex_problem),
  case coin() of
    false -> mutex_problem_app:require(rand_id());
    true -> mutex_problem_app:release(rand_id())
  end.


%%====================================================================
%% Internal functions
%%====================================================================

start() ->
  application:start(mutex_problem).

stop(_) ->
    application:stop(mutex_problem).

coin() ->
  rand:uniform(2) =:= 1.

rand_id() ->
  {ok, ProcessCount} = application:get_env(mutex_problem, process_count),
  rand:uniform(ProcessCount).

