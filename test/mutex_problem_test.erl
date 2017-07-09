-module(mutex_problem_test).

-include("mp_test_header.hrl").


%%====================================================================
%% Test fixtures
%%====================================================================

start_test_() ->
  ?setup(fun is_started/1).

stable_test_() ->
  ?setup(fun(_) -> no_crash(100) end).

%%====================================================================
%% Test functions
%%====================================================================

is_started(_) ->
  ?_test(ensure_started()).

no_crash(N) ->
  {generator,
    fun() ->
      if N > 0 ->
        [?_test(rand_api()) | no_crash(N - 1)];
        true -> []
      end
    end}.

rand_api() ->
  timer:sleep(500),
  ensure_started(),
  case coin() of
    false -> mutex_problem_app:require(rand_id()), ?debugMsg("require");
    true -> mutex_problem_app:release(rand_id()), ?debugMsg("release")
  end.


%%====================================================================
%% Internal functions
%%====================================================================

start() ->
  application:start(mutex_problem).

stop(_) ->
    application:stop(mutex_problem).

ensure_started() ->
  L = application:which_applications(),
  {mutex_problem, _, _} = lists:keyfind(mutex_problem, 1, L).

coin() ->
  rand:uniform(2) =:= 1.

rand_id() ->
  {ok, ProcessCount} = application:get_env(mutex_problem, process_count),
  rand:uniform(ProcessCount).


