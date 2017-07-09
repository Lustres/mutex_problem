-module(mutex_problem_test).

-include("mp_test_header.hrl").


%%====================================================================
%% Test fixtures
%%====================================================================

start_test_() ->
  ?setup(fun is_started/1).

%%====================================================================
%% Test functions
%%====================================================================

is_started(_) ->
  ?_test(ensure_started()).

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
