-module(mp_res_test).

-include("mp_test_header.hrl").


%%====================================================================
%% Test fixtures
%%====================================================================

legal_test_() ->
  ?setup(fun(P) -> {inorder, [
           is_started(P),
           use_resource(P),
           use_resource(P)
         ]} end).

illegal_test_() ->
  ?foreach([fun empty_release/1,
            fun acquire_twice_by_self/1,
            fun acquire_by_other/1,
            fun release_twice/1,
            fun release_by_other/1]).

%%====================================================================
%% Test functions
%%====================================================================

is_started({ResPid, _}) ->
  [?_assert(erlang:is_process_alive(ResPid)),
   ?_assertEqual(ResPid, erlang:whereis(mp_res))].

use_resource({_, Serv}) ->
  Name = mp_test_server:reg(Serv),

  R1 = mp_res:acquire(Serv),

  Owner1 = mp_res:owner(),

  R2 = mp_res:release(Serv),

  Owner2 = mp_res:owner(),

  [?_assertEqual(ok, R1),
   ?_assertEqual(Name, Owner1),
   ?_assertEqual(ok, R2),
   ?_assertEqual(undefined, Owner2)].

empty_release({_, Serv}) ->
  undefined = mp_res:owner(),

  R = mp_res:release(Serv),

  Owner = mp_res:owner(),

  [?_assertEqual(already_released, R),
   ?_assertEqual(undefined, Owner)].

acquire_by_other({_, Serv}) ->
  Name = mp_test_server:reg(Serv),
  %% acquire first time
  ok = mp_res:acquire(Serv),

  Name = mp_res:owner(),

  {ok, Pid} = mp_test_server:start_link(),
  mp_test_server:reg(Pid),
  %% acquire by another
  R = mp_res:acquire(Pid),

  Owner = mp_res:owner(),

  gen_server:stop(Pid),

  [?_assertEqual(Name, Owner),
   ?_assertEqual({busy, Name}, R)].

acquire_twice_by_self({_, Serv}) ->
  Name = mp_test_server:reg(Serv),
  %% acquire first time
  ok = mp_res:acquire(Serv),

  Name = mp_res:owner(),

  %% acquire by self
  R1 = mp_res:acquire(Serv),

  Owner1 = mp_res:owner(),

  %% rename
  NewName = mp_test_server:reg(Serv),

  %% acquire by new self
  R2 = mp_res:acquire(Serv),

  Owner2 = mp_res:owner(),

  [?_assertEqual({busy, Name}, R1),
   ?_assertEqual(Name, Owner1),

   ?_assertEqual({busy, NewName}, R2),
   ?_assertEqual(NewName, Owner2)].

release_twice({_, Serv}) ->
  mp_test_server:reg(Serv),
  ok = mp_res:acquire(Serv),
  ok = mp_res:release(Serv),
  undefined = mp_res:owner(),

  %% release by self
  R1 = mp_res:release(Serv),
  Owner1 = mp_res:owner(),

  {ok, Pid} = mp_test_server:start_link(),
  mp_test_server:reg(Pid),

  %% release by another
  R2 = mp_res:release(Serv),
  Owner2 = mp_res:owner(),

  gen_server:stop(Pid),

  [?_assertEqual(already_released, R1),
   ?_assertEqual(undefined, Owner1),

   ?_assertEqual(already_released, R2),
   ?_assertEqual(undefined, Owner2)].

release_by_other({_, Serv}) ->
  mp_test_server:reg(Serv),
  ok = mp_res:acquire(Serv),

  Name = mp_res:owner(),
  {ok, Pid} = mp_test_server:start_link(),

  R = mp_res:release(Pid),
  Owner = mp_res:owner(),

  [?_assertEqual(Name, Owner),
   ?_assertEqual({busy, Name}, R)].

%%====================================================================
%% Internal functions
%%====================================================================

start() ->
  {ok, ResPid} = mp_res:start_link(),
  {ok, TestPid} = mp_test_server:start_link(),
  {ResPid, TestPid}.

stop({_, TestPid}) ->
  gen_server:stop(mp_res),
  gen_server:stop(TestPid).
