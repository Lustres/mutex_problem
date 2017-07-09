%%%-------------------------------------------------------------------
%% @doc mutex_problem top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mutex_problem_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ResourceSpec   = {mp_res,
                      {mp_res, start_link, []},
                      permanent,
                      100,
                      worker,
                      [mp_res]},

    ProcessSupSpec = {processes_sup,
                      {mp_processes_sup, start_link, []},
                      permanent,
                      2000,
                      supervisor,
                      [mp_process_sup]},

    {ok, { {one_for_all, 0, 1}, [ResourceSpec, ProcessSupSpec]} }.
