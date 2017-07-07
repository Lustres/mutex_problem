%%%-------------------------------------------------------------------
%% @doc mutex_problem processes supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mp_processes_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: pos_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore).
init([]) ->

  ProcessSpec = {process,
                 {mp_process, start_link, []},
                 permanent,
                 100,
                 worker,
                 [mp_process]},

  {ok, { {simple_one_for_one, 0, 1}, [ProcessSpec]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
