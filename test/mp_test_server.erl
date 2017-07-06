-module(mp_test_server).

-behaviour(gen_server).

%% API
-export([start_link/0, name/1, reg/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------

name(Pid) ->
  gen_server:call(Pid, name).

%%--------------------------------------------------------------------

reg(Pid) ->
  gen_server:call(Pid, reg).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{name = undefined}}.

%%--------------------------------------------------------------------

handle_call(name, _From, S) ->
  {reply, S#state.name, S};

handle_call(reg, _From, S = #state{name = undefined}) ->
  Name = list_to_atom("mp_test_id" ++ integer_to_list(erlang:unique_integer())),
  true = register(Name, self()),
  handle_call(name, _From, S#state{name = Name});

handle_call(reg, _From, S) ->
  handle_call(unreg, _From, S),
  handle_call(reg, _From, #state{name = undefined});

handle_call(unreg, _From, S = #state{name = Name})  ->
  case whereis(Name) of
    undefined -> ok;
    _Pid -> unregister(Name)
  end,
  {reply, ok, S#state{name = undefined}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
