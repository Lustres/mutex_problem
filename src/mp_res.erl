%%%-------------------------------------------------------------------
%% @doc mutex_problem resource server
%% @end
%%%-------------------------------------------------------------------
-module(mp_res).

-behaviour(gen_server).

%% API
-export([start_link/0, owner/0, owner/1, acquire/1, release/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {owner :: pid() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Get resource owner
%%
%% @end
%%--------------------------------------------------------------------
-spec(owner() -> Name :: atom()).
owner() ->
  gen_server:call(?SERVER, owner).

%%--------------------------------------------------------------------
%% @doc
%% Get pid of resource owner
%%
%% @end
%%--------------------------------------------------------------------
-spec(owner(pid) -> pid()).
owner(pid) ->
  gen_server:call(?SERVER, {owner, pid}).

%%--------------------------------------------------------------------
%% @doc
%% acquire resource if available
%%
%% @end
%%--------------------------------------------------------------------
-spec(acquire(Process :: pid()) -> ok | {busy, Name :: atom()}).
acquire(Pid) ->
  gen_server:call(?SERVER, {acquire, Pid}).

%%--------------------------------------------------------------------
%% @doc
%% release owned resource
%%
%% @end
%%--------------------------------------------------------------------
-spec(release(Process :: pid()) -> ok | {busy, Name :: atom()} | already_released).
release(Pid) ->
  gen_server:call(?SERVER, {release, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{owner = undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(owner, _From, S = #state{owner = Owner}) ->
  {reply, get_name(Owner), S};

%%--------------------------------------------------------------------

handle_call({owner, pid}, _From, S = #state{owner = Owner}) ->
  {reply, Owner, S};

%%--------------------------------------------------------------------

handle_call({acquire, Pid}, _From, S = #state{owner = Owner}) when Owner =:= undefined->
  error_logger:info_msg("res required: ~p~n", [get_name(Pid)]),
  {reply, ok, S#state{owner = Pid}};

handle_call({acquire, _Pid}, _From, S)->
  {reply, {busy, get_name(S#state.owner)}, S};

%%--------------------------------------------------------------------

handle_call({release, Pid}, _From, S = #state{owner = Owner}) when Owner =:= Pid ->
  error_logger:info_msg("res released: ~p~n", [get_name(Pid)]),
  {reply, ok, S#state{owner = undefined}};

handle_call({release, _Pid}, _From, S = #state{owner = Owner}) when is_pid(Owner) ->
  {reply, {busy, get_name(Owner)}, S};

handle_call({release, _Pid}, _From, S) ->
  {reply, already_released, S};

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_name(Pid) when is_pid(Pid) ->
  [{registered_name, Name}] = erlang:process_info(Pid, [registered_name]),
  case Name of
    [] -> undefined;
    _  -> Name
  end;

get_name(_Pid) -> undefined.
