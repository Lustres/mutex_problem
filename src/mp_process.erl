%%%-------------------------------------------------------------------
%% @doc mutex_problem top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(mp_process).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {queue :: [{non_neg_integer(), pos_integer()}],
                time  :: non_neg_integer(),
                id    :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ID :: non_neg_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID) ->
  gen_server:start_link({local, list_to_atom(io_lib:format("P~p", [ID]))}, ?MODULE, [ID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ID]) ->
  {ok, #state{time = 0, id = ID, queue = [{0, 1}]}}.

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
handle_call({require, Msg = {Time, _Id}}, From, S = #state{queue = Q}) ->
  gen_server:reply(From, S#state.time),
  {noreply, tick(S#state{queue = [Msg | Q]}, Time), 0};

%%--------------------------------------------------------------------

handle_call({release, {Time, ID}}, From, S = #state{queue = Q}) ->
  gen_server:reply(From, ok),
  NewS = S#state{queue = [X || X = {_, P} <- Q, P =/= ID]},
  {noreply, tick(NewS, Time), 0};

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
handle_cast(require, S) ->
  NewState = tick(S),
  Processes = get_processes(),
  Msg = {NewState#state.time, NewState#state.id},
  R = [gen_server:call(Pid, {require, Msg}) || Pid <- Processes],
  {noreply, tick(NewState, lists:max(R))};

%%--------------------------------------------------------------------

handle_cast(release, S) ->
  NewState = tick(S),
  Processes = get_processes(),
  Msg = {NewState#state.time, NewState#state.id},
  _ = [gen_server:call(Pid, {release, Msg}) || Pid <- Processes],
  {noreply, S};

%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, S) ->
  case winner(S#state.queue, S#state.id) of
    true  -> mp_res:acquire(self()),
             timer:apply_after(1000, gen_server, cast, [self(), S#state.id]);
    _ -> void
  end,
  {noreply, S};

%%--------------------------------------------------------------------

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
%% @spec terminate(Reason, State) -> void()
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
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
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

%%--------------------------------------------------------------------
%% @doc
%% Increase local time
%%
%% @spec tick(State) -> NewState
%% @end
%%--------------------------------------------------------------------
-spec(tick(State :: #state{}) -> NewState :: #state{}).
tick(S = #state{time = T}) ->
  S#state{time = T + 1}.

%%--------------------------------------------------------------------
%% @doc
%% Increase local to timestamp
%%
%% @spec tick(State, Timestamp) -> New
%% @end
%%--------------------------------------------------------------------
-spec(tick(State :: #state{}, Timestamp :: non_neg_integer()) -> NewState :: #state{}).
tick(S = #state{time = T}, Timestamp) ->
  S#state{time = max(T, Timestamp) + 1}.

%%--------------------------------------------------------------------
%% @doc
%% Get all processes
%%
%% @spec get_processes() -> [Process]
%% @end
%%--------------------------------------------------------------------
-spec(get_processes() -> [Process :: pid()]).
get_processes() ->
  Children = supervisor:which_children(mp_processes_sup),
  [element(2, C) || C <- Children].

%%--------------------------------------------------------------------
%% @doc
%% Select the winner
%%
%% @spec winner(Q, ID) -> boolean()
%% @end
%%--------------------------------------------------------------------
-spec(winner(Q :: [{non_neg_integer(), pos_integer()}], ID :: pos_integer()) -> boolean()).
%%winner(Q, ID) -> winner1(Q, ID) andalso winner2(Q, ID).

winner(Q, ID) ->
  case hd(Q) of
    {_, ID} -> message_count(Q);
    _ -> false
  end.

message_count(Q) ->
  {ok, ProcessCount} = application:get_env(mutex_problem, process_count),
  S = sets:from_list(element(2, lists:unzip(Q))),
  sets:size(S) =:= ProcessCount.
