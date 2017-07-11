%%%-------------------------------------------------------------------
%% @doc mutex_problem processes
%% @end
%%%-------------------------------------------------------------------
-module(mp_process).

-behaviour(gen_server).

%% API
-export([start_link/1, require/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-type time_stamp() :: {pos_integer(), pos_integer()}.

-record(state, {id    :: pos_integer(),
                state :: ready | {wait, non_neg_integer()} | busy,
                time  :: non_neg_integer(),
                queue :: ordsets:ordset(time_stamp())}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ID :: pos_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID) ->
  gen_server:start_link({local, mp_lib:server_id(ID)}, ?MODULE, [ID], []).

%%--------------------------------------------------------------------
%% @doc
%% Let server send resource request with using time
%%
%% @end
%%--------------------------------------------------------------------
-spec(require(ID :: pos_integer(), Duration :: non_neg_integer())
      -> ok | {wait, ResOwner :: pos_integer()} | owned).
require(ID, Duration) ->
  ServName = mp_lib:server_id(ID),
  gen_server:call(ServName, {require, Duration}).

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
  {ok, #state{id = ID,
              state = ready,
              time = 0,
              queue = ordsets:new()}}.

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
handle_call({require, TimeStamp = {Time, _ID}}, _From, S) ->
  Queue = ordsets:add_element(TimeStamp, S#state.queue),
  {reply, {S#state.time, S#state.id}, S#state{time = tick(S#state.time, Time),
                                              queue = Queue}};

%%--------------------------------------------------------------------

handle_call({require, Duration}, _From,
            S = #state{state = ready, id = ID, time = Time}) when is_integer(Duration) ->

  Queue = ordsets:add_element({Time, ID}, S#state.queue),

  Msg = {require, {Time, ID}},
  R = broad_call(get_children(mp_processes_sup, other), Msg),

  OtherTimes = lists:max(erlang:element(1, lists:unzip(R))),

  {reply, {wait, mp_res:owner()}, S#state{state = {wait, Duration},
                                          time = tick(Time, OtherTimes),
                                          queue = Queue}, 0};

handle_call({require, _Duration}, _From, S = #state{state = {wait, _D}}) ->
  {reply, {wait, mp_res:owner()}, S};

handle_call({require, _Duration}, _From, S = #state{state = busy}) ->
  {reply, owned, S};

%%--------------------------------------------------------------------

handle_call({release, {Time, ID}}, _From, S) ->
  Queue = ordsets:filter(fun({_, I}) when I == ID -> false;
                            (_)       -> true
                         end, S#state.queue),

  {reply, ok, S#state{queue = Queue, time = tick(Time)}, 0};

%%--------------------------------------------------------------------

handle_call(release, _From, S = #state{state = busy, id = ID, time = Time}) ->

  ok = mp_res:release(self()),

  Queue = ordsets:filter(fun({_, I}) when I == ID -> false;
                            (_)       -> true
                         end, S#state.queue),
  Msg = {release, {Time, ID}},
  broad_call(get_children(mp_processes_sup, other), Msg),

  {reply, ok, S#state{state = ready, queue = Queue, time = tick(Time)}};

handle_call(release, _From, S = #state{state = ready}) ->
  {reply, empty, S};

handle_call(release, _From, S = #state{state = {wait, _D}}) ->
  {reply, {wait, mp_res:owner()}, S};

%%--------------------------------------------------------------------

handle_call(Request, _From, State) ->
  mp_lib:unknown_msg(call, Request),
  {reply, ok, State}.

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
handle_info(timeout, S = #state{state = {wait, Duration}}) ->
  State = case is_candidate(S) of
      true -> ok = mp_res:acquire(self()),
              if Duration /= 0 ->
                  timer:apply_after(Duration, gen_server, call, [self(), release])
              end,
              S#state{state = busy};

      false -> S
  end,
  {noreply, State};

handle_info(timeout, State) ->
  {noreply, State};

%%--------------------------------------------------------------------

handle_info(Info, State) ->
  mp_lib:unknown_msg(info, Info),
  {noreply, State}.

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
handle_cast(Request, State) ->
  mp_lib:unknown_msg(cast, Request),
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
%% @private
%% @doc
%% Tick 1s
%%
%% @end
%%--------------------------------------------------------------------
-spec(tick(Time :: non_neg_integer()) -> pos_integer()).
tick(Time) ->
  Time + 1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tick aligned to other time
%%
%% @end
%%--------------------------------------------------------------------
-spec(tick(Time :: non_neg_integer(), OtherTime :: non_neg_integer()) -> pos_integer()).
tick(Time, OtherTime) ->
  erlang:max(tick(Time), OtherTime).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decide whether granted resource
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_candidate(State :: #state{}) -> boolean()).
is_candidate(#state{state = {wait, _}, id = ID, queue = [{_, ID} | _]}) ->
  true;

is_candidate(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call all server in given list one by one
%%
%% @end
%%--------------------------------------------------------------------
-spec(broad_call(Servers :: gen_server:server_ref(), Request :: term()) -> [Reply :: term()]).
broad_call(Servers, Request) ->
  [gen_server:call(Ref, Request) || Ref <- Servers].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get children of supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_children(SupRef :: supervisor:sup_ref(), Options :: all | other) -> [term()]).

get_children(SupRef, all) ->
  [Pid || {_, Pid, _, _} <- supervisor:which_children(SupRef)];

get_children(SupRef, other) ->
  lists:delete(self(), get_children(SupRef, all)).
