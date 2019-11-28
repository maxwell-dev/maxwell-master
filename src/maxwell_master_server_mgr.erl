%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jan 2019 19:20
%%%-------------------------------------------------------------------
-module(maxwell_master_server_mgr).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  add_and_monitor/2,
%%  add/2,
%%  remove/2,
%%  clear/1,
  fetch/1,
  add_listener/2,
  delete_listener/2
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(ON_CONNECTED_CMD(Key), {'$on_connected', Key}).
-define(ON_DISCONNECTED_CMD(Key), {'$on_disconnected', Key}).

-record(state, {
  name, key_refs, ref_keys, current, listeners
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName], []).

add_and_monitor(ServerName, Key) ->
  gen_server:call(ServerName, {add_and_monitor, Key, self()}).

%%add(ServerName, Key) ->
%%  gen_server:call(ServerName, {add, Key}).
%%
%%remove(ServerName, Key) ->
%%  gen_server:call(ServerName, {remove, Key}).
%%
%%clear(ServerName) ->
%%  gen_server:call(ServerName, clear).

fetch(ServerName) ->
  gen_server:call(ServerName, fetch).

add_listener(ServerName, ListenerPid) ->
  gen_server:call(ServerName, {add_listener, ListenerPid}).

delete_listener(ServerName, ListenerPid) ->
  gen_server:call(ServerName, {delete_listener, ListenerPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ServerName]) ->
  State = init_state(ServerName),
  lager:info("Initializing ~p: state: ~p", [?MODULE, State]),
  {ok, State}.

handle_call({add_and_monitor, Key, Pid}, _From, State) ->
  lager:debug("Adding and monitoring server: ~p", [Key]),
  reply(add_and_monitor2(Key, Pid, State));
%%handle_call({add, Key}, _From, State) ->
%%  lager:debug("Adding server: ~p", [Key]),
%%  reply(add2(Key, State));
%%handle_call({remove, Key}, _From, State) ->
%%  lager:debug("Removing server: ~p", [Key]),
%%  reply(remove2(Key, State));
%%handle_call(clear, _From, State) ->
%%  lager:debug("Clearing all servers", []),
%%  reply(clear2(State));
handle_call(fetch, _From, State) ->
  reply(fetch2(State));
handle_call({add_listener, ListenerPid}, _From, State) ->
  reply({ok, add_listener2(ListenerPid, State)});
handle_call({delete_listener, ListenerPid}, _From, State) ->
  reply({ok, delete_listener2(ListenerPid, State)});
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason}, State) ->
  lager:info(
    "Server was lost: name: ~p, ref: ~p, reason: ~p",
    [State#state.name, Ref, Reason]
  ),
  noreply(on_server_lost(Ref, State));
handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  lager:info(
    "Terminating ~p: reason: ~p, state: ~p", [?MODULE, Reason, State]
  ),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(ServerName) ->
  #state{
    name = ServerName,
    key_refs = gb_trees:empty(),
    ref_keys = dict:new(),
    current = undefined,
    listeners = []
  }.

add_and_monitor2(Key, Pid, State) ->
  lager:info("!!!!!!!!!!!!!!!!!add_and_monitor2: ~p", [Key]),
  {_, _, State2} = remove_by_key(Key, State),
  Ref = monitor(process, Pid),
  State3 = State2#state{
    key_refs = gb_trees:insert(Key, Ref, State2#state.key_refs),
    ref_keys = dict:store(Ref, Key, State2#state.ref_keys)
  },
  {ok, notify_and_clear(?ON_CONNECTED_CMD(Key), State3)}.

remove_by_key(Key, State) ->
  case gb_trees:take_any(Key, State#state.key_refs) of
    {undefined, KeyRefs} -> State#state{key_refs = KeyRefs};
    {Ref, KeyRefs} ->
      demonitor(Ref),
      {Key, Ref, State#state{
        key_refs = KeyRefs,
        ref_keys = dict:erase(Ref, State#state.ref_keys)
      }};
    error -> {Key, undefined, State}
  end.

%%add2(Key, State) ->
%%  {_, _, State2} = remove_by_key(Key, State),
%%  {ok, State2#state{
%%    key_refs = gb_trees:insert(Key, undefined, State2#state.key_refs)
%%  }}.
%%
%%remove2(Key, State) ->
%%  {_, _, State2} = remove_by_key(Key, State),
%%  {ok, State2}.

%%clear2(State) ->
%%  Iter = gb_trees:iterator(State#state.key_refs),
%%  {ok, clear3(gb_trees:next(Iter), State)}.
%%
%%clear3({Key, Ref, Iter}, State) ->
%%  State2 = State#state{key_refs = gb_trees:delete_any(Key, State)},
%%  case Ref =/= undefined of
%%    true ->
%%      demonitor(Ref),
%%      clear3(
%%        gb_trees:next(Iter),
%%        State2#state{ref_keys = dict:erase(Ref, State2#state.ref_keys)}
%%      );
%%    false ->
%%      clear3(gb_trees:next(Iter), State2)
%%  end;
%%clear3(none, State) -> State.

fetch2(State) ->
  State2 = next(State),
  {State2#state.current, State2}.

next(State) ->
  case gb_trees:next(iter(State)) of
    {Key, _, _} -> State#state{current = Key};
    none -> State#state{current = undefined}
  end.

iter(State) -> %% return an iterator from current's next key
  Current = State#state.current,
  case Current =/= undefined of
    true ->
      Iter = gb_trees:iterator_from(Current, State#state.key_refs),
      case gb_trees:next(Iter) of
        %% current is still there
        {Current, _, Iter2} ->
          {Largest, _} = gb_trees:largest(State#state.key_refs),
          case Current =:= Largest of
            true -> gb_trees:iterator(State#state.key_refs);
            false -> Iter2
          end;
        %% current has been deleted
        {_, _, _} -> Iter;
        %% current is last element and has been deleted,
        %% so rewind the iterator
        none -> gb_trees:iterator(State#state.key_refs)
      end;
    false -> gb_trees:iterator(State#state.key_refs)
  end.

add_listener2(ListenerPid, State) ->
  case lists:member(ListenerPid, State#state.listeners) of
    true -> State;
    false ->
      State#state{
        listeners = lists:append(State#state.listeners, [ListenerPid])
      }
  end.

delete_listener2(ListenerPid, State) ->
  State#state{
    listeners = lists:delete(ListenerPid, State#state.listeners)
  }.

notify_and_clear(Msg, State) ->
  NewListeners = lists:filter(
    fun(ListenerPid) -> erlang:is_process_alive(ListenerPid) end,
    State#state.listeners
  ),
  lists:foreach(
    fun(ListenerPid) -> ListenerPid ! Msg end, NewListeners
  ),
  State#state{listeners = NewListeners}.

on_server_lost(Ref, State) ->
  case remove_by_ref(Ref, State) of
    {undefined, _, State2} -> State2;
    {Key, _, State2} -> notify_and_clear(?ON_DISCONNECTED_CMD(Key), State2)
  end.

remove_by_ref(Ref, State) ->
  demonitor(Ref),
  case dict:take(Ref, State#state.ref_keys) of
    {Key, RefKeys} ->
      {Key, Ref, State#state{
        key_refs = gb_trees:delete_any(Key, State#state.key_refs),
        ref_keys = RefKeys
      }};
    error -> {undefined, Ref, State}
  end.

reply({Reply, State}) ->
  {reply, Reply, State}.

noreply(State) ->
  {noreply, State}.