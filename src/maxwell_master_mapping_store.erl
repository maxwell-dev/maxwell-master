%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2018 8:56 PM
%%%-------------------------------------------------------------------
-module(maxwell_master_mapping_store).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  add/2,
  get/1
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

-define(SERVER, ?MODULE).
-record(state, {db_ref}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Topic, Key) ->
  gen_server:call(?SERVER, {add, Topic, Key}).

get(Topic) ->
  gen_server:call(?SERVER, {get, Topic}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, DbRef} = open_db(),
  lager:info("Initializing ~p: db_ref: ~p", [?MODULE, DbRef]),
  {ok, #state{db_ref = DbRef}}.

handle_call({add, Topic, Key}, _From, State) ->
  reply({add2(Topic, Key, State), State});
handle_call({get, Topic}, _From, State) ->
  reply({get2(Topic, State), State});
handle_call(Request, _From, State) ->
  lager:error("Received unknown call: ~p", [Request]),
  reply({ok, State}).

handle_cast(Request, State) ->
  lager:error("Received unknown cast: ~p", [Request]),
  noreply(State).

handle_info(Info, State) ->
  lager:error("Received unknown info: ~p", [Info]),
  noreply(State).

terminate(Reason, State) ->
  lager:info(
    "Terminating ~p: db_ref: ~p, reason: ~p",
    [?MODULE, State#state.db_ref, Reason]
  ),
  lists:foreach(
    fun(DbRef) ->
      maxwell_store_db:close(DbRef)
    end, State#state.db_ref),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_db() ->
  DbDir = maxwell_master_config:get_mapping_store_dir(),
  ok = filelib:ensure_dir(DbDir),
  Spec = get_db_spec(),
  Result = {ok, _} = rocksdb:open(DbDir, Spec),
  lager:info("Mapping store opened: dir: ~p, spec: ~p", [DbDir, Spec]),
  Result.

get_db_spec() ->
  [
    {create_if_missing, true},
    {prefix_extractor, {fixed_prefix_transform, 4}},
    {max_open_files, -1},
    {use_fsync, false},
    {bytes_per_sync, 8388608},
    {table_cache_numshardbits, 6},
    {write_buffer_size, 268435456},
    {max_write_buffer_number, 4},
    {min_write_buffer_number_to_merge, 2},
    {target_file_size_base, 1073741824},
    {level0_slowdown_writes_trigger, 1024},
    {level0_stop_writes_trigger, 800},
    {compaction_style, universal},
    {max_background_compactions, get_concurrency_count() div 2},
    {max_background_flushes, get_concurrency_count() div 2},
    {total_threads, get_concurrency_count()}
  ].

get_concurrency_count() ->
  case erlang:system_info(schedulers_online) of
    undefined ->
      lager:warning("Failed to read system info: schedulers_online"),
      8;
    Any ->
      Any
  end.

add2(Topic, {PublicIp, PrivateIp, Port}, State) ->
  Key = <<PublicIp/binary, ":", PrivateIp/binary, ":", Port/binary>>,
  rocksdb:put(State#state.db_ref, Topic, Key, []).

get2(Topic, State) ->
  case rocksdb:get(State#state.db_ref, Topic, []) of
    {ok, Key} ->
      Parts = binary:split(Key, <<":">>, [global]),
      {lists:nth(1, Parts), lists:nth(2, Parts), lists:nth(3, Parts)};
    Error -> 
      Error
  end.

reply({Reply, State}) ->
  {reply, Reply, State}.

noreply(State) ->
  {noreply, State}.
