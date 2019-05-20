%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jan 2019 19:20
%%%-------------------------------------------------------------------
-module(maxwell_master_frontend_mgr).

%% API
-export([
  start_link/0,
  add_and_monitor/1,
%%  add/1,
%%  remove/1,
%%  clear/0,
  fetch/0,
  add_listener/1,
  delete_listener/1
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  maxwell_master_server_mgr:start_link(?SERVER).

add_and_monitor(Key) ->
  maxwell_master_server_mgr:add_and_monitor(?SERVER, Key).

%%add(Key) ->
%%  maxwell_master_server_mgr:add(?SERVER, Key).
%%
%%remove(Key) ->
%%  maxwell_master_server_mgr:remove(?SERVER, Key).
%%
%%clear() ->
%%  maxwell_master_server_mgr:clear(?SERVER).

fetch() ->
  maxwell_master_server_mgr:fetch(?SERVER).

add_listener(ListenerPid) ->
  maxwell_master_server_mgr:add_listener(?SERVER, ListenerPid).

delete_listener(ListenerPid) ->
  maxwell_master_server_mgr:delete_listener(?SERVER, ListenerPid).