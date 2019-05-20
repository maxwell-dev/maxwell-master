%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2019 18:36
%%%-------------------------------------------------------------------
-module(maxwell_master_route_mgr).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  add/2,
  remove/2,
  replace/2,
  dump_without/1
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
-define(ON_DISCONNECTED_CMD(Key), {'$on_disconnected', Key}).

-record(state, {reversed_routes}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Type, Endpoint) ->
  gen_server:call(?SERVER, {add, Type, Endpoint}).

remove(Type, Endpoint) ->
  gen_server:call(?SERVER, {remove, Type, Endpoint}).

replace(Types, Endpoint) ->
  gen_server:call(?SERVER, {replace, Types, Endpoint}).

dump_without(Endpoint) ->
  gen_server:call(?SERVER, {dump_without, Endpoint}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  maxwell_master_frontend_mgr:add_listener(self()),
  {ok, #state{reversed_routes = dict:new()}}.

handle_call({add, Type, Endpoint}, _From, State) ->
  lager:debug("Adding route: ~p -> ~p", [Type, Endpoint]),
  reply(add2(Type, Endpoint, State));
handle_call({remove, Type, Endpoint}, _From, State) ->
  lager:debug("Removing route: ~p -> ~p", [Type, Endpoint]),
  reply(remove2(Type, Endpoint, State));
handle_call({replace, Types, Endpoint}, _From, State) ->
  lager:debug("Replacing routes: ~p -> ~p", [Types, Endpoint]),
  reply(replace2(Types, Endpoint, State));
handle_call({dump_without, Endpoint}, _From, State) ->
  reply(dump_without2(Endpoint, State));
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(?ON_DISCONNECTED_CMD(Key), State) ->
  lager:debug("ON_DISCONNECTED_CMD: key: ~p", [Key]),
  {noreply, on_disconnected(Key, State)};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add2(Type, Endpoint, State) ->
  {ok, State#state{
    reversed_routes = add3(Type, Endpoint, State#state.reversed_routes)
  }}.

add3(Type, Endpoint, ReversedRoutes) ->
  case dict:find(Endpoint, ReversedRoutes) of
    {ok, Types} ->
      dict:store(Endpoint, sets:add_element(Type, Types), ReversedRoutes);
    error ->
      dict:store(Endpoint, sets:add_element(Type, sets:new()), ReversedRoutes)
  end.

remove2(Type, Endpoint, State) ->
  {ok, State#state{
    reversed_routes = remove3(Type, Endpoint, State#state.reversed_routes)
  }}.

remove3(Type, Endpoint, ReversedRoutes) ->
  case dict:find(Endpoint, ReversedRoutes) of
    {ok, Types} ->
      dict:store(
        Endpoint, sets:del_element(Type, Types), ReversedRoutes
      );
    error -> ReversedRoutes
  end.

replace2(Types, Endpoint, State) ->
  {ok, State#state{
    reversed_routes = dict:store(
      Endpoint, sets:from_list(Types), State#state.reversed_routes
    )
  }}.

dump_without2(Endpoint, State) ->
  {to_list(
    reverse(
      remove_by_endpoint(
        Endpoint, State#state.reversed_routes
      )
    )
  ), State}.

to_list(RervesedRoutes) ->
  dict:fold(
    fun(Type, Endpoints, Routes) ->
      dict:store(Type, sets:to_list(Endpoints), Routes)
    end,
    dict:new(),
    RervesedRoutes
  ).

reverse(RervesedRoutes) ->
  dict:fold(
    fun(Endpoint, Types, Routes) ->
      sets:fold(
        fun(Type, Routes2) ->
          case dict:find(Type, Routes2) of
            {ok, Endpoints} ->
              dict:store(Type, sets:add_element(Endpoint, Endpoints), Routes2);
            error ->
              dict:store(Type, sets:add_element(Endpoint, sets:new()), Routes2)
          end
        end, Routes, Types
      )
    end, dict:new(), RervesedRoutes
  ).

remove_by_endpoint(Endpoint, RervesedRoutes) ->
  dict:erase(Endpoint, RervesedRoutes).

on_disconnected({_, PrivateIp, Port}, State) ->
  ReversedRoutes = remove_by_endpoint(
    build_endpoint(PrivateIp, Port), State#state.reversed_routes
  ),
  State#state{reversed_routes = ReversedRoutes}.

build_endpoint(Ip, Port) ->
  lists:concat([binary_to_list(Ip), ":", binary_to_list(Port)]).

reply({Reply, State}) ->
  {reply, Reply, State}.

%%noreply(State) ->
%%  {noreply, State}.