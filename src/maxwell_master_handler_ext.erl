%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2018 5:35 PM
%%%-------------------------------------------------------------------
-module(maxwell_master_handler_ext).

-include_lib("maxwell_protocol/include/maxwell_protocol_pb.hrl").

-export([
  init/1,
  pre_pull/2,
  pre_push/2,
  handle/2,
  terminate/2
]).

-record(state, {
  peer_ip,
  server_port,
  handler_pid
}).

%%%===================================================================
%%% Server callbacks
%%%===================================================================
init(Req) ->
  HandlerPid = self(),
  State = #state{
    peer_ip = build_private_ip(Req),
    handler_pid = HandlerPid
  },
  lager:debug("Initializing handler_ext: state: ~p", [State]),
  State.

pre_pull(_Msg, _State) ->
  ok.
pre_push(_Msg, _State) ->
  ok.

%% frontend-master part
handle(#register_frontend_req_t{ref = Ref} = Req, State) ->
  Key = {_, _, Port} = build_key(Req, State),
  case verify_key(Key) of
    ok ->
      ok = maxwell_master_frontend_mgr:add_and_monitor(Key),
      Rep = #register_frontend_rep_t{ref = Ref},
      reply(Rep, State#state{server_port = Port});
    Error ->
      reply(build_error_rep(Error, Ref), State)
  end;
handle(#push_routes_req_t{types = Types, ref = Ref}, State) ->
  case State#state.server_port =/= undefined of
    true ->
      ok = maxwell_master_route_mgr:replace(
        Types,
        build_endpoint(State#state.peer_ip, State#state.server_port)
      ),
      Rep = #push_routes_rep_t{ref = Ref},
      reply(Rep, State);
    false ->
      reply(build_error_rep(not_registered_yet, Ref), State)
  end;
handle(#pull_routes_req_t{ref = Ref}, State) ->
  case State#state.server_port =/= undefined of
    true ->
      Routes = maxwell_master_route_mgr:dump_without(
        build_endpoint(State#state.peer_ip, State#state.server_port)
      ),
      reply(build_pull_routes_rep(Routes, Ref), State);
    false ->
      reply(build_error_rep(not_registered_yet, Ref), State)
  end;

%% backend-master part
handle(#register_backend_req_t{ref = Ref} = Req, State) ->
  Key = {_, _, Port} = build_key(Req, State),
  case verify_key(Key) of
    ok ->
      ok = maxwell_master_backend_mgr:add_and_monitor(Key),
      Rep = #register_backend_rep_t{ref = Ref},
      reply(Rep, State#state{server_port = Port});
    Error ->
      reply(build_error_rep(Error, Ref), State)
  end;

%% client-master part
handle(#resolve_frontend_req_t{ref = Ref}, State) ->
  Key = maxwell_master_frontend_mgr:fetch(),
  case Key =/= undefined of
    true ->
      Rep = #resolve_frontend_rep_t{
        endpoint = select_endpoint(Key, State), ref = Ref
      },
      reply(Rep, State);
    false ->
      reply(build_error_rep(frontend_not_found, Ref), State)
  end;
handle(#resolve_backend_req_t{ref = Ref}, State) ->
  Key = maxwell_master_backend_mgr:fetch(),
  case Key =/= undefined of
    true ->
      Rep = #resolve_backend_rep_t{
        endpoint = select_endpoint(Key, State), ref = Ref
      },
      reply(Rep, State);
    false ->
      reply(build_error_rep(backend_not_found, Ref), State)
  end.

terminate(_Reason, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_private_ip(Req) ->
  Endpoint = maps:get(endpoint, Req),
  [Ip, _] = parse_endpoint_parts(Endpoint),
  Ip.

build_endpoint(Ip, Port) ->
  lists:concat([binary_to_list(Ip), ":", binary_to_list(Port)]).

parse_endpoint_parts(Endpoint) -> binary:split(Endpoint, <<":">>).

build_key(#register_frontend_req_t{endpoint = Endpoint}, State) ->
  [PublicIp, Port] = parse_endpoint_parts(Endpoint),
  PrivateIp = State#state.peer_ip,
  {PublicIp, PrivateIp, Port};
build_key(#register_backend_req_t{endpoint = Endpoint}, State) ->
  [PublicIp, Port] = parse_endpoint_parts(Endpoint),
  PrivateIp = State#state.peer_ip,
  {PublicIp, PrivateIp, Port}.

verify_key({PublicIp, PrivateIp, _}) ->
  case verify_public_ip(PublicIp) of
    ok ->
      case verify_private_ip(PrivateIp) of
        ok -> ok;
        Error -> Error
      end;
    Error -> Error
  end.

verify_public_ip(Ip) ->
  case verify_private_ip(Ip) of
    ok -> {error, {not_public_ip, Ip}};
    {error, _} ->
      case Ip of
        <<"0.0.0.0">> ->
          {error, uninitialized_public_ip, Ip};
        _ -> ok
      end
  end.

verify_private_ip(Ip) ->
  case Ip of
    <<"127", _/binary>> -> ok;
    <<"10", _/binary>> -> ok;
    <<"172", _/binary>> -> ok;
    <<"192", _/binary>> -> ok;
    _ -> {error, {not_private_ip, Ip}}
  end.

select_endpoint(Key, State) ->
  {PublicIp, PrivateIp, Port} = Key,
  case State#state.peer_ip of
    <<"127", _/binary>> -> build_endpoint(PrivateIp, Port);
    <<"10", _/binary>> -> build_endpoint(PrivateIp, Port);
    <<"172", _/binary>> -> build_endpoint(PrivateIp, Port);
    <<"192", _/binary>> -> build_endpoint(PrivateIp, Port);
    _ -> build_endpoint(PublicIp, Port)
  end.

build_error_rep(Error, Ref) ->
  #error_rep_t{
    code = 1, desc = io_lib:format("~p", [Error]), ref = Ref
  }.

build_pull_routes_rep(Routes, Ref) ->
  #pull_routes_rep_t{route_groups = build_route_groups(Routes), ref = Ref}.

build_route_groups(Routes) ->
  dict:fold(
    fun(Type, Endpoints, RouteGroups) ->
      [
        #route_group_t{type = Type, endpoints = Endpoints} | RouteGroups
      ]
    end, [], Routes
  ).

reply(Reply, State) ->
  {reply, Reply, State}.

%%noreply(State) ->
%%  {noreply, State}.