%%%-------------------------------------------------------------------
%%% @author xuchaoqian
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jun 2018 6:13 PM
%%%-------------------------------------------------------------------
-module(maxwell_master_config).

%% API
-export([
  get_mapping_store_dir/0
]).

get_mapping_store_dir() ->
  {ok, DataDir} = application:get_env(maxwell_master, mapping_store_dir),
  string:strip(DataDir, right, $/).