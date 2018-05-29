%%%-------------------------------------------------------------------
%% @doc mdnserl public API
%% @end
%%%-------------------------------------------------------------------

-module(mdnserl).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         records/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mdnserl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

records() ->
    {ok, Records} = gen_server:call(mdnserl_server, records),
    Records.
