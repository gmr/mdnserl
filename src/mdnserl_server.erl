%%%-------------------------------------------------------------------
%% @doc mDNS server providing both announcement and discovery
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(mdnserl_server).

-behavior(gen_server).

-export([start_link/0,
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-include_lib("kernel/src/inet_dns.hrl").

-define(DEFAULT_TTL, 120).

-record(state, {announcement, discovery, records}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([term()]) -> term().
init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, Announcement} = mdnserl_udp:announcement_socket(),
    {ok, Discovery} = mdnserl_udp:discovery_socket(),
    {ok, #state{announcement=Announcement, discovery=Discovery, records=[]}}.

handle_call(records, _From, State) ->
    E = current_epoch(),
    Recs = [R || R <- State#state.records, proplists:get_value(expires, R, E + 1) > E],
    {reply, {ok, [maps:from_list(R) || R <- Recs]}, State#state{records=Recs}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, Socket, _IP, _InPort, Packet}, State) ->
    {ok, Record} = inet_dns:decode(Packet),
    Records = update_records(Record, State#state.records),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{records=Records}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.announcement),
    gen_udp:close(State#state.discovery),
    net_kernel:monitor_nodes(false),
    ok.

code_change(_OldVsn, _Vsn, State) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_records(list(), #dns_rec{}) -> list().
update_records(RecordIn, Current) ->
    Records = [inet_dns:rr(RR) || RR <- inet_dns:msg(RecordIn, arlist)],
    merge_records(Records, Current).

merge_records([], Out) -> Out;
merge_records([H|T], Records) ->
    merge_records(T, merge_record(H, Records, [], nomatch)).

merge_record(_, [], Out, match) -> Out;
merge_record(N, [], Out, nomatch) ->
    lists:append(Out, [add_time_info(N)]);
merge_record(N, [H|T], Out, Match) ->
    {R, M} = case record_match(N, H) of
        true  -> {add_time_info(N), match};
        false -> {H, Match}
    end,
    merge_record(N, T, lists:append(Out, [R]), M).

record_match(N, C) ->
    record_match(proplists:get_value(domain, N),
                 proplists:get_value(type, N),
                 proplists:get_value(domain, C),
                 proplists:get_value(type, C)).

record_match(ND, NT, CD, CT) when ND == CD, NT == CT -> true;
record_match(_, _, _, _) -> false.

add_time_info(R) ->
    E = current_epoch(),
    lists:append(R, [{ts, E}, {expires, E + proplists:get_value(ttl, R, ?DEFAULT_TTL)}]).

current_epoch() ->
    to_epoch(calendar:now_to_datetime(erlang:timestamp())).

to_epoch(V) ->
    calendar:datetime_to_gregorian_seconds(V) - 62167219200.
