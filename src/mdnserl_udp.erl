%%%-------------------------------------------------------------------
%% @doc mDNS UDP utilities
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(mdnserl_udp).

-export([announcement_socket/0,
         discovery_socket/0,
         make_header/0]).

-include_lib("kernel/src/inet_dns.hrl").

-define(MDNS_ADDR, {224, 0, 0, 251}).
-define(MDNS_PORT, 5353).

-spec announcement_socket() -> inet:socket().
%% @spec announcement_socket() -> inet:socket()
%% @doc Return an UDP socket for sending mDNS announcements
%% @end
%%
announcement_socket() ->
    gen_udp:open(0, [binary]).

-spec discovery_socket() -> inet:socket().
%% @spec discovery_socket() -> inet:socket()
%% @doc Return an UDP socket for receiving mDNS announcements
%% @end
%%
discovery_socket() ->
    gen_udp:open(
        ?MDNS_PORT,
        [
            {active, true},
            {broadcast, true},
            {add_membership, {?MDNS_ADDR, {0, 0, 0, 0}}},
            {ip, ?MDNS_ADDR},
            {mode, binary},
            {multicast_if, {0, 0, 0, 0}},
            {multicast_loop, true},
            {multicast_ttl, 255},
            {reuseaddr, true}
        ]).

-spec make_header() -> #dns_header{}.
%% @spec make_header() -> #dns_header{}
%% @doc Return a DNS header record
%% @end
%%
make_header() ->
    inet_dns:make_header(
        [{id, 0},
         {qr, true},
         {opcode, query},
         {aa, true},
         {tc, false},
         {rd, false},
         {ra, false},
         {pr, false},
         {rcode, 0}]).
