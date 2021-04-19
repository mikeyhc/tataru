-module(discord_api).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-export([start_link/2, get_gateway/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(connection, {pid :: pid(),
                     ref :: reference()
                    }).
-record(state, {url :: string(),
                token :: string(),
                connection :: #connection{} | undefined
               }).

%% API functions

-spec start_link(string(), string()) -> any().
start_link(Url, Token) ->
    gen_server:start_link(?MODULE, [Url, Token], []).

-spec get_gateway(pid()) -> binary().
get_gateway(Pid) ->
    gen_server:call(Pid, get_gateway).

%% gen_server callbacks

init([Url, Token]) ->
    gen_server:cast(self(), connect),
    {ok, #state{url=Url, token=Token}}.

handle_call(get_gateway, _From, State) ->
    ?LOG_INFO("actually called"),
    #state{token=Token, connection=#connection{pid=ConnPid}} = State,
    Auth = "Bot " ++ Token,
    StreamRef = gun:get(ConnPid, "/api/gateway/bot",
                        [{<<"authorization">>, Auth}]),
    ?LOG_INFO("did a get"),
    #{<<"url">> := Url} = jsone:decode(read_body(ConnPid, StreamRef)),
    {reply, Url, State}.

handle_cast(connect, S=#state{url=Url}) ->
    {ok, ConnPid} = gun:open(Url, 443),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    Conn = #connection{pid=ConnPid, ref=MRef},
    {noreply, S#state{connection=Conn}}.

handle_info({gun_down, ConnPid, _, _, _},
            S=#state{connection=#connection{pid=ConnPid}}) ->
    ?LOG_INFO("gun lost api connection"),
    gun:await_up(ConnPid),
    ?LOG_INFO("gun regained api connection"),
    {noreply, S};
handle_info({'DOWN', MRef, process, ConnPid, Reason},
            S=#state{connection=#connection{pid=ConnPid, ref=MRef}}) ->
    ?LOG_ERROR("discord api disconnected ~p~n", [Reason]),
    {stop, disconnected, S}.

%% helper functions

read_body(ConnPid, StreamRef) ->
    % TODO handle non-200 status
    ?LOG_INFO("reading body (~p:~p)", [ConnPid, StreamRef]),
    receive
        {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
            no_data;
        {gun_response, ConnPid, StreamRef, nofin, 200, _Headers} ->
            receive_data(ConnPid, StreamRef, <<>>)
    after 1000 -> throw(timeout)
    end.

receive_data(ConnPid, StreamRef, Acc) ->
    ?LOG_INFO("receiving data"),
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_data(ConnPid, StreamRef, <<Acc/binary, Data/binary>>);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            <<Acc/binary, Data/binary>>
    after 1000 -> throw(timeout)
    end.

