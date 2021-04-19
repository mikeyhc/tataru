-module(plugin_server).
-behaviour(gen_server).

-export([start_link/0, broadcast/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {plugins=[]}).

%% API functions

-spec start_link() -> any().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec broadcast(pid(), #{any() => any()}) -> ok.
broadcast(Pid, Msg) ->
    gen_server:cast(Pid, {broadcast, Msg}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({broadcast, Msg}, State) ->
    logger:info("got broadcast ~p", [Msg]),
    say_hi(Msg),
    {noreply, State}.

%% helper functions

say_hi(#{<<"channel_id">> := ChannelId,
         <<"author">> := #{<<"id">> := UserId}}) ->
    ApiServer = discord_sup:get_api_server(),
    Msg = <<"<@!", UserId/binary, "> Ta-Ta-Tataru!">>,
    discord_api:send_message(ApiServer, ChannelId, Msg).
