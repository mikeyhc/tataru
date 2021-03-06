-module(plugin_server).
-behaviour(gen_server).

-export([start_link/0, broadcast/2, broadcast_react/2]).
-export([add_plugin/2, remove_plugin/2, list_plugins/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {plugins=#{}}).

%% API functions

-spec start_link() -> any().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec broadcast(pid(), #{any() => any()}) -> ok.
broadcast(Pid, Msg) ->
    gen_server:cast(Pid, {broadcast, Msg}).

-spec broadcast_react(pid(), #{any() => any()}) -> ok.
broadcast_react(Pid, Msg) ->
    gen_server:cast(Pid, {broadcast_react, Msg}).

-spec add_plugin(pid(), atom()) -> ok | {error, already_installed}.
add_plugin(Pid, Plugin) ->
    gen_server:call(Pid, {add_plugin, Plugin}).

-spec remove_plugin(pid(), atom()) -> ok | {error, not_installed}.
remove_plugin(Pid, Plugin) ->
    gen_server:call(Pid, {remove_plugin, Plugin}).

-spec list_plugins(pid()) -> [atom()].
list_plugins(Pid) ->
    gen_server:call(Pid, list_plugins).

%% gen_server callbacks

init([]) ->
    gen_server:cast(self(), initialize),
    {ok, #state{}}.

handle_call({add_plugin, Name}, _From, S0) ->
    Plugins = S0#state.plugins,
    {R, State} = case maps:is_key(Name, Plugins) of
                     true -> {{error, already_installed}, S0};
                     false ->
                         {ok, Init} = plugin_sup:start_plugin(Name),
                         {ok, S0#state{plugins=Plugins#{Name => Init}}}
                 end,
    {reply, R, State};
handle_call({remove_plugin, Name}, _From, S0) ->
    Plugins = S0#state.plugins,
    {R, State} = case maps:is_key(Name, Plugins) of
                     false -> {{error, not_installed}, S0};
                     true ->
                         Init = maps:get(Name, Plugins),
                         plugin_sup:stop_plugin(Name, Init),
                         {ok, S0#state{plugins=maps:remove(Name, Plugins)}}
                 end,
    {reply, R, State};
handle_call(list_plugins, _From, State) ->
    {reply, {ok, maps:keys(State#state.plugins)}, State}.

handle_cast(initialize, State) ->
    {ok, Init} = plugin_sup:start_plugin(plugin_handler),
    {noreply, State#state{plugins=#{plugin_handler => Init}}};
handle_cast({broadcast, Msg}, State) ->
    lists:foreach(fun({M, V}) -> M:handle(V, {msg, Msg}) end,
                  maps:to_list(State#state.plugins)),
    {noreply, State};
handle_cast({broadcast_react, Msg}, State) ->
    lists:foreach(fun({M, V}) -> M:handle(V, {react, Msg}) end,
                  maps:to_list(State#state.plugins)),
    {noreply, State}.
