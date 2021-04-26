-module(plugin_handler).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-export([install/0, handle/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-export([add_plugin/3, remove_plugin/2]).

-define(KNOWN_PLUGINS, [scheduler_plugin]).

-record(state, {authorized_users,
                known_plugins=?KNOWN_PLUGINS}).

%% API functions

install() ->
    AuthUsers = parse_envvar("TATARU_AUTHORIZED"),
    DefaultPlugins = parse_envvar("TATARU_PLUGINS"),
    gen_server:start_link(?MODULE, [AuthUsers, DefaultPlugins], []).

handle(Pid, {msg, Msg}) ->
    gen_server:cast(Pid, {msg, Msg});
handle(_Pid, {react, _}) ->
    ok.

%% gen_server callbacks

init([AuthUsers, DefaultPlugins]) ->
    lists:foreach(fun(P) -> gen_server:cast(self(), {default, P}) end,
                  DefaultPlugins),
    {ok, #state{authorized_users=AuthUsers}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({msg, Msg}, State=#state{authorized_users=AuthUsers}) ->
    #{<<"content">> := Content} = Msg,
    Parts = binary:split(Content, <<" ">>, [global, trim_all]),
    case Parts of
        [_, <<"plugin">>, <<"add">>, Rest] ->
            authorized_command(add_plugin, AuthUsers, Msg, [Rest, Msg, State]);
        [_, <<"plugin">>, <<"remove">>, Rest] ->
            authorized_command(remove_plugin, AuthUsers, Msg, [Rest, Msg]);
        [_, <<"plugin">>, <<"list">>] ->
            list_plugins(Msg);
        _ -> ok
    end,
    {noreply, State};
handle_cast({default, Plugin}, State) ->
    {ok, APlugin} = atom_plugin(Plugin),
    PluginServer = tataru_sup:get_plugin_server(),
    ok = plugin_server:add_plugin(PluginServer, APlugin),
    {noreply, State}.

%% helper functions

authorized_command(Fn, AuthUsers, Msg, Args) ->
    #{<<"author">> := #{<<"id">> := AuthorId}} = Msg,
    case lists:member(AuthorId, AuthUsers) of
        true -> apply(?MODULE, Fn, Args);
        false ->
            send_reply(<<"you are not authorized for that command">>, Msg)
    end.

send_reply(Reply, Message) ->
    #{<<"channel_id">> := ChannelId,
      <<"author">> := #{<<"id">> := AuthorId}} = Message,
    ApiServer = discord_sup:get_api_server(),
    R = <<"<@!", AuthorId/binary, "> ", Reply/binary>>,
    discord_api:send_message(ApiServer, ChannelId, R).

atom_plugin(Plugin) ->
    try
        PluginStr = binary:bin_to_list(Plugin),
        {ok, list_to_existing_atom(PluginStr)}
    catch
        error:badarg -> {error, unknown_argument}
    end.

add_plugin(Plugin, Msg, State) ->
    case atom_plugin(Plugin) of
        {ok, PluginAtom} -> handle_add(PluginAtom, Plugin, Msg, State);
        {error, unknown_argument} ->
            send_reply(<<"unknown plugin: ", Plugin/binary>>, Msg)
    end.

handle_add(Plugin, PluginBin, Msg, State) ->
    case lists:member(Plugin, State#state.known_plugins) of
        false ->
            send_reply(<<"unknown plugin: ", PluginBin/binary>>, Msg);
        true ->
            PluginServer = tataru_sup:get_plugin_server(),
            case plugin_server:add_plugin(PluginServer, Plugin) of
                ok ->
                    send_reply(<<"plugin ", PluginBin/binary,  " added">>, Msg);
                {error, already_installed} ->
                    Reply = <<"plugin ", PluginBin/binary,
                              " already installed">>,
                    send_reply(Reply, Msg)
            end
    end.

remove_plugin(<<"plugin_handler">>, Msg) ->
    send_reply(<<"I'm afraid I can't do that, Dave">>, Msg);
remove_plugin(Plugin, Msg) ->
    case atom_plugin(Plugin) of
        {ok, PluginAtom} -> handle_remove(PluginAtom, Plugin, Msg);
        {error, unknown_argument} ->
            send_reply(<<"unknown plugin: ", Plugin/binary>>, Msg)
    end.

handle_remove(Plugin, PluginBin, Msg) ->
    PluginServer = tataru_sup:get_plugin_server(),
    case plugin_server:remove_plugin(PluginServer, Plugin) of
        ok ->
            send_reply(<<"plugin ", PluginBin/binary,  " removed">>, Msg);
        {error, not_installed} ->
            Reply = <<"plugin ", PluginBin/binary,
                      " not installed">>,
            send_reply(Reply, Msg)
    end.

binjoin(A, B) -> <<A/binary, B/binary>>.

list_plugins(Msg) ->
    PluginServer = tataru_sup:get_plugin_server(),
    {ok, Plugins} = plugin_server:list_plugins(PluginServer),
    Fn = fun(X) -> binary:list_to_bin(atom_to_list(X)) end,
    BinPlugins = lists:map(Fn, Plugins),
    PluginBin = lists:foldl(fun binjoin/2, <<>>,
                            lists:join(<<", ">>, BinPlugins)),
    send_reply(<<"Installed: ", PluginBin/binary>>, Msg).

parse_envvar(EnvVar) ->
    lists:map(fun binary:list_to_bin/1,
              lists:filter(fun(X) -> X =/= "" end,
                           string:split(os:getenv(EnvVar, ""), ";"))).
