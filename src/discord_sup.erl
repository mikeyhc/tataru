-module(discord_sup).
-behaviour(supervisor).

-export([start_link/2, get_api_server/0]).
-export([init/1]).

%% API functions

start_link(DiscordUrl, DiscordToken) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [DiscordUrl, DiscordToken]).

get_api_server() ->
    Children = supervisor:which_children(?MODULE),
    {_, Pid, _, _} = lists:keyfind(discord_api, 1, Children),
    Pid.

%% supervisor callbacks

init([DiscordUrl, DiscordToken]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => discord_api,
                    start => {discord_api, start_link,
                              [DiscordUrl, DiscordToken]}},
                  #{id => discord_gateway,
                    start => {discord_gateway, start_link, [DiscordToken]}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.
