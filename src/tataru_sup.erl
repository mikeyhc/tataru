-module(tataru_sup).
-behaviour(supervisor).

-export([start_link/2, get_plugin_server/0]).
-export([init/1]).

%% API functions

start_link(DiscordUrl, DiscordToken) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [DiscordUrl, DiscordToken]).

get_plugin_server() ->
    Children = supervisor:which_children(?MODULE),
    {_, Pid, _, _} = lists:keyfind(plugin_server, 1, Children),
    Pid.

%% supervisor callbacks

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([DiscordUrl, DiscordToken]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => discord_sup,
                    start => {discord_sup, start_link,
                              [DiscordUrl, DiscordToken]},
                    type => supervisor},
                  #{id => plugin_server,
                    start => {plugin_server, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
