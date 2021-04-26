-module(plugin_sup).
-behaviour(supervisor).

-export([start_link/0, start_plugin/1, start_plugin/2, stop_plugin/2]).
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_plugin(Module) ->
    start_plugin(Module, []).

start_plugin(Module, Args) ->
    supervisor:start_child(plugin_sup, [{Module, install, Args}]).

stop_plugin(Module, Args) ->
    apply(Module, uninstall, [Args]).

%% supervisor callbacks
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 20},
    ChildSpecs = [#{id => plugin_wrapper,
                    restart => transient,
                    start => {plugin_wrapper, install, []}}],
    {ok, {SupFlags, ChildSpecs}}.
