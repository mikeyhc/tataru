-module(scheduler_plugin).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

-export([install/0, uninstall/1, handle/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(schedule_entry, {name :: binary(),
                         description :: binary()
                        }).

%% API functions

install() ->
    install_tables(),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

uninstall(Pid) ->
    gen_server:cast(Pid, stop).

handle(_Pid, Msg) ->
    ?LOG_INFO("scheduler message: ~p", [Msg]).

%% gen_server callbacks

init([]) ->
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

%% helper functions

install_tables() ->
    case lists:member(schedule_entry, mnesia:system_info(tables)) of
        true ->
            ?LOG_INFO("schedule_entry table already installed"),
            ok;
        false ->
            ?LOG_INFO("installing schedule_entry table"),
            mnesia:create_table(schedule_entry,
                                [{attributes,
                                  record_info(fields, schedule_entry)},
                                 {disc_copies, [node()]}])
    end.

