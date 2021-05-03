-module(scheduler_plugin).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

-export([install/0, uninstall/1, handle/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(INTERVAL, 60000). % 1 minute

-type frequency() :: oneoff | daily | weekly | monthly.

-record(schedule_entry, {name :: binary(),
                         time :: calendar:time(),
                         date :: calendar:date(),
                         frequency :: frequency()
                        }).

-record(state, {timer :: timer:tref() | undefined}).

-define(REACTION, <<":ree:818301546831151114">>).
-define(HOUR, 3600). % 60 * 60
-define(TEN_MINUTES, 600). % 10 * 60

%% API functions

install() ->
    install_tables(),
    gen_server:start_link(?MODULE, [], []).

uninstall(Pid) ->
    gen_server:cast(Pid, stop).

handle(Pid, {msg, Msg=#{<<"content">> := Content}}) ->
    case binary:split(Content, <<" ">>, [global, trim_all]) of
        [_,<<"schedule">>|Rest] -> handle_command(Pid, Rest, Msg); _ -> ok
    end;
handle(Pid, {react, Msg}) ->
    gen_server:cast(Pid, {react, Msg}).

handle_command(_Pid, [], Msg) ->
    send_help(Msg);
handle_command(_Pid, [<<"help">>|_], Msg) ->
    send_help(Msg);
handle_command(Pid, [<<"add">>|Rest], Msg) ->
    gen_server:cast(Pid, {add, Rest, Msg});
handle_command(Pid, [<<"list">>|_], Msg) ->
    gen_server:cast(Pid, {list, Msg});
handle_command(Pid, [<<"delete">>, First|Rest], Msg) ->
    gen_server:cast(Pid, {delete, [First|Rest], Msg});
handle_command(_Pid, [Cmd|_], Msg) ->
    send_reply(<<"unknown command: ", Cmd/binary>>, Msg),
    send_help(Msg).

%% gen_server callbacks

init([]) ->
    gen_server:cast(self(), start_timer),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({add, [Time, Date, Freq, Name0|Name1], Msg}, State) ->
    Name = lists:foldl(fun(A, B) -> <<B/binary, A/binary>> end, <<>>,
                       lists:join(<<" ">>, [Name0|Name1])),
    case handle_add_(Name, Time, Date, Freq) of
        ok ->
            Reply = <<"schedule ", Name/binary, " created">>,
            {ok, R} = send_reply_reply(Reply, Msg),
            send_reaction(?REACTION, R);
        {error, Error} ->
            send_reply(<<"error creating schedule ", Name/binary, ": ",
                         Error/binary>>, Msg)
    end,
    {noreply, State};
handle_cast({add, _, Msg}, State) ->
    send_reply(<<"not enough arguments to add">>, Msg),
    {noreply, State};
handle_cast({react, Msg}, State) ->
    handle_react(Msg),
    {noreply, State};
handle_cast({list, Msg}, State) ->
    BinJoin = fun(X, Acc) -> <<Acc/binary, "\n", X/binary>> end,
    BinEntries = lists:sort(lists:map(fun entry_bin/1, get_schedule_entries())),
    Reply = lists:foldl(BinJoin, <<"Current Schedules:">>, BinEntries),
    send_reply(Reply, Msg),
    {noreply, State};
handle_cast({delete, Parts, Msg}, State) ->
    Name = lists:foldl(fun(A, B) -> <<B/binary, A/binary>> end, <<>>,
                       lists:join(<<" ">>, Parts)),
    case handle_delete_(Name) of
        ok -> send_reply(<<"schedule ", Name/binary, " deleted">>, Msg);
        {error, Error} ->
            send_reply(<<"error deleting schedule ", Name/binary, ": ",
                         Error/binary>>, Msg)
    end,
    {noreply, State};
handle_cast(start_timer, State) ->
    if State#state.timer =/= undefined ->
           timer:cancel(State#state.timer);
       true -> ok
    end,
    {ok, TRef} = timer:apply_interval(?INTERVAL, gen_server, cast,
                                      [self(), timer_tick]),
    {noreply, State#state{timer=TRef}};
handle_cast(timer_tick, State) ->
    ?LOG_INFO("got timer tick"),
    process_events(),
    remove_stale_events(),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%% helper functions

handle_add_(Name, Time, Date, Freq) ->
    case lists:member(Name, get_role_names()) of
        true ->
            {error, <<"there is already a role with that name!">>};
        false ->
            case build_entry(Name, Time, Date, Freq) of
                {ok, Entry} ->
                    ok = create_role(Name),
                    add_entry(Entry);
                E -> E
            end
    end.

add_entry(Entry) ->
    F = fun() ->
        case mnesia:read({schedule_entry, Entry#schedule_entry.name}) =:= [] of
            true -> mnesia:write(Entry);
            false -> {error, <<"schedule already exists">>}
        end
    end,
    mnesia:activity(transaction, F).

build_entry(Name, Time, Date, Freq) ->
    V = [parse_time(Time), parse_date(Date), parse_freq(Freq)],
    case V of
        [{ok, PTime}, {ok, PDate}, {ok, PFreq}] ->
            {ok, #schedule_entry{name=Name,
                                 time=PTime,
                                 date=PDate,
                                 frequency=PFreq}};
        Errors ->
            lists:keyfind(error, 1, Errors)
    end.

split_number(BNum) ->
    case string:to_integer(string:to_lower(binary:bin_to_list(BNum))) of
        {Hour, [$:|Rest]} ->
            case string:to_integer(Rest) of
                E={error, _} -> E;
                {Minute, Period} -> {Hour, Minute, Period}
            end;
        V -> V
    end.

parse_time(Time) ->
    case split_number(Time) of
        {HourMin, []} -> validate_time(HourMin div 100, HourMin rem 100);
        {Hour, "am"} -> validate_time(Hour, 0);
        {Hour, "pm"} -> validate_time(Hour + 1200, 0);
        {Hour, Minute, "am"} -> validate_time(Hour, Minute);
        {Hour, Minute, "pm"} -> validate_time(Hour + 1200, Minute);
        _ -> {error, <<"invalid time">>}
    end.

validate_time(Hour, Min) ->
    if Hour >= 0 andalso Hour < 24 andalso Min >= 0 andalso Min < 60 ->
           {ok, {Hour, Min, 0}};
       true ->
           {error, <<"time not in valid range">>}
    end.

parse_date(Date) ->
    case binary:split(Date, [<<"-">>, <<"/">>], [global]) of
        L=[_, _, _] ->
            try
                [Y, M, D] = lists:map(fun erlang:binary_to_integer/1, L),
                validate_date({Y, M, D})
            catch
                error:badarg -> {error, <<"invalid date">>}
            end;
        _ -> {error, <<"invalid date">>}
    end.

validate_date(Date) ->
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> {error, <<"date out of range">>}
    end.

parse_freq(Freq) ->
    validate_freq(string:to_lower(binary:bin_to_list(Freq))).

validate_freq("oneoff") -> {ok, oneoff};
validate_freq("daily") -> {ok, daily};
validate_freq("weekly") -> {ok, weekly};
validate_freq("monthly") -> {ok, monthly};
validate_freq(_) -> {error, <<"invalid frequency">>}.

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

build_reply(Reply, #{<<"author">> := #{<<"id">> := AuthorId}}) ->
    <<"<@!", AuthorId/binary, "> ", Reply/binary>>.

send_reply(Reply, Message=#{<<"channel_id">> := ChannelId}) ->
    ApiServer = discord_sup:get_api_server(),
    R = build_reply(Reply, Message),
    discord_api:send_message(ApiServer, ChannelId, R).

send_reply_reply(Reply, Message=#{<<"channel_id">> := ChannelId}) ->
    ApiServer = discord_sup:get_api_server(),
    R = build_reply(Reply, Message),
    discord_api:send_message_reply(ApiServer, ChannelId, R).

send_reaction(Reaction, #{<<"id">> := Id, <<"channel_id">> := ChannelId}) ->
    ApiServer = discord_sup:get_api_server(),
    discord_api:send_reaction(ApiServer, ChannelId, Id, Reaction).

send_help(Message) ->
    Reply = <<"Scheduler Help\n\n",
              "`add {time} {date} {frequency} {name}` - create a schedule "
              "called `{name}`, starting on `{date}` at `{time}`, repeating "
              "with `{frequency}`\n",
              "`list` - show all current schedules\n",
              "`delete {name}` - delete the schedule with name `{name}`\n\n",
              "Examples:\n\n",
              "To create a schedule called `sample` which will alert you "
              "`daily` from the date `2021-04-21` an hour and 10 minutes before"
              "`1540` you would do:\n",
              "```\n",
              "schedule add 1540 2021-04-21 daily sample\n",
              "```\n"
            >>,
    send_reply(Reply, Message).

get_roles() ->
    Gateway = discord_sup:get_gateway(),
    {ok, GuildId} = discord_gateway:guild_id(Gateway),
    ApiServer = discord_sup:get_api_server(),
    discord_api:get_roles(ApiServer, GuildId).

get_role_names() ->
    lists:map(fun(#{<<"name">> := Name}) -> Name end, get_roles()).

create_role(Name) ->
    Gateway = discord_sup:get_gateway(),
    {ok, GuildId} = discord_gateway:guild_id(Gateway),
    ApiServer = discord_sup:get_api_server(),
    discord_api:create_role(ApiServer, GuildId, Name).

handle_react(#{<<"emoji">> := #{<<"id">> := null}}) -> ok;
handle_react(#{<<"user_id">> := UserId,
               <<"message_id">> := MessageId,
               <<"channel_id">> := ChannelId,
               <<"guild_id">> := GuildId,
               <<"emoji">> := #{<<"name">> := EmojiName,
                                <<"id">> := EmojiId}}) ->
    React = <<":", EmojiName/binary, ":", EmojiId/binary>>,
    if React =:= ?REACTION ->
           case schedule_message(ChannelId, MessageId) of
               {true, Schedule} -> grant_role(GuildId, UserId, Schedule);
               false -> ok
           end;
       true -> ok
    end.

schedule_message(ChannelId, MessageId) ->
    ApiServer = discord_sup:get_api_server(),
    Gateway = discord_sup:get_gateway(),
    Message = discord_api:get_message(ApiServer, ChannelId, MessageId),
    #{<<"author">> := #{<<"id">> := AuthorId}} = Message,
    {ok, UserId} = discord_gateway:user_id(Gateway),
    if UserId =:= AuthorId ->
           #{<<"content">> := Content} = Message,
           case binary:split(Content, [<<" ">>], [global]) of
               [_,<<"schedule">>|Rest] -> parse_schedule_message(Rest);
               _ -> false
           end;
       true -> false
    end.

parse_schedule_message(ScheduleParts) ->
    case lists:reverse(ScheduleParts) of
        [<<"created">>|Rest] ->
            {true, lists:foldl(fun(A, B) -> <<B/binary, A/binary>> end, <<>>,
                               lists:join(<<" ">>, lists:reverse(Rest)))};
        _ -> false
    end.

grant_role(GuildId, UserId, RoleName) ->
    ?LOG_INFO("granting role ~s to user ~s", [RoleName, UserId]),
    RoleId = get_role_id(RoleName),
    ApiServer = discord_sup:get_api_server(),
    discord_api:add_member_role(ApiServer, GuildId, UserId, RoleId).

get_role_id(RoleName) ->
    FilterFn = fun(#{<<"name">> := Name}) -> RoleName =:= Name end,
    [#{<<"id">> := RoleId}] = lists:filter(FilterFn, get_roles()),
    RoleId.

get_schedule_entries() ->
    Fn = fun() ->
        mnesia:foldl(fun(X, Acc) -> [X|Acc] end, [], schedule_entry)
    end,
    mnesia:activity(transaction, Fn).

get_oneoff_entries() ->
    Entries = get_schedule_entries(),
    lists:filter(fun(#schedule_entry{frequency=Freq}) -> Freq =:= oneoff end,
                 Entries).

alert_today(Today, #schedule_entry{date=Date, frequency=oneoff}) ->
    Date =:= Today;
alert_today(Today, #schedule_entry{date=Date, frequency=daily}) ->
    TSec = calendar:date_to_gregorian_days(Today),
    DSec = calendar:date_to_gregorian_days(Date),
    TSec - DSec >= 0;
alert_today(Today, #schedule_entry{date=Date, frequency=weekly}) ->
    TSec = calendar:date_to_gregorian_days(Today),
    DSec = calendar:date_to_gregorian_days(Date),
    TDay = calendar:day_of_the_week(Today),
    DDay = calendar:day_of_the_week(Date),
    TSec - DSec >= 0 andalso TDay =:= DDay;
alert_today(Today={_, _, TD}, #schedule_entry{date=Date, frequency=monthly}) ->
    {_, _, DD} = Date,
    TSec = calendar:date_to_gregorian_days(Today),
    DSec = calendar:date_to_gregorian_days(Date),
    TSec - DSec >= 0 andalso TD =:= DD.

should_fire({NDate, NTime}, S=#schedule_entry{name=Name, time=Time}) ->
    case alert_today(NDate, S) of
        true ->
            Diff = calendar:time_to_seconds(Time) -
                calendar:time_to_seconds(NTime),
            ?LOG_INFO("alert ~s is scheduled for today", [Name]),
            ?LOG_INFO("alert ~s has time diff ~p", [Name, Diff]),
            if Diff =< ?HOUR + 60 andalso Diff > ?HOUR -> true;
               Diff =< ?TEN_MINUTES + 60 andalso Diff > ?TEN_MINUTES -> true;
               Diff =< 60 andalso Diff > 0 -> true;
               true -> false
            end;
        false ->
            ?LOG_INFO("alert ~s is NOT scheduled for today", [Name]),
            false
    end.

time_name(NTime, Time) ->
    Diff = calendar:time_to_seconds(Time) - calendar:time_to_seconds(NTime),
    if Diff =< ?HOUR + 60 andalso Diff > ?HOUR ->
           <<"in an hour">>;
       Diff =< ?TEN_MINUTES + 60 andalso Diff > ?TEN_MINUTES ->
           <<" in 10 minutes">>;
       Diff =< 60 andalso Diff > 0 ->
           <<"now!">>
    end.

process_events() ->
    Now = calendar:local_time(),
    Entries = get_schedule_entries(),
    Current = lists:filter(fun(X) -> should_fire(Now, X) end, Entries),
    ChannelId = binary:list_to_bin(os:getenv("TATARU_SCHEDULE_CHANNEL")),
    lists:foreach(fun(X) -> fire_alert(ChannelId, Now, X) end, Current).

remove_stale_events() ->
    Now = calendar:local_time(),
    Entries = get_oneoff_entries(),
    Past = lists:filter(fun(E) -> entry_in_past(Now, E) end, Entries),
    RemoveFn = fun(#schedule_entry{name=Name}) ->
                       ?LOG_INFO("removing old oneoff entry ~s", [Name]),
                       handle_delete_(Name)
               end,
    lists:foreach(RemoveFn, Past).

entry_in_past(Now, #schedule_entry{date=Date, time=Time}) ->
    D0 = calendar:datetime_to_gregorian_seconds({Date, Time}),
    D1 = calendar:datetime_to_gregorian_seconds(Now),
    D0 - D1 < 0.

fire_alert(ChannelId, {_NDate, NTime}, #schedule_entry{name=Name, time=Time}) ->
    ?LOG_INFO("firing alert ~s", [Name]),
    RoleId = get_role_id(Name),
    TimeName = time_name(NTime, Time),
    Alert = <<"<@&", RoleId/binary, "> coming up ", TimeName/binary>>,
    ApiServer = discord_sup:get_api_server(),
    discord_api:send_message(ApiServer, ChannelId, Alert).

time_to_binary({H, M, _}) ->
  binary:list_to_bin(io_lib:format("~2..0w:~2..0w", [H, M])).

date_to_binary({Y, M, D}) ->
  binary:list_to_bin(io_lib:format("~4..0w/~2..0w/~2..0w", [Y, M, D])).

entry_bin(#schedule_entry{name=Name, time=Time, date=Date, frequency=Freq}) ->
    BinFreq = binary:list_to_bin(atom_to_list(Freq)),
    BinDate = date_to_binary(Date),
    BinTime = time_to_binary(Time),
    <<"  ", Name/binary, "(",  BinFreq/binary, ") starting on ", BinDate/binary,
      " at ", BinTime/binary>>.

handle_delete_(Name) ->
    ApiServer = discord_sup:get_api_server(),
    case get_schedule_entry(Name) of
        {ok, _Entry} ->
            ok = delete_entry(Name),
            RoleId = get_role_id(Name),
            Gateway = discord_sup:get_gateway(),
            {ok, GuildId} = discord_gateway:guild_id(Gateway),
            discord_api:delete_role(ApiServer, GuildId, RoleId);
        E={error, _} -> E
    end.

get_schedule_entry(Name) ->
    Fn = fun() -> mnesia:read({schedule_entry, Name}) end,
    case mnesia:activity(transaction, Fn) of
        [E] -> {ok, E};
        [_|_] -> {error, <<"multiple schedules found">>};
        [] -> {error, <<"schedule doesn't exist">>}
    end.

delete_entry(Name) ->
    Fn = fun() -> mnesia:delete({schedule_entry, Name}) end,
    mnesia:activity(transaction, Fn).
