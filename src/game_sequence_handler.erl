%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2020-2021, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game time sequence handler
%%% @end
%%% Created : 11. 九月 2020 15:20
%%%-------------------------------------------------------------------
-module(game_sequence_handler).

-behaviour(gen_server).

-include("game_sequence.hrl").

%% API
-export([start_link/1, stop_task/1, stop_event/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(LOOP_EVENT, loop).
-define(CLOSE_EVENT, init_close).
-define(START_TIMER(Time, Event), erlang:start_timer(Time, self(), Event)).

-record(state, {
    key = undefined :: term(),
    proc = undefined :: undefined | {pid(), reference()},
    handle = undefined :: undefined | {atom(), tuple()},
    tasks = [] :: list(),
    loop_tasks = [] :: list(),
    loop_time_mark = 0 :: max | integer(),
    notice_tasks = [] :: list(),
    timer = undefined :: undefined | reference(),
    done_tasks = [] :: list(),
    close = false :: false | integer(),
    close_timer = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec(start_link(Opts :: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc 关闭任务
-spec(stop_task(Server :: undefined | pid()) -> ok | server_not_exist).
stop_task(undefined) -> server_not_exist;
stop_task(Pid) ->
    gen_server:cast(Pid, stop).

%% @doc 关闭任务子事件
-spec(stop_event(Server :: undefined | pid(), Event :: term()) -> ok | server_not_exist).
stop_event(undefined, _Event) -> server_not_exist;
stop_event(Pid, Event) ->
    gen_server:cast(Pid, {stop_event, Event}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Opts) ->
    Key = proplists:get_value(key, Opts),
    %% 新增索引
    game_sequence_data:add(Key, self()),
    %% 构建任务数据
    Now = erlang:system_time(1000),
    OriginTasks = proplists:get_value(tasks, Opts, []),
    {Tasks, LoopTasks, NoticeTasks, DoneTasks} = init_tasks(OriginTasks, Now, [], [], [], []),
    TempState = #state{
        key = Key, tasks = Tasks, loop_tasks = LoopTasks,
        loop_time_mark = Now, notice_tasks = [],
        timer = ?START_TIMER(0, ?LOOP_EVENT), done_tasks = DoneTasks
    },
    State =
        case proplists:get_value(proc, Opts) of
            undefined ->
                Handler = proplists:get_value(handle, Opts),
                {ok, InsideState} = Handler:init(NoticeTasks),
                TempState#state{handle = {Handler, InsideState}};
            ProcHandler ->
                {ok, Pid} = ProcHandler:start_link(NoticeTasks),
                Ref = erlang:monitor(process, Pid),
                TempState#state{proc = {Pid, Ref}}
        end,
    %% 任务关闭时间
    case proplists:get_value(last_time, Opts) of
        T when is_integer(T) ->
            case Now >= T of
                true ->
                    {stop, run_out_time};
                false ->
                    {ok, State#state{
                        close = T,
                        close_timer = ?START_TIMER(1000, ?CLOSE_EVENT)
                    }}
            end;
        _ ->
            {ok, State#state{close = false}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% 关闭任务
handle_cast(stop, #state{handle = undefined, proc = {Pid, _}} = State) ->
    %% 给事务进程发送关闭信息，定时进程需要等待事务进程先关闭
    erlang:send(Pid, stop),
    %% 停止子任务定时器（保留关闭定时器）
    {noreply, State#state{timer = undefined}};
handle_cast(stop, State) ->
    {stop, stop, State};
%% 关闭任务子事件
handle_cast({stop_event, Event}, #state{
    tasks = Tasks, loop_tasks = LoopTasks,
    notice_tasks = NoticeTasks, loop_time_mark = TimeMark,
    done_tasks = DoneTasks
} = State) ->
    {NewTasks, D1} = lists:partition(
        fun(#task{event = E1}) -> E1 /= Event end,
        Tasks),
    {NewLoopTasks, D2} = lists:partition(
        fun(#loop_task{event = E2}) -> E2 /= Event end,
        LoopTasks),
    NewNoticeTasks = lists:filter(
        fun({E3, _}) -> E3 /= Event end,
        NoticeTasks),
    NewState = handle_notice_event(State#state{
        loop_time_mark = erlang:system_time(1000),
        notice_tasks = [{Event, ?EVENT_REMOVE}]
    }),
    {noreply, NewState#state{
        tasks = NewTasks, loop_tasks = NewLoopTasks,
        notice_tasks = NewNoticeTasks, loop_time_mark = TimeMark,
        done_tasks = DoneTasks ++ (D1 ++ D2)
    }};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({timeout, TimerRef, ?LOOP_EVENT}, #state{
    tasks = Tasks, loop_tasks = LoopTasks, loop_time_mark = TimeMark,
    notice_tasks = NoticeTasks, timer = TimerRef,
    close = Close, close_timer = CloseRef, done_tasks = DoneTasks
} = State) ->
    Now = erlang:system_time(1000),
    case Now >= TimeMark of
        true ->
            NewState = handle_notice_event(State),
            {NewTasks, NewLoopTasks, NewDoneTasks} = update_tasks(NoticeTasks, TimeMark, Tasks, LoopTasks, DoneTasks),
            {NewTimeMark, NewNoticeTasks} = get_task_timer(NewTasks, NewLoopTasks),
            NewTimerRef = minute_timer(Now, NewTimeMark, ?LOOP_EVENT),
            {NewClose, NewCloseRef} = maybe_close(NewTimerRef, Now, Close, CloseRef),
            {noreply, NewState#state{
                tasks = NewTasks, loop_tasks = NewLoopTasks,
                loop_time_mark = NewTimeMark, notice_tasks = NewNoticeTasks,
                timer = NewTimerRef, close = NewClose,
                close_timer = NewCloseRef, done_tasks = NewDoneTasks
            }};
        false ->
            NewTimerRef = minute_timer(Now, TimeMark, ?LOOP_EVENT),
            {noreply, State#state{timer = NewTimerRef}}
    end;
handle_info({timeout, TimerRef, ?CLOSE_EVENT}, #state{
    close = Time, close_timer = TimerRef
} = State) ->
    %% 轮询时间设置为一分钟
    Now = erlang:system_time(1000),
    case Now >= Time of
        true ->
            %% 时间到
            NewState = handle_notice_event(State#state{
                loop_time_mark = Time, notice_tasks = ?EVENT_FINISH
            }),
            {stop, normal, NewState#state{
                timer = undefined, notice_tasks = []
            }};
        false ->
            NewTimerRef = minute_timer(Now, Time, ?CLOSE_EVENT),
            {noreply, State#state{close_timer = NewTimerRef}}
    end;
handle_info({'DOWN', Ref, process, _Object, _Reason}, #state{proc = {_, Ref}} = State) ->
    %% 事务进程关闭，任务定时进程原本的定时器忽略，改为立即关闭
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, #state{key = Key, handle = undefined} = State) ->
    error_logger:info_msg("Server stop ~p with ~p", [Reason, State]),
    game_sequence_data:del(Key),
    ok;
terminate(Reason, #state{key = Key, handle = {Handler, InsideState}} = State) ->
    error_logger:info_msg("Server stop ~p with ~p", [Reason, State]),
    Handler:terminate(Reason, InsideState),
    game_sequence_data:del(Key),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 初始化子任务
init_tasks([{Event, StartTime, {?TASK_LOOP_DAY, MSecs}}|T], Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    Point =
        case StartTime >= Now of
            true ->
                StartTime;
            false ->
                get_new_point(?TASK_LOOP_DAY, StartTime, MSecs, Now)
        end,
    init_tasks(T, Now, Tasks, [#loop_task{
        event = Event,
        point = Point,
        type = ?TASK_LOOP_DAY,
        type_mark = MSecs
    }|LoopTasks], NoticeTasks, DoneTasks);
init_tasks([{Event, StartTime, {?TASK_LOOP_DAYS, Days}}|T], Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    Point =
        case StartTime >= Now of
            true ->
                StartTime;
            false ->
                get_new_point(?TASK_LOOP_DAYS, StartTime, Days, Now)
        end,
    init_tasks(T, Now, Tasks, [#loop_task{
        event = Event,
        point = Point,
        type = ?TASK_LOOP_DAYS,
        type_mark = Days
    }|LoopTasks], NoticeTasks, DoneTasks);
init_tasks([{Event, StartTime, {?TASK_LOOP_WEEK, WeekNum}}|T], Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    Point =
        case StartTime >= Now of
            true ->
                StartTime;
            false ->
                get_new_point(?TASK_LOOP_WEEK, StartTime, WeekNum, Now)
        end,
    init_tasks(T, Now, Tasks, [#loop_task{
        event = Event,
        point = Point,
        type = ?TASK_LOOP_WEEK,
        type_mark = WeekNum
    }|LoopTasks], NoticeTasks, DoneTasks);
init_tasks([{Event, StartTime, {?TASK_LOOP_MONTH, Day}}|T], Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    Point =
        case StartTime >= Now of
            true ->
                StartTime;
            false ->
                get_new_point(?TASK_LOOP_MONTH, StartTime, Day, Now)
        end,
    init_tasks(T, Now, Tasks, [#loop_task{
        event = Event,
        point = Point,
        type = ?TASK_LOOP_MONTH,
        type_mark = Day
    }|LoopTasks], NoticeTasks, DoneTasks);
init_tasks([{Event, StartTime, CloseTime}|T], Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    if
        StartTime >= Now ->
            Task = #task{
                event = Event, state = ?TASK_START,
                start_time = StartTime, close_time = CloseTime
            },
            init_tasks(T, Now, [Task|Tasks], LoopTasks, NoticeTasks, DoneTasks);
        CloseTime > Now ->
            Task = #task{
                event = Event, state = ?TASK_GOING,
                start_time = StartTime, close_time = CloseTime
            },
            init_tasks(T, Now, [Task|Tasks], LoopTasks, [
                {Event, ?EVENT_START, StartTime}|NoticeTasks], DoneTasks);
        true ->
            Task = #task{
                event = Event, state = ?TASK_CLOSE,
                start_time = StartTime, close_time = CloseTime
            },
            init_tasks(T, Now, Tasks, LoopTasks, [
                {Event, ?EVENT_START, StartTime},
                {Event, ?EVENT_CLOSE, CloseTime}|NoticeTasks], [Task|DoneTasks])
    end;
init_tasks([], _Now, Tasks, LoopTasks, NoticeTasks, DoneTasks) ->
    {Tasks, LoopTasks, NoticeTasks, DoneTasks}.

%% 更新子任务
update_tasks([{Event, ?EVENT_LOOP}|T], Now, Tasks, LoopTasks, DoneTasks) ->
    case lists:keytake(Event, #loop_task.event, LoopTasks) of
        {value, #loop_task{
            point = Point,
            type = Type,
            type_mark = TypeMark
        } = LoopTask, Rest} ->
            NewPoint = get_new_point(Type, Point, TypeMark, Now),
            update_tasks(T, Now, Tasks, [LoopTask#loop_task{point = NewPoint}|Rest], DoneTasks);
        _ ->
            update_tasks(T, Now, Tasks, LoopTasks, DoneTasks)
    end;
update_tasks([{Event, ?EVENT_START}|T], Now, Tasks, LoopTasks, DoneTasks) ->
    case lists:keytake(Event, #task.event, Tasks) of
        {value, #task{state = ?TASK_START} = Task, Rest} ->
            update_tasks(T, Now, [Task#task{state = ?TASK_GOING}|Rest], LoopTasks, DoneTasks);
        _ ->
            update_tasks(T, Now, Tasks, LoopTasks, DoneTasks)
    end;
update_tasks([{Event, ?EVENT_CLOSE}|T], Now, Tasks, LoopTasks, DoneTasks) ->
    case lists:keytake(Event, #task.event, Tasks) of
        {value, #task{state = ?TASK_GOING} = Task, Rest} ->
            update_tasks(T, Now, Rest, LoopTasks, [Task#task{state = ?TASK_CLOSE}|DoneTasks]);
        _ ->
            update_tasks(T, Now, Tasks, LoopTasks, DoneTasks)
    end;
update_tasks([], _Now, Tasks, LoopTasks, DoneTasks) ->
    {Tasks, LoopTasks, DoneTasks}.

%% 设置子任务定时器
get_task_timer(Tasks, LoopTasks) ->
    {Mark, Temp} = get_intime_tasks(Tasks, max, []),
    get_intime_loop_tasks(LoopTasks, Mark, Temp).

%% 普通定时子任务时间信息
get_intime_tasks([#task{
    event = Event, start_time = StartTime, state = ?TASK_START
}|T], Mark, Temp) ->
    if
        Mark > StartTime ->
            get_intime_tasks(T, StartTime, [{Event, ?EVENT_START}]);
        Mark < StartTime ->
            get_intime_tasks(T, Mark, Temp);
        true ->
            get_intime_tasks(T, Mark, [{Event, ?EVENT_START}|Temp])
    end;
get_intime_tasks([#task{
    event = Event, close_time = CloseTime, state = ?TASK_GOING
}|T], Mark, Temp) ->
    if
        Mark > CloseTime ->
            get_intime_tasks(T, CloseTime, [{Event, ?EVENT_CLOSE}]);
        Mark < CloseTime ->
            get_intime_tasks(T, Mark, Temp);
        true ->
            get_intime_tasks(T, Mark, [{Event, ?EVENT_CLOSE}|Temp])
    end;
get_intime_tasks([], Mark, Temp) ->
    {Mark, Temp}.

%% 循环子任务时间信息
get_intime_loop_tasks([#loop_task{
    event = Event, point = Point
}|T], Mark, Temp) ->
    if
        Point > Mark ->
            get_intime_loop_tasks(T, Mark, Temp);
        Point < Mark ->
            get_intime_loop_tasks(T, Point, [{Event, ?EVENT_LOOP}]);
        true ->
            get_intime_loop_tasks(T, Mark, [{Event, ?EVENT_LOOP}|Temp])
    end;
get_intime_loop_tasks([], Mark, Temp) ->
    {Mark, Temp}.

%% 新的循环时间点（毫秒）
get_new_point(?TASK_LOOP_DAY, Point, MSecs, Point) ->
    Point + MSecs;
get_new_point(?TASK_LOOP_DAY, Point, MSecs, Now) ->
    N = erlang:ceil((Now - Point) / MSecs),
    Point + erlang:trunc(N * MSecs);
get_new_point(?TASK_LOOP_DAYS, Point, Days, Now) ->
    next_day(Point, Days, Now);
get_new_point(?TASK_LOOP_WEEK, Point, WeekNum, Now) ->
    D = erlang:trunc(7 * ?DAY_M_SECS),
    StartPoint =
        case Now - Point of
            DP when DP > D -> Now - D;
            _ -> Point
        end,
    Time = next_week(StartPoint div 1000, WeekNum, Now div 1000),
    MSec = Point rem 1000,
    erlang:trunc(Time * 1000) + MSec;
get_new_point(?TASK_LOOP_MONTH, Point, Day, Now) ->
    D = erlang:trunc(31 * ?DAY_M_SECS),
    StartPoint =
        case Now - Point of
            DP when DP > D -> Now - D;
            _ -> Point
        end,
    Time = next_month(StartPoint div 1000, Day, Now div 1000),
    MSec = Point rem 1000,
    erlang:trunc(Time * 1000) + MSec.

%% 天时间点(毫秒)
next_day(Point, _Days, Now) when Point > Now ->
    Point;
next_day(Point, Days, Now) ->
    next_day(Point + erlang:trunc(Days * ?DAY_M_SECS), Days, Now).

%% 周时间点(秒)
next_week(Point, WeekNum, Now) ->
    {Date, Time} = game_sequence_data:datetime(Point, 1),
    N = calendar:day_of_the_week(Date),
    D =
        case N < WeekNum of
            true ->
                (WeekNum - N) * ?DAY_SECS + calendar:time_to_seconds(Time);
            false ->
                (7 + N - WeekNum) * ?DAY_SECS + calendar:time_to_seconds(Time)
        end,
    NewPoint = erlang:trunc(game_sequence_data:timestamp({Date, {0, 0, 0}}) + D),
    case NewPoint >= Now of
        true -> NewPoint;
        false -> next_week(NewPoint, WeekNum, Now)
    end.

%% 月时间点(秒)
next_month(Point, Day, Now) ->
    {Date, Time} = game_sequence_data:datetime(Point, 1),
    NewPoint = erlang:trunc(do_next_month(Date, Time, Day)),
    case NewPoint >= Now of
        true -> NewPoint;
        false -> next_month(NewPoint, Day, Now)
    end.

do_next_month({Year, Month, OldDay}, Time, last) ->
    case calendar:last_day_of_the_month(Year, Month) of
        OldDay ->
            {NewYear, NewMonth} = game_sequence_data:incr_month(Year, Month),
            do_next_month({NewYear, NewMonth, 1}, Time, last);
        Day ->
            game_sequence_data:timestamp({{Year, Month, Day}, Time})
    end;
do_next_month({Year, Month, OldDay}, Time, Day) ->
    case OldDay < Day andalso calendar:valid_date(Year, Month, Day) of
        true ->
            game_sequence_data:timestamp({{Year, Month, Day}, Time});
        false ->
            Date = get_valid_month_date(Year, Month, Day, 12),
            game_sequence_data:timestamp({Date, Time})
    end.

get_valid_month_date(Year, Month, Day, N) when N > 0 ->
    {NewYear, NewMonth} = game_sequence_data:incr_month(Year, Month),
    case calendar:valid_date(NewYear, NewMonth, Day) of
        true ->
            {NewYear, NewMonth, Day};
        false ->
            get_valid_month_date(NewYear, NewMonth, Day, N - 1)
    end.

%% 通知事件处理
handle_notice_event(#state{notice_tasks = []} = State) ->
    State;
handle_notice_event(#state{
    loop_time_mark = TimeMark, notice_tasks = NoticeTasks,
    handle = {Handler, InsideState}
} = State) ->
    case catch Handler:handle(NoticeTasks, TimeMark, InsideState) of
        {ok, NewInsideState} ->
            State#state{handle = {Handler, NewInsideState}};
        Error ->
            error_logger:error_msg("~p handle ~p fail: ~p", [Handler, NoticeTasks, Error]),
            State
    end;
handle_notice_event(#state{
    loop_time_mark = TimeMark, notice_tasks = NoticeTasks,
    proc = {Pid, _}
} = State) ->
    erlang:send(Pid, {event, TimeMark, NoticeTasks}),
    State;
handle_notice_event(#state{notice_tasks = NoticeTasks} = State) ->
    error_logger:warning_msg("not handle ~p", [NoticeTasks]),
    State.

maybe_close(undefined, Now, false, _CloseRef) ->
    {Now + ?MINUTE_M_SECS, ?START_TIMER(?MINUTE_M_SECS, ?CLOSE_EVENT)};
maybe_close(_Timer, _Now, Close, CloseRef) ->
    {Close, CloseRef}.

%% 定时每分钟（小于分钟的使用实际时间）
minute_timer(StartTime, EndTime, Event) when is_integer(EndTime) ->
    D = max(0, EndTime - StartTime),
    DT =
        case D > ?MINUTE_M_SECS of
            true -> ?MINUTE_M_SECS;
            false -> D
        end,
    ?START_TIMER(DT, Event);
minute_timer(_StartTime, _EndTime, _Event) ->
    undefined.
