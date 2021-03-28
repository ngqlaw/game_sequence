%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2020-2021, ninggq <ngq_scut@126.com>
%%% @doc
%%% time sequence api
%%% @end
%%% Created : 11. 九月 2020 15:20
%%%-------------------------------------------------------------------
-module(game_sequence).

-include("game_sequence.hrl").

-export([
    start_task/1,
    stop_task/1,
    stop_task_event/2
]).

-compile({inline, [is_handler_exist/1]}).

%% 初始化，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(init(InitEvents::list()) -> {ok, State::any()}).
%% 事件处理，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(handle(Events::atom() | list(), TimeMark::integer(), State::any()) -> {ok, State::any()}).
%% 进程关闭，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(terminate(Reason::term(), State::any()) -> ok).

%% 事务进程启动，初始化选项有{proc, ProcHandler}时，必须有该回调函数
-callback(start_link(InitEvents::list()) -> {ok, pid()}).

-optional_callbacks([init/1, handle/3, terminate/2, start_link/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc 启动一个任务
start_task(Opts) ->
    FixOpts = normalize_options(lists:ukeysort(1, Opts), []),
    game_sequence_sup:start_child(FixOpts).

%% @doc 关闭任务
stop_task(TaskId) ->
    Pid = game_sequence_data:get_value(TaskId),
    game_sequence_handler:stop_task(Pid).

%% @doc 关闭任务子事件
stop_task_event(TaskId, Event) ->
    Pid = game_sequence_data:get_value(TaskId),
    game_sequence_handler:stop_event(Pid, Event).

%%%===================================================================
%%% Internal functions
%%%===================================================================
normalize_options([{proc, Handler}|T], Opt) when is_atom(Handler) ->
    case is_handler_exist(Opt) of
        true ->
            normalize_options(T, Opt);
        false ->
            normalize_options(T, [{proc, Handler}|Opt])
    end;
normalize_options([{handle, Handler}|T], Opt) when is_atom(Handler) ->
    case is_handler_exist(Opt) of
        true ->
            normalize_options(T, Opt);
        false ->
            normalize_options(T, [{handle, Handler}|Opt])
    end;
normalize_options([{key, Key}|T], Opt) ->
    %% 必须保证key唯一
    normalize_options(T, [{key, Key}|Opt]);
normalize_options([{last_time, MSecs}|T], Opt) ->
    normalize_options(T, [{last_time, MSecs}|Opt]);
normalize_options([{tasks, [_|_] = Tasks}|T], Opt) ->
    NewTasks = check_task(Tasks, []),
    normalize_options(T, [{tasks, NewTasks}|Opt]);
normalize_options([_|T], Opt) ->
    normalize_options(T, Opt);
normalize_options([], Opt) ->
    lists:reverse(Opt).

check_task([{_Event, StartTime, {?TASK_LOOP_DAY, MSecs}} = H|T], Res)
when is_integer(StartTime) andalso is_integer(MSecs)
andalso MSecs >= 0 andalso MSecs < ?DAY_M_SECS ->
    check_task(T, [H|Res]);
check_task([{_Event, StartTime, {?TASK_LOOP_DAYS, Days}} = H|T], Res)
when is_integer(StartTime) andalso is_integer(Days) andalso Days > 0 ->
    check_task(T, [H|Res]);
check_task([{_Event, StartTime, {?TASK_LOOP_WEEK, WeekNum}} = H|T], Res)
when is_integer(StartTime) andalso is_integer(WeekNum)
andalso WeekNum >= 1 andalso WeekNum =< 7 ->
    check_task(T, [H|Res]);
check_task([{_Event, StartTime, {?TASK_LOOP_MONTH, Day}} = H|T], Res)
when is_integer(StartTime)
andalso ((is_integer(Day) andalso Day >= 1 andalso Day =< 31) orelse Day =:= last) ->
    check_task(T, [H|Res]);
check_task([{_Event, StartTime, CloseTime} = H|T], Res)
when is_integer(StartTime) andalso is_integer(CloseTime) andalso CloseTime > StartTime ->
    check_task(T, [H|Res]);
check_task([_|T], Res) ->
    check_task(T, Res);
check_task([], Res) ->
    Res.

is_handler_exist(Opt) ->
    lists:keymember(proc, 1, Opt) orelse lists:keymember(handle, 1, Opt).