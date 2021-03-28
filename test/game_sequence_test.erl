-module(game_sequence_test).

-include_lib("eunit/include/eunit.hrl").
-include("game_sequence.hrl").

optional_test() ->
    O1 = game_sequence:normalize_options([{proc, a},{proc, b},{handle, c}], []),
    ?assertEqual([{proc, a}], O1),

    O2 = game_sequence:normalize_options([{handle, a},{proc, b},{handle, c}], []),
    ?assertEqual([{handle, a}], O2),

    O3 = game_sequence:normalize_options([
        {tasks, [a]}, {key, b}, {last_time, 1},
        {other, o}], []),
    ?assert(lists:keymember(key, 1, O3)),
    ?assert(lists:keymember(last_time, 1, O3)),
    ?assert(lists:keymember(tasks, 1, O3)),
    ?assertNot(lists:keymember(other, 1, O3)),

    StartTime = erlang:system_time(1000),
    T1 = game_sequence:check_task([{a, StartTime, {?TASK_LOOP_DAY, 600000}}], []),
    T2 = game_sequence:check_task([{a, time, {?TASK_LOOP_DAY, 600000}}], []),
    T3 = game_sequence:check_task([{a, StartTime, {?TASK_LOOP_DAY, m_sec}}], []),
    T4 = game_sequence:check_task([{a, StartTime, {other, 600000}}], []),
    T5 = game_sequence:check_task([{a, StartTime, {?TASK_LOOP_DAY, ?DAY_M_SECS}}], []),
    ?assertEqual([{a, StartTime, {?TASK_LOOP_DAY, 600000}}], T1),
    ?assertEqual([], T2),
    ?assertEqual([], T3),
    ?assertEqual([], T4),
    ?assertEqual([], T5),

    T6 = game_sequence:check_task([{b, StartTime, {?TASK_LOOP_DAYS, 0}}], []),
    T7 = game_sequence:check_task([{b, StartTime, {?TASK_LOOP_DAYS, 1}}], []),
    ?assertEqual([], T6),
    ?assertEqual([{b, StartTime, {?TASK_LOOP_DAYS, 1}}], T7),

    T8 = game_sequence:check_task([{c, StartTime, {?TASK_LOOP_WEEK, 1}}], []),
    T9 = game_sequence:check_task([{c, StartTime, {?TASK_LOOP_WEEK, 7}}], []),
    T10 = game_sequence:check_task([{c, StartTime, {?TASK_LOOP_WEEK, 0}}], []),
    T11 = game_sequence:check_task([{c, StartTime, {?TASK_LOOP_WEEK, 8}}], []),
    ?assertEqual([{c, StartTime, {?TASK_LOOP_WEEK, 1}}], T8),
    ?assertEqual([{c, StartTime, {?TASK_LOOP_WEEK, 7}}], T9),
    ?assertEqual([], T10),
    ?assertEqual([], T11),

    T12 = game_sequence:check_task([{d, StartTime, {?TASK_LOOP_MONTH, 1}}], []),
    T13 = game_sequence:check_task([{d, StartTime, {?TASK_LOOP_MONTH, 31}}], []),
    T14 = game_sequence:check_task([{d, StartTime, {?TASK_LOOP_MONTH, last}}], []),
    T15 = game_sequence:check_task([{d, StartTime, {?TASK_LOOP_MONTH, 0}}], []),
    T16 = game_sequence:check_task([{d, StartTime, {?TASK_LOOP_MONTH, 32}}], []),
    ?assertEqual([{d, StartTime, {?TASK_LOOP_MONTH, 1}}], T12),
    ?assertEqual([{d, StartTime, {?TASK_LOOP_MONTH, 31}}], T13),
    ?assertEqual([{d, StartTime, {?TASK_LOOP_MONTH, last}}], T14),
    ?assertEqual([], T15),
    ?assertEqual([], T16),

    T17 = game_sequence:check_task([{e, StartTime, StartTime}], []),
    T18 = game_sequence:check_task([{e, StartTime, StartTime - 1000}], []),
    CloseTime = StartTime + 1,
    T19 = game_sequence:check_task([{e, StartTime, CloseTime}], []),
    T20 = game_sequence:check_task([{e, StartTime, time}], []),
    ?assertEqual([], T17),
    ?assertEqual([], T18),
    ?assertEqual([{e, StartTime, CloseTime}], T19),
    ?assertEqual([], T20),

    T21 = game_sequence:check_task([f], []),
    ?assertEqual([], T21),
    ok.

init_task_test() ->
    Now = erlang:system_time(1000),
    Start1 = Now - 1000,
    Start2 = Now - 5000,
    Close1 = Now + 5000,
    Close2 = Now - 1000,
    {Tasks, _, NoticeTasks, DoneTasks} = game_sequence_handler:init_tasks([
        {t1, Now, Close1}, {t2, Start1, Close1}, {t3, Start2, Close2}
    ], Now, [], [], [], []),

    ?assertEqual(#task{
        event = t1, state = ?TASK_START, start_time = Now, close_time = Close1
    }, lists:keyfind(t1, #task.event, Tasks)),

    ?assertEqual(#task{
        event = t2, state = ?TASK_GOING, start_time = Start1, close_time = Close1
    }, lists:keyfind(t2, #task.event, Tasks)),
    ?assertEqual({t2, ?EVENT_START, Start1}, lists:keyfind(t2, 1, NoticeTasks)),

    ?assertEqual(false, lists:keyfind(t3, #task.event, Tasks)),
    ?assert(lists:member({t3, ?EVENT_START, Start2}, NoticeTasks)),
    ?assert(lists:member({t3, ?EVENT_CLOSE, Close2}, NoticeTasks)),
    ?assertEqual([#task{
        event = t3, state = ?TASK_CLOSE, start_time = Start2, close_time = Close2
    }], DoneTasks),

    {_, [#loop_task{
        event = loop_day, point = DayPoint, type = day, type_mark = 1000
    }], _, _} = game_sequence_handler:init_tasks([
        {loop_day, 0, {?TASK_LOOP_DAY, 1000}}
    ], Now, [], [], [], []),
    ?assert(DayPoint > Now),

    {_, [#loop_task{
        event = loop_days, point = DaysPoint, type = days, type_mark = 1
    }], _, _} = game_sequence_handler:init_tasks([
        {loop_days, 0, {?TASK_LOOP_DAYS, 1}}
    ], Now, [], [], [], []),
    ?assert(DaysPoint > Now),

    {_, [#loop_task{
        event = loop_week, point = WeekPoint, type = week, type_mark = 1
    }], _, _} = game_sequence_handler:init_tasks([
        {loop_week, 0, {?TASK_LOOP_WEEK, 1}}
    ], Now, [], [], [], []),
    ?assert(WeekPoint > Now),

    {_, [#loop_task{
        event = loop_month, point = MonthPoint, type = month, type_mark = 1
    }], _, _} = game_sequence_handler:init_tasks([
        {loop_month, 0, {?TASK_LOOP_MONTH, 1}}
    ], Now, [], [], [], []),
    ?assert(MonthPoint > Now),
    ok.

task_time_test() ->
    Now = erlang:system_time(1000),

    T1 = Now + 1000,
    T2 = Now + 5000,
    T3 = Now + 8000,
    T4 = Now + 9000,
    {M1, L1} = game_sequence_handler:get_intime_tasks([#task{
        event = a1, start_time = T1, close_time = T2, state = ?TASK_START
    }], max, []),
    ?assert(M1 == T1),
    ?assertEqual([{a1, ?EVENT_START}], L1),

    {M2, L2} = game_sequence_handler:get_intime_tasks([#task{
        event = a1, start_time = T1, close_time = T2, state = ?TASK_START
    }, #task{
        event = a2, start_time = T1, close_time = T3, state = ?TASK_START
    }, #task{
        event = a3, start_time = T2, close_time = T3, state = ?TASK_START
    }], max, []),
    ?assert(M2 == T1),
    ?assertEqual({a1, ?EVENT_START}, lists:keyfind(a1, 1, L2)),
    ?assertEqual({a2, ?EVENT_START}, lists:keyfind(a2, 1, L2)),
    ?assertEqual(false, lists:keyfind(a3, 1, L2)),

    {M3, L3} = game_sequence_handler:get_intime_tasks([#task{
        event = a1, start_time = T2, close_time = T3, state = ?TASK_START
    }, #task{
        event = a2, start_time = T1, close_time = T3, state = ?TASK_START
    }, #task{
        event = a3, start_time = T2, close_time = T3, state = ?TASK_START
    }], max, []),
    ?assert(M3 == T1),
    ?assertEqual(false, lists:keyfind(a1, 1, L3)),
    ?assertEqual({a2, ?EVENT_START}, lists:keyfind(a2, 1, L3)),
    ?assertEqual(false, lists:keyfind(a3, 1, L3)),

    {M4, L4} = game_sequence_handler:get_intime_tasks([#task{
        event = a1, start_time = T2, close_time = T3, state = ?TASK_GOING
    }, #task{
        event = a2, start_time = T1, close_time = T3, state = ?TASK_GOING
    }, #task{
        event = a3, start_time = T3, close_time = T4, state = ?TASK_START
    }], max, []),
    ?assert(M4 == T3),
    ?assertEqual({a1, ?EVENT_CLOSE}, lists:keyfind(a1, 1, L4)),
    ?assertEqual({a2, ?EVENT_CLOSE}, lists:keyfind(a2, 1, L4)),
    ?assertEqual({a3, ?EVENT_START}, lists:keyfind(a3, 1, L4)),

    {M5, L5} = game_sequence_handler:get_intime_tasks([#task{
        event = a1, start_time = T2, close_time = T4, state = ?TASK_GOING
    }, #task{
        event = a2, start_time = T1, close_time = T3, state = ?TASK_GOING
    }, #task{
        event = a3, start_time = T1, close_time = T2, state = ?TASK_START
    }], max, []),
    ?assert(M5 == T1),
    ?assertEqual(false, lists:keyfind(a1, 1, L5)),
    ?assertEqual(false, lists:keyfind(a2, 1, L5)),
    ?assertEqual({a3, ?EVENT_START}, lists:keyfind(a3, 1, L5)),

    {M6, L6} = game_sequence_handler:get_intime_loop_tasks([#loop_task{
        event = a1, point = T1
    }], max, []),
    ?assert(M6 == T1),
    ?assertEqual([{a1, ?EVENT_LOOP}], L6),

    {M7, L7} = game_sequence_handler:get_intime_loop_tasks([#loop_task{
        event = a1, point = T1
    }, #loop_task{
        event = a2, point = T1
    }, #loop_task{
        event = a3, point = T2
    }], max, []),
    ?assert(M7 == T1),
    ?assertEqual({a1, ?EVENT_LOOP}, lists:keyfind(a1, 1, L7)),
    ?assertEqual({a2, ?EVENT_LOOP}, lists:keyfind(a2, 1, L7)),
    ?assertEqual(false, lists:keyfind(a3, 1, L7)),

    {M8, L8} = game_sequence_handler:get_intime_loop_tasks([#loop_task{
        event = a1, point = T3
    }, #loop_task{
        event = a2, point = T1
    }, #loop_task{
        event = a3, point = T2
    }], max, []),
    ?assert(M8 == T1),
    ?assertEqual(false, lists:keyfind(a1, 1, L8)),
    ?assertEqual({a2, ?EVENT_LOOP}, lists:keyfind(a2, 1, L8)),
    ?assertEqual(false, lists:keyfind(a3, 1, L8)),
    ok.

point_test() ->
    NowMsc = erlang:system_time(1000) rem 1000,
    T1 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,1},{1,0,0}})) + NowMsc,
    T2 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,1},{0,0,0}})) + NowMsc,
    T3 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,2},{1,0,0}})) + NowMsc,
    T4 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,12},{0,0,0}})) + NowMsc,
    T5 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,12},{1,0,0}})) + NowMsc,
    T6 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,19},{0,0,0}})) + NowMsc,
    T7 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,13},{1,0,0}})) + NowMsc,
    T8 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,1,18},{1,0,0}})) + NowMsc,
    T9 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,2,1},{1,0,0}})) + NowMsc,
    T10 = erlang:trunc(1000 * game_sequence_data:timestamp({{2021,2,1},{0,0,0}})) + NowMsc,

    P1 = game_sequence_handler:get_new_point(?TASK_LOOP_DAY, T1, 10000, T1),
    ?assertEqual(P1, T1 + 10000),


    P2 = game_sequence_handler:get_new_point(?TASK_LOOP_DAY, T2, 10000, T1),
    ?assert(P2 == T1),

    P3 = game_sequence_handler:get_new_point(?TASK_LOOP_DAYS, T1, 1, T1),
    P4 = game_sequence_handler:get_new_point(?TASK_LOOP_DAYS, T1, 1, T4),
    ?assert(P3 == T3),
    ?assert(P4 == T5),

    P5 = game_sequence_handler:get_new_point(?TASK_LOOP_WEEK, T4, 2, T4),
    P6 = game_sequence_handler:get_new_point(?TASK_LOOP_WEEK, T4, 2, T5),
    P7 = game_sequence_handler:get_new_point(?TASK_LOOP_WEEK, T4, 2, T7),
    P8 = game_sequence_handler:get_new_point(?TASK_LOOP_WEEK, T4, 2, T8),
    ?assert(P5 == T6),
    ?assert(P6 == T6),
    ?assert(P7 == T6),
    ?assert(P8 == T6),

    P9 = game_sequence_handler:get_new_point(?TASK_LOOP_MONTH, T1, 1, T1),
    P10 = game_sequence_handler:get_new_point(?TASK_LOOP_MONTH, T1, 1, T6),
    P11 = game_sequence_handler:get_new_point(?TASK_LOOP_MONTH, T1, 1, T10),
    ?assert(P9 == T9),
    ?assert(P10 == T9),
    ?assert(P11 == T9),
    ok.
