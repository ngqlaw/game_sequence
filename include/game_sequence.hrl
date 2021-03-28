-ifndef(GAME_SEQUENCE_H).
-define(GAME_SEQUENCE_H, true).

-define(MINUTE_M_SECS,  60000).
-define(DAY_SECS,       86400).
-define(DAY_M_SECS,     86400000).
-define(H_DAY_M_SECS,   43200000).

%% 消息事件
-define(EVENT_START,    start).
-define(EVENT_CLOSE,    close).
-define(EVENT_LOOP,     loop).
-define(EVENT_REMOVE,   remove).
-define(EVENT_FINISH,   finish).

%% 任务循环时间类型
-define(TASK_LOOP_DAY,         day).
-define(TASK_LOOP_DAYS,        days).
-define(TASK_LOOP_WEEK,        week).
-define(TASK_LOOP_MONTH,       month).

%% 任务状态
-define(TASK_START,     start).
-define(TASK_GOING,     going).
-define(TASK_CLOSE,     close).

-record(task, {
    event = undefined :: term(),
    start_time = 0 :: integer(),
    close_time = 0 :: integer(),
    state = start :: start | close
}).

-record(loop_task, {
    event = undefined :: term(),
    point = 0 :: integer(),
    type = undefined :: undefined | day | days | week | month,
    type_mark = 0 :: integer()
}).

-endif.