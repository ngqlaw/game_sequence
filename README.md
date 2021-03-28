game_sequence
=====

An time sequence library for special game

Usage
-----

* 使用的时候需要先启动game_sequence应用
```
application:start(App, permanent)
```

or

```
.app.src文件

{applications, [game_sequence]}.
```

* 启动一个任务
```
game_sequence:start_task/1
```

* 启动任务前需要设置回调模块
```
%% 初始化，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(init(InitEvents::list()) -> {ok, State::any()}).
%% 事件处理，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(handle(Events::list(), TimeMark::integer(), State::any()) -> {ok, State::any()}).
%% 进程关闭，初始化选项有{handle, Handler}时，必须有该回调函数
-callback(terminate(Reason::term(), State::any()) -> ok).

%% 事务进程启动，初始化选项有{proc, ProcHandler}时，必须有该回调函数
-callback(start_link(InitEvents::list()) -> {ok, pid()}).

-optional_callbacks([init/1, handle/3, terminate/2, start_link/1]).
```

* 结束已启动任务
```
game_sequence:stop_task/1
```

* 子任务格式
```
{
    Event   ::  term(),                             % 事件标志

    Start   ::  integer(),                          % 开始时间戳（毫秒）

    End     ::  {day,   T :: integer()} |           % 按照T时长（毫秒）循环，T不大于一天的总时长

                {days,  D :: integer()} |           % D为循环的天数

                {week,  W :: integer()} |           % 周循环，W是循环开始的星期数 1~7，具体每天时间使用开始时间的天时间

                {month, D :: last | integer()} |    % 月循环，D是循环开始的号数 1~31，或者last表示每月的最后一天，具体每天时间使用开始时间的天时间
                
                integer()                           % 子任务存在的最终时间戳（毫秒）
}
```