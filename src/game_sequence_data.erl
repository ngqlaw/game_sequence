-module(game_sequence_data).

-export([
    init/0,
    is_key_exist/1,
    add/2,
    get_value/1,
    del/1
]).
-export([
    datetime/2,
    timestamp/1,
    incr_month/2
]).

-define(TAB, ?MODULE).

init() ->
    ets:new(?TAB, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]).

is_key_exist(Key) ->
    ets:member(?TAB, Key).

add(Key, Value) ->
    ets:insert_new(?TAB, {Key, Value}).

get_value(Key) ->
    case ets:lookup(?TAB, Key) of
        [{_, Value}] -> Value;
        _ -> undefined
    end.

del(Key) ->
    ets:delete(?TAB, Key).

datetime(Timestamp, TimeUnit) ->
    %% OTP 21.0
    calendar:system_time_to_local_time(Timestamp, TimeUnit).

timestamp(Datetime) ->
    [DT] = calendar:local_time_to_universal_time_dst(Datetime),
    calendar:datetime_to_gregorian_seconds(DT) - 62167219200.

incr_month(Year, 12) ->
    {Year + 1, 1};
incr_month(Year, Month) when is_integer(Month) andalso Month > 0 andalso Month < 12 ->
    {Year, Month + 1}.