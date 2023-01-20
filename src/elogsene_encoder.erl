-module(elogsene_encoder).

-export([
    encode/5
]).

encode(Event, LogseneAppToken, LogseneIndex, _Hostname, _Formatter) when is_binary(Event) ->
    IndexPayload = generate_index(LogseneAppToken, LogseneIndex, elogsene_utils:sha_hex(Event)),
    <<IndexPayload/binary, "\n", Event/binary, "\n">>;
encode(#{level := Level, meta := Meta} = Event, LogseneAppToken, LogseneIndex, Hostname, {FormatterModule, FormatterConfig}) ->

    {Module, _Function, _Arity} = maps:get(mfa, Meta, {null, null, null}),
    MessageBody = elogsene_utils:to_binary(FormatterModule:format(Event, FormatterConfig)),
    TimeOffset = maps:get(time_offset, FormatterConfig, $Z),
    TimeDesignator = maps:get(time_designator, FormatterConfig, $T),

    Payload = [
        {<<"severity">>, get_severity(Level)},
        {<<"host">>, Hostname},
        {<<"tags">>, elogsene_utils:to_binary(Module)},
        {<<"@timestamp">>, format_time(maps:get(time, Meta, 0), TimeOffset, TimeDesignator)},
        {<<"message">>, MessageBody}
    ],

    PayloadJson = elogsene_utils:safe_json_encode(Payload),
    <<(generate_index(LogseneAppToken, LogseneIndex, elogsene_utils:sha_hex(PayloadJson)))/binary, "\n", PayloadJson/binary, "\n">>.

generate_index(LogseneAppToken, LogseneIndex, Id) ->
    <<"{ \"index\" : { \"_index\": \"", LogseneAppToken/binary, "\", \"_type\" : \"", LogseneIndex/binary, "\", \"_id\" : \"", Id/binary, "\" } }">>.

get_severity(debug) ->
    <<"DEBUG">>;
get_severity(info) ->
    <<"INFO">>;
get_severity(notice) ->
    <<"NOTICE">>;
get_severity(warning) ->
    <<"WARNING">>;
get_severity(error) ->
    <<"ERROR">>;
get_severity(critical) ->
    <<"CRITICAL">>;
get_severity(alert) ->
    <<"ALERT">>;
get_severity(emergency) ->
    <<"EMERGENCY">>;
get_severity(Level) ->
    list_to_binary(string:to_upper(atom_to_list(Level))).

format_time(SysTime, TimeOffset, TimeDesignator) ->
    calendar:system_time_to_rfc3339(SysTime,[
        {unit,microsecond},
        {offset,TimeOffset},
        {time_designator,TimeDesignator}
    ]).
