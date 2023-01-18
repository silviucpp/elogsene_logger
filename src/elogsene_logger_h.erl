-module(elogsene_logger_h).

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-behaviour(gen_server).

-export([
    start_link/2,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(HACKNEY_SEMATEXT_POOL, sematext_pool).

-record(state, {
    hostname,
    formatter,
    logsene_url,
    logsene_app_token,
    logsene_index,
    upload_batch_max_size,
    upload_batch_inteval_ms,
    upload_failed_retry_count,
    upload_failed_retry_delay_ms,

    messages,
    msg_count,
    flush_timer
}).

start_link(Id, Opts) ->
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).

init([Config, Formatter]) ->
    {ok, update_config(Config, Formatter, #state {
        hostname = get_hostname(),
        messages = [],
        msg_count = 0}
    )}.

handle_call({update_config, Config, Formatter}, _From, State) ->
    {reply, ok, update_config(Config, Formatter, State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Msg}, State) ->
    {noreply, log_message(Msg, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(push_messages, #state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            {noreply, do_push_messages(State)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        {stop, shutdown} ->
            flush(State),
            wait_for_childrens(self(), 1000);
        _ ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

log_message(Msg, #state {
    messages = MessageList,
    msg_count = MessageCount,
    upload_batch_max_size = UploadBatchMaxSize,
    upload_batch_inteval_ms = UploadBatchMaxIntervalMs,
    flush_timer = FlushTimerRef
} = State) ->

    NewMessageCount = MessageCount + 1,

    case NewMessageCount >= UploadBatchMaxSize of
        true ->
            do_push_messages(State#state{messages = [Msg | MessageList], msg_count = NewMessageCount});
        _ ->
            NewTimer = case FlushTimerRef of
                undefined ->
                    erlang:send_after(UploadBatchMaxIntervalMs, self(), push_messages);
                _ ->
                    FlushTimerRef
            end,

            State#state{flush_timer = NewTimer, messages = [Msg | MessageList], msg_count = NewMessageCount}
    end.

do_push_messages(#state {
    upload_failed_retry_count = RetryCount,
    upload_failed_retry_delay_ms = RetryDelayMs,
    flush_timer = FlushTimerRef,
    formatter = Formatter,
    hostname = Hostname,
    logsene_app_token = LogseneAppToken,
    logsene_index = LogseneIndex,
    logsene_url = LogseneUrl,
    messages = Messages
} = State) ->

    case FlushTimerRef of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(FlushTimerRef)
    end,

    F = fun() ->
        ConvertFun = fun(Event, Accumulator) ->
            [elogsene_encoder:encode(Event, LogseneAppToken, LogseneIndex, Hostname, Formatter) | Accumulator]
        end,

        http_request(LogseneUrl, post, elogsene_utils:join(lists:foldl(ConvertFun, [], Messages), <<>>), RetryCount, RetryDelayMs)
    end,

    spawn_link(F),
    State#state{messages = [], msg_count = 0, flush_timer = undefined}.

flush(#state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            ?PRINT_MSG("~p:flush_messages remaining messages: ~p, ~n", [?MODULE, MsgCount]),
            do_push_messages(State);
        _ ->
            ?PRINT_MSG("~p:flush_messages no remaining messages to push ~n", [?MODULE]),
            State
    end.

http_request(_Url, _Method, _PayloadJson, 0, _IntervalMs) ->
    ok;
http_request(Url, Method, PayloadJson, Retries, IntervalMs) ->

    %?PRINT_MSG("#### Send: ~p ~n~n", [PayloadJson]),

    try
        Options = [
            {pool, ?HACKNEY_SEMATEXT_POOL}
        ],

        {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, Url, [], PayloadJson, Options),
        {ok, _Response} = hackney:body(ClientRef),

        case StatusCode of
            200 ->
                %?PRINT_MSG(<<"### Log sent response: ~p payload: ~p ~n">>, [Response, PayloadJson]),
                ok;
            _ ->
                ?PRINT_MSG("~p:http_request url: ~p failed status: ~p payload: ~p retries: ~p ~n", [?MODULE, Url, StatusCode, PayloadJson, Retries]),
                timer:sleep(IntervalMs),
                http_request(Url, Method, PayloadJson, Retries - 1, IntervalMs)
        end
    catch
        _ : Term ->
            ?PRINT_MSG("~p:http_request url: ~p exception: ~p payload: ~p retries: ~p ~n", [?MODULE, Url, Term, PayloadJson, Retries]),
            http_request(Url, Method, PayloadJson, Retries - 1, IntervalMs)
    end.

get_hostname() ->
    {ok, Host} = inet:gethostname(),
    elogsene_utils:to_binary(Host).

wait_for_childrens(Pid, Timeout) ->
    {links, LinkedProcesses} = process_info(Pid, links),
    NumberChildrens = length(LinkedProcesses) - 1,

    case NumberChildrens > 0 of
        true ->
            ?PRINT_MSG("wait for childrens: ~p ~n", [NumberChildrens]),
            timer:sleep(Timeout),
            wait_for_childrens(Pid, Timeout);
        _
            -> ok
    end.

create_http_pool(PoolOptions) ->
    case hackney_pool:find_pool(?HACKNEY_SEMATEXT_POOL) of
        undefined ->
            ok = hackney_pool:start_pool(?HACKNEY_SEMATEXT_POOL, PoolOptions);
        _ ->
            Timeout = proplists:get_value(timeout, PoolOptions, hackney_pool:timeout(?HACKNEY_SEMATEXT_POOL)),
            MaxConnections = proplists:get_value(max_connections, PoolOptions, hackney_pool:max_connections(?HACKNEY_SEMATEXT_POOL)),
            hackney_pool:set_timeout(?HACKNEY_SEMATEXT_POOL,Timeout),
            hackney_pool:set_max_connections(?HACKNEY_SEMATEXT_POOL, MaxConnections),
            ok
    end.

update_config(Config, Formatter, State) ->
    PoolOptions = maps:get(http_pool_options, Config, []),
    Logsene = maps:get(logsene, Config),

    ok = create_http_pool(PoolOptions),
    LogseneUrl = <<(maps:get(host, Logsene))/binary, "/_bulk">>,
    LogseneAppToken = maps:get(app_token, Logsene),
    LogseneIndex = maps:get(index, Logsene),

    State#state {
        formatter = Formatter,
        logsene_url = LogseneUrl,
        logsene_app_token = LogseneAppToken,
        logsene_index = LogseneIndex,
        upload_batch_max_size = maps:get(upload_batch_max_size, Config, 50),
        upload_batch_inteval_ms = maps:get(upload_batch_inteval_ms, Config, 5000),
        upload_failed_retry_count = maps:get(upload_failed_retry_count, Config, 3),
        upload_failed_retry_delay_ms = maps:get(upload_failed_retry_delay_ms, Config, 1000)
    }.
