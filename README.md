# elogsene_logger

[![Build Status](https://travis-ci.com/silviucpp/elogsene_logger.svg?branch=main)](https://travis-ci.com/github/silviucpp/elogsene_logger)
[![GitHub](https://img.shields.io/github/license/silviucpp/elogsene_logger)](https://github.com/silviucpp/elogsene_logger/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/elogsene_logger)](https://hex.pm/packages/elogsene_logger)

OTP [logger][1] backend that sends log events to [Sematext Logsene][2].

## Quick start

Add to your `rebar.config`

```erlang
{deps, [elogsene_logger]}.
```

add the logger handler settings to your `sys.config`

```erlang
[
    {elogsene_logger, [
        {logger, [
            {handler, sematext_logsene, elogsene_logger, #{
                level => info,
                config => #{
                    http_pool_options => [
                        {timeout, 15000},
                        {max_connections, 10}
                    ],

                    logsene => #{
                        host => <<"https://logsene-receiver.sematext.com">>,
                        app_token => <<"YOUR_API_TOKEN_HERE">>,
                        index => <<"YOUR_INDEX_NAME_HERE">>
                    },

                    upload_batch_max_size => 50,
                    upload_batch_inteval_ms => 5000,
                    upload_failed_retry_count => 3,
                    upload_failed_retry_delay_ms => 1000
                },
                formatter => {
                    logger_formatter, #{
                        single_line => true,
                        template => [pid, " ", mfa,":",line, " => ", msg],
                        time_offset => "Z"
                    }
                },
                filters => [
                    %{remote_group_leader, {fun logger_filters:remote_gl/2, stop}},
                    %{progress, {fun logger_filters:progress/2, stop}},
                    %{sasl, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}}
                ]
            }}
        ]}
    ]}
].

```

Or you can add it via logger API from your code:

```erlang
logger:add_handler(my_handler, elogsene_logger, #{...}).
```

**Note:** It's not currently possible to set `elogsene_logger` as `default` handler via `sys.config` (or add the handler there directly under the `kernel` app),
because `sys.config` is applied at `kernel` application start time and `elogsene_logger` application depends on `kernel` application (cyclic dependency). 

## Config properties

| Property               | Mandatory |    Description |
| --------------------- | :-----: | -------------- |
| http_pool_options      |        | [hackney][3] pool config. |
| logsene                | Y      | A map with 3 keys: `host` - logsene host, `app_token` - your application token, `index` - the index name.|
| upload_batch_max_size |         | *Default: 50*. The events are sent in batches. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed.|
| upload_batch_inteval_ms|        | *Default: 5000*. Number of milliseconds we can wait for events to accumulate. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed.|
| upload_failed_retry_count |     | *Default: 3*. In case a batch sending operation failed, how many times we retry to resubmit.|
| upload_failed_retry_delay_ms|   | *Default: 1000*. The delay between resubmitting failed batches.|

- Beside this custom settings, all other standard `logger:handler_config()` properties are accepted (`level`, `filters`, `formatter`).
- Multiple instances of `elogsene_logger` handler can be started. 

[1]:https://www.erlang.org/doc/apps/kernel/logger_chapter.html
[2]:https://sematext.com/logsene/
[3]:https://github.com/benoitc/hackney
