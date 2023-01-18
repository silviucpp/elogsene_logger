-module(elogsene_logger_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = R = elogsene_logger_sup:start_link(),
    case logger:add_handlers(elogsene_logger) of
        ok ->
            R;
        {error, Reason} ->
            {error, {handlers_not_added, Reason}}
    end.

stop(_State) ->
    ok.
