-module(czech_app).

-behaviour(application).

-export([start/0, start/1, stop/0, priv_dir/0]).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, czech).

start() -> start([]).

start(_) ->
    application:load(?APP),
    application:ensure_all_started(?APP).

stop() ->
    application:stop(?APP).

priv_dir() ->
    case code:priv_dir(?APP) of
        {error,bad_name} -> % hack when ?APP/ebin/ is not in path (discouraged)
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv");
        Dir when is_list(Dir) -> Dir
    end.

%%====================================================================
%% application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    czech_sup:start_link().

stop(_State) ->
    ok.
