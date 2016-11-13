-module(handlers_SUITE).
-compile(export_all).
-include("czech.hrl").

all() ->
    [keypress_up, activate, change_volume].

init_per_testcase(_Tc, Config) ->
    {ok,_Pid} = czech:start_link(null),
    czech:add_handler(self()),
    ct:pal("czech state: ~p", [sys:get_state(czech)]),
    Config.

end_per_testcase(_Tc, _Config) ->
    czech:stop().

keypress_up(_Config) ->
    czech ! {self(),#cec{flags  = [],
                         src    = 0,dest = 4,
                         op     = ?CEC_USER_CONTROL_PRESSED,
                         params = <<1>>}},
    czech ! {self(),#cec{flags  = [],
                         src    = 0,dest = 4,
                         op     = ?CEC_USER_CONTROL_RELEASED,
                         params = <<>>}},
    receive
        {keypress,_Pid,up} = M ->
            ct:pal("got message: ~w", [M]),
            ok
        after 1000 ->
            ct:fail("message not received")
    end,
    receive
        {keyrel,_Pid2} = M2 ->
            ct:pal("got message: ~w", [M2]),
            ok
        after 1000 ->
            ct:fail("message not received")
    end.

activate(_Config) ->
    czech ! {self(),#cec{flags  = [],
                         src    = 0,dest = 4,
                         op     = ?CEC_ROUTING_CHANGE,
                         params = <<0,0,0,1>>}},
    receive
        {activate,_Pid} = M ->
            ct:pal("got message: ~w", [M]),
            ok
        after 1000 ->
            ct:fail("message not received")
    end.

change_volume(_Config) ->
    czech ! {self(),#cec{flags  = [],
                         src    = 0,dest = 4,
                         op     = ?CEC_REPORT_AUDIO_STATUS,
                         params = <<1:1,55:7>>}},
    receive
        {volume,_Pid,true,55} = M ->
            ct:pal("got message: ~w", [M]),
            ok
        after 1000 ->
            ct:fail("message not received")
    end.
