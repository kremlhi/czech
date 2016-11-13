-module(p8_SUITE).
-compile(export_all).
-include("czech.hrl").

all() ->
    [callbacks].

init_per_testcase(_TestCase, Config) ->
    DD = proplists:get_value(data_dir, Config),
    Root = lists:reverse(tl(tl(lists:reverse(filename:split(DD))))),
    {ok,Pid} = p8:open([{extprog,filename:join(Root ++ ["priv/echo"])}]),
    ct:pal("p8 state: ~p", [sys:get_state(Pid)]),
    [{userdata,Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(userdata, Config),
    p8:close(Pid).

callbacks(Config) ->
    Pid = proplists:get_value(userdata, Config),
    {ok,1}            = p8:get_adapter_type(Pid),
    {ok,Bdate}        = p8:get_builddate(Pid),
    {ok,FwVsn}        = p8:get_firmware_vsn(Pid),
    {ok,<<_,_>>}      = p8:get_paddr(Pid),
    {ok,?CEC_VSN_1_4} = p8:get_hdmi_vsn(Pid),
    {ok,<<0,21,130>>} = p8:get_vendor(Pid),
    true = Bdate > 0,
    true = FwVsn > 2,
    ok.
