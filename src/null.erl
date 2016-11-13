-module(null).
-compile(export_all).
-include("czech.hrl").

open() ->
    {ok,?MODULE}.

close(H) ->
    ok.

controlling_process(_H, _Cpid) ->
    ok.

send(_H, _Flags, Src, Src) -> %polling_message
    {error,tx_nack};
send(_H, _Flags, _Src, _Dest) ->
    ok.

send(_H, _Flags, _Src, _Dest, _Op, _Params) ->
    ok.

get_adapter_type(_H) ->
    ?DEV_PLAYBDEV.

get_builddate(_H) ->
    1478988990.

get_firmware_vsn(_H) ->
    3.

get_hdmi_vsn(_H) ->
    ?CEC_VSN_1_4.

get_paddr(_H) ->
    <<0,1>>. % 0.0.0.1

get_vendor(_H) ->
    <<0,21,130>>. %pulse-eight

set_ack_mask(_H, _, _) ->
    ok.

set_controlled(_H, _) ->
    ok.
