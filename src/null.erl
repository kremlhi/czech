-module(null).
-compile(export_all).
-include("czech.hrl").

open(_Opts) ->
    {ok,?MODULE}.

close(_H) ->
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
    {ok,?DEV_PLAYBDEV}.

get_builddate(_H) ->
    {ok,1478988990}.

get_firmware_vsn(_H) ->
    {ok,3}.

get_hdmi_vsn(_H) ->
    {ok,?CEC_VSN_1_4}.

get_paddr(_H) ->
    {ok,<<0,1>>}. % 0.0.0.1

get_vendor(_H) ->
    {ok,<<0,21,130>>}. %pulse-eight

set_ack_mask(_H, _, _) ->
    ok.

set_controlled(_H, _) ->
    ok.
