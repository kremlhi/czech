-module(p8_pretty).

-include("p8.hrl").
-include("czech.hrl").

-define(mwhen(X), ?X -> ??X).
-define(mwhen2(X, Y), ?X when Y -> ??X).

-export([print/1, print/3, pretty/1]).

pretty(L) ->
    [lists:flatten(pretty2(X)) || X <- L].

pretty2(#cmd{op=Op, param=Param}) ->
    io_lib:format("cmd ~s(~w) ~w", [codetostr(Op), Op, Param]);
pretty2(#cmd_tx{flags=Fx, src=Src, dest=Dest, op=Op, params=Px}) ->
    if Op =:= undefined ->
            io_lib:format("cmd_tx ~w ~s(~w) ~s(~w)",
                          [Fx, laddr(Src,src),Src, laddr(Dest,dest),Dest]);
       true ->
            io_lib:format("cmd_tx ~w ~s(~w) ~s(~w) ~s(~w) ~w",
                          [Fx, laddr(Src,src),Src, laddr(Dest,dest),Dest,
                           scecop(Op),Op, Px])
    end;
pretty2(#ind_ack{ack=Ack, op=Op}) ->
    io_lib:format("ind_ack ~w ~s(~w)", [Ack, codetostr(Op),Op]);
pretty2(#ind_err{type=Type, param=Param}) ->
    io_lib:format("ind_err ~w ~w", [Type, Param]);
pretty2(#ind_tx_ack{ack=Ack}) ->
    io_lib:format("ind_tx_ack ~w", [Ack]);
pretty2(#ind_rx{ack=Ack, src=Src, dest=Dest, op=Op, params=Px}) ->
    if Op =:= undefined ->
            io_lib:format("ind_rx ~w ~s(~w) ~s(~w)",
                          [Ack, laddr(Src,src),Src, laddr(Dest,dest),Dest]);
       true ->
            io_lib:format("ind_rx ~w ~s(~w) ~s(~w) ~s(~w) ~w",
                          [Ack, laddr(Src,src),Src, laddr(Dest,dest),Dest,
                           scecop(Op),Op, Px])
    end;
pretty2(Unexp) ->
    io_lib:format("unknown ~p", [Unexp]).

print(L) ->
    print(true, "", L).

print(false, _, _) ->
    ok;
print(_, Prefix, L) ->
    _ = [io:format("~s~s~n",[Prefix, X]) || X <- pretty(L)],
    ok.

-spec codetostr(integer()) -> nonempty_string().
codetostr(Code) ->
    case Code of
        ?mwhen(P8_CMD_NONE);
        ?mwhen(P8_CMD_PING);
        ?mwhen(P8_IND_ERR_TIMEOUT);
        ?mwhen(P8_IND_ERR_HIGH);
        ?mwhen(P8_IND_ERR_LOW);
        ?mwhen(P8_IND_RX_START);
        ?mwhen(P8_IND_RX_NEXT);
        ?mwhen(P8_IND_RX_FAILED);
        ?mwhen(P8_IND_ACK);
        ?mwhen(P8_IND_NACK);
        ?mwhen(P8_CMD_SET_ACK_MASK);
        ?mwhen(P8_CMD_TX);
        ?mwhen(P8_CMD_TX_EOM);
        ?mwhen(P8_CMD_TX_SET_IDLE);
        ?mwhen(P8_CMD_TX_SET_ACK_P);
        ?mwhen(P8_CMD_TX_SET_TIMEOUT);

        ?mwhen(P8_IND_TX_ACK);
        ?mwhen(P8_IND_TX_FAIL_LINE);
        ?mwhen(P8_IND_TX_NACK);
        ?mwhen(P8_IND_TX_TIMEOUT_D);
        ?mwhen(P8_IND_TX_TIMEOUT_L);

        ?mwhen(P8_CMD_FIRMWARE_VSN);
        ?mwhen(P8_CMD_START_BOOTLOADER);
        ?mwhen(P8_CMD_GET_BUILDDATE);
        ?mwhen(P8_CMD_SET_CONTROLLED);
        ?mwhen(P8_CMD_GET_AUTO_ENABLED);
        ?mwhen(P8_CMD_SET_AUTO_ENABLED);
        ?mwhen(P8_CMD_GET_DEF_LADDR);
        ?mwhen(P8_CMD_SET_DEF_LADDR);
        ?mwhen(P8_CMD_GET_LADDR_MASK);
        ?mwhen(P8_CMD_SET_LADDR_MASK);
        ?mwhen(P8_CMD_GET_PADDR);
        ?mwhen(P8_CMD_SET_PADDR);
        ?mwhen(P8_CMD_GET_DEV_TYPE);
        ?mwhen(P8_CMD_SET_DEV_TYPE);
        ?mwhen(P8_CMD_GET_HDMI_VSN);
        ?mwhen(P8_CMD_SET_HDMI_VSN);
        ?mwhen(P8_CMD_GET_OSD_NAME);
        ?mwhen(P8_CMD_SET_OSD_NAME);
        ?mwhen(P8_CMD_WRITE_EEPROM);
        ?mwhen(P8_CMD_GET_ADAPTER_TYPE);
        ?mwhen(P8_CMD_SET_ACTIVE_SOURCE);

        _ -> "UNKNOWN(" ++ integer_to_list(Code) ++ ")"
    end.

-spec scecop(integer()) -> nonempty_string().
scecop(Op) ->
    case Op of
        ?mwhen(CEC_ACTIVE_SOURCE);
        ?mwhen(CEC_IMAGE_VIEW_ON);
        ?mwhen(CEC_TEXT_VIEW_ON);
        ?mwhen(CEC_INACTIVE_SOURCE);
        ?mwhen(CEC_REQUEST_ACTIVE_SOURCE);
        ?mwhen(CEC_ROUTING_CHANGE);
        ?mwhen(CEC_ROUTING_INFORMATION);
        ?mwhen(CEC_SET_STREAM_PATH);
        ?mwhen(CEC_STANDBY);
        ?mwhen(CEC_RECORD_OFF);
        ?mwhen(CEC_RECORD_ON);
        ?mwhen(CEC_RECORD_STATUS);
        ?mwhen(CEC_RECORD_TV_SCREEN);
        ?mwhen(CEC_CLEAR_ANALOGUE_TIMER);
        ?mwhen(CEC_CLEAR_DIGITAL_TIMER);
        ?mwhen(CEC_CLEAR_EXTERNAL_TIMER);
        ?mwhen(CEC_SET_ANALOGUE_TIMER);
        ?mwhen(CEC_SET_DIGITAL_TIMER);
        ?mwhen(CEC_SET_EXTERNAL_TIMER);
        ?mwhen(CEC_SET_TIMER_PROGRAM_TITLE);
        ?mwhen(CEC_TIMER_CLEARED_STATUS);
        ?mwhen(CEC_TIMER_STATUS);
        ?mwhen(CEC_CEC_VERSION);
        ?mwhen(CEC_GET_CEC_VERSION);
        ?mwhen(CEC_GIVE_PHYSICAL_ADDRESS);
        ?mwhen(CEC_GET_MENU_LANGUAGE);
        ?mwhen(CEC_REPORT_PHYSICAL_ADDRESS);
        ?mwhen(CEC_SET_MENU_LANGUAGE);
        ?mwhen(CEC_DECK_CONTROL);
        ?mwhen(CEC_DECK_STATUS);
        ?mwhen(CEC_GIVE_DECK_STATUS);
        ?mwhen(CEC_PLAY);
        ?mwhen(CEC_GIVE_TUNER_DEVICE_STATUS);
        ?mwhen(CEC_SELECT_ANALOGUE_SERVICE);
        ?mwhen(CEC_SELECT_DIGITAL_SERVICE);
        ?mwhen(CEC_TUNER_DEVICE_STATUS);
        ?mwhen(CEC_TUNER_STEP_DECREMENT);
        ?mwhen(CEC_TUNER_STEP_INCREMENT);
        ?mwhen(CEC_DEVICE_VENDOR_ID);
        ?mwhen(CEC_GIVE_DEVICE_VENDOR_ID);
        ?mwhen(CEC_VENDOR_COMMAND);
        ?mwhen(CEC_VENDOR_COMMAND_WITH_ID);
        ?mwhen(CEC_VENDOR_REMOTE_BUTTON_DOWN);
        ?mwhen(CEC_VENDOR_REMOTE_BUTTON_UP);
        ?mwhen(CEC_SET_OSD_STRING);
        ?mwhen(CEC_GIVE_OSD_NAME);
        ?mwhen(CEC_SET_OSD_NAME);
        ?mwhen(CEC_MENU_REQUEST);
        ?mwhen(CEC_MENU_STATUS);
        ?mwhen(CEC_USER_CONTROL_PRESSED);
        ?mwhen(CEC_USER_CONTROL_RELEASE);
        ?mwhen(CEC_GIVE_DEVICE_POWER_STATUS);
        ?mwhen(CEC_REPORT_POWER_STATUS);
        ?mwhen(CEC_FEATURE_ABORT);
        ?mwhen(CEC_ABORT);
        ?mwhen(CEC_GIVE_AUDIO_STATUS);
        ?mwhen(CEC_GIVE_SYSTEM_AUDIO_MODE_STATUS);
        ?mwhen(CEC_REPORT_AUDIO_STATUS);
        ?mwhen(CEC_SET_SYSTEM_AUDIO_MODE);
        ?mwhen(CEC_SYSTEM_AUDIO_MODE_REQUEST);
        ?mwhen(CEC_SYSTEM_AUDIO_MODE_STATUS);
        ?mwhen(CEC_SET_AUDIO_RATE);

        %% CEC 1.4
        ?mwhen(CEC_START_ARC);
        ?mwhen(CEC_REPORT_ARC_STARTED);
        ?mwhen(CEC_REPORT_ARC_ENDED);
        ?mwhen(CEC_REQUEST_ARC_START);
        ?mwhen(CEC_REQUEST_ARC_END);
        ?mwhen(CEC_END_ARC);
        ?mwhen(CEC_CDC);
        ?mwhen(CEC_NONE);

        _ -> "UNKNOWN("++integer_to_list(Op)++")"
    end.

laddr(X, Dir) ->
    case X of
        ?mwhen(LADDR_TV);
        ?mwhen(LADDR_RECDEV1);
        ?mwhen(LADDR_RECDEV2);
        ?mwhen(LADDR_TUNER1);
        ?mwhen(LADDR_PLAYBDEV1);
        ?mwhen(LADDR_AUDIOSYS);
        ?mwhen(LADDR_TUNER2);
        ?mwhen(LADDR_TUNER3);
        ?mwhen(LADDR_PLAYBDEV2);
        ?mwhen(LADDR_RECDEV3);
        ?mwhen(LADDR_TUNER4);
        ?mwhen(LADDR_PLAYBDEV3);
        ?mwhen(LADDR_RESERVED1);
        ?mwhen(LADDR_RESERVED2);
        ?mwhen(LADDR_FREEUSE);
        ?mwhen2(LADDR_UNREG, Dir =:= src);
        ?mwhen2(LADDR_BROADCAST, Dir =:= dest);
        _ -> "UNKNOWN("++integer_to_list(X)++")"
    end.

-spec vendor(integer()) -> nonempty_string().
vendor(Id) ->
    case Id of
        16#000039 -> "TOSHIBA";
        16#0000F0 -> "SAMSUNG";
        16#0005CD -> "DENON";
        16#000678 -> "MARANTZ";
        16#000982 -> "LOEWE";
        16#0009B0 -> "ONKYO";
        16#000CB8 -> "MEDION";
        16#000CE7 -> "TOSHIBA2";
        16#001582 -> "PULSE EIGHT";
        16#001950 -> "HARMAN KARDON2";
        16#001A11 -> "GOOGLE";
        16#0020C7 -> "AKAI";
        16#002467 -> "AOC";
        16#008045 -> "PANASONIC";
        16#00903E -> "PHILIPS";
        16#009053 -> "DAEWOO";
        16#00A0DE -> "YAMAHA";
        16#00D0D5 -> "GRUNDIG";
        16#00E036 -> "PIONEER";
        16#00E091 -> "LG";
        16#08001F -> "SHARP";
        16#080046 -> "SONY";
        16#18C086 -> "BROADCOM";
        16#6B746D -> "VIZIO";
        16#8065E9 -> "BENQ";
        16#9C645E -> "HARMAN KARDON";
        _ -> "UNKNOWN(0x"++integer_to_list(Id,16)++")"
    end.
