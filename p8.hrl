-record(cmd, {op :: 0..63,
              param = <<>> :: binary()}).
-record(cmd_tx, {flags = [] :: [binary()],
                 src :: 0..15,
                 dest :: 0..15,
                 op :: byte(),
                 params = [] :: [binary()]}).
-record(ind_ack, {ack :: ok | {error,nack},
                  op :: 0..63}).
-record(ind_err, {type :: timeout | high | low,
                    param = <<>> :: binary()}).
-record(ind_tx_ack, {ack :: ok |
                            {error,tx_nack} |
                            {error,fail_line} |
                            {error,timeout_d} |
                            {error,timeout_l}}).
-record(ind_rx, {ack :: 0 | 1,
                 src :: 0..15,
                 dest :: 0..15,
                 op :: byte(),
                 params = [] :: [binary()]}).

-type cmd() :: #cmd{}.
-type cmd_tx() :: #cmd_tx{}.
-type ind_ack() :: #ind_ack{}.
-type ind_err() :: #ind_err{}.
-type ind_tx_ack() :: #ind_tx_ack{}.
-type ind_rx() :: #ind_rx{}.

-type packets() :: cmd() | cmd_tx() |
                   ind_ack() | ind_tx_ack() | ind_err() | ind_rx().


-define(BEG, 16#ff).
-define(END, 16#fe).

-define(P8_IND_ERR_TIMEOUT, 2).
-define(P8_IND_ERR_HIGH, 3).
-define(P8_IND_ERR_LOW, 4).

-define(P8_IND_RX_START, 5).
-define(P8_IND_RX_NEXT, 6).
-define(P8_IND_RX_FAILED, 7).

-define(P8_IND_ACK, 8).
-define(P8_IND_NACK, 9).

-define(P8_CMD_TX, 11).
-define(P8_CMD_TX_EOM, 12).
-define(P8_CMD_TX_SET_IDLE, 13).
%% set ack polarity to high when transmitting to the broadcast address
%% set ack polarity low when transmitting to any other address
-define(P8_CMD_TX_SET_ACK_P, 14).
-define(P8_CMD_TX_SET_TIMEOUT, 15).

-define(P8_IND_TX_ACK, 16).
-define(P8_IND_TX_FAIL_LINE, 17).
-define(P8_IND_TX_NACK, 18).
-define(P8_IND_TX_TIMEOUT_D, 19).
-define(P8_IND_TX_TIMEOUT_L, 20).

-define(P8_CMD_NONE, 0).
-define(P8_CMD_PING, 1).
-define(P8_CMD_SET_ACK_MASK, 10).
-define(P8_CMD_FIRMWARE_VSN, 21).
-define(P8_CMD_START_BOOTLOADER, 22).
-define(P8_CMD_GET_BUILDDATE, 23).
-define(P8_CMD_SET_CONTROLLED, 24).
-define(P8_CMD_GET_AUTO_ENABLED, 25).
-define(P8_CMD_SET_AUTO_ENABLED, 26).
-define(P8_CMD_GET_DEF_LADDR, 27).
-define(P8_CMD_SET_DEF_LADDR, 28).
-define(P8_CMD_GET_LADDR_MASK, 29).
-define(P8_CMD_SET_LADDR_MASK, 30).
-define(P8_CMD_GET_PADDR, 31).
-define(P8_CMD_SET_PADDR, 32).
-define(P8_CMD_GET_DEV_TYPE, 33).
-define(P8_CMD_SET_DEV_TYPE, 34).
-define(P8_CMD_GET_HDMI_VSN, 35).
-define(P8_CMD_SET_HDMI_VSN, 36).
-define(P8_CMD_GET_OSD_NAME, 37).
-define(P8_CMD_SET_OSD_NAME, 38).
-define(P8_CMD_WRITE_EEPROM, 39).
-define(P8_CMD_GET_ADAPTER_TYPE, 40).
-define(P8_CMD_SET_ACTIVE_SOURCE, 41).


-define(P8_FRAME_ACK, 16#40).
-define(P8_FRAME_EOM, 16#80).
