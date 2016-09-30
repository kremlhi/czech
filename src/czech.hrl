%% logical address to devices
-define(LADDR_TV,        0).
-define(LADDR_RECDEV1,   1).
-define(LADDR_RECDEV2,   2).
-define(LADDR_TUNER1,    3).
-define(LADDR_PLAYBDEV1, 4).
-define(LADDR_AUDIOSYS,  5).
-define(LADDR_TUNER2,    6).
-define(LADDR_TUNER3,    7).
-define(LADDR_PLAYBDEV2, 8).
-define(LADDR_RECDEV3,   9).
-define(LADDR_TUNER4,    10).
-define(LADDR_PLAYBDEV3, 11).
-define(LADDR_RESERVED1, 12).
-define(LADDR_RESERVED2, 13).
-define(LADDR_FREEUSE,   14).
-define(LADDR_UNREG,     15).
-define(LADDR_BROADCAST, 15).

%% device types
-define(DEV_TV,       0).
-define(DEV_RECDEV,   1).
%% 2 reserved
-define(DEV_TUNER,    3).
-define(DEV_PLAYBDEV, 4).
-define(DEV_AUDIOSYS, 5).

-define(CEC_VSN_1_1,  0).
-define(CEC_VSN_1_2,  1).
-define(CEC_VSN_1_2A, 2).
-define(CEC_VSN_1_3,  3).
-define(CEC_VSN_1_3A, 4).
-define(CEC_VSN_1_4,  5).

%% DA: Directly addressed
%% BC: Broadcast
-define(CEC_FEATURE_ABORT,                   0). %16#00 DA
-define(CEC_IMAGE_VIEW_ON,                   4). %16#04 DA
-define(CEC_TUNER_STEP_INCREMENT,            5). %16#05 DA
-define(CEC_TUNER_STEP_DECREMENT,            6). %16#06 DA
-define(CEC_TUNER_DEVICE_STATUS,             7). %16#07 DA
-define(CEC_GIVE_TUNER_DEVICE_STATUS,        8). %16#08 DA
-define(CEC_RECORD_ON,                       9). %16#09 DA
-define(CEC_RECORD_STATUS,                  10). %16#0A DA
-define(CEC_RECORD_OFF,                     11). %16#0B DA
-define(CEC_TEXT_VIEW_ON,                   13). %16#0D BC
-define(CEC_RECORD_TV_SCREEN,               15). %16#0F DA
-define(CEC_GIVE_DECK_STATUS,               26). %16#1A DA
-define(CEC_DECK_STATUS,                    27). %16#1B DA
-define(CEC_SET_MENU_LANGUAGE,              50). %16#32 BC
-define(CEC_CLEAR_ANALOGUE_TIMER,           51). %16#33 DA
-define(CEC_SET_ANALOGUE_TIMER,             52). %16#34 DA
-define(CEC_TIMER_STATUS,                   53). %16#35 DA
-define(CEC_STANDBY,                        54). %16#36 DA,BC
-define(CEC_PLAY,                           65). %16#41 DA
-define(CEC_DECK_CONTROL,                   66). %16#42 DA
-define(CEC_TIMER_CLEARED_STATUS,           67). %16#43 DA
-define(CEC_USER_CONTROL_PRESSED,           68). %16#44 DA
-define(CEC_USER_CONTROL_RELEASED,          69). %16#45 DA
-define(CEC_GIVE_OSD_NAME,                  70). %16#46 DA
-define(CEC_SET_OSD_NAME,                   71). %16#47 DA
-define(CEC_SET_OSD_STRING,                100). %16#64 DA
-define(CEC_SET_TIMER_PROGRAM_TITLE,       103). %16#67 DA
-define(CEC_SYSTEM_AUDIO_MODE_REQUEST,     112). %16#70 DA
-define(CEC_GIVE_AUDIO_STATUS,             113). %16#71 DA
-define(CEC_SET_SYSTEM_AUDIO_MODE,         114). %16#72 DA,BC
-define(CEC_REPORT_AUDIO_STATUS,           122). %16#7A DA
-define(CEC_GIVE_SYSTEM_AUDIO_MODE_STATUS, 125). %16#7D DA
-define(CEC_SYSTEM_AUDIO_MODE_STATUS,      126). %16#7E DA
-define(CEC_ROUTING_CHANGE,                128). %16#80 BC
-define(CEC_ROUTING_INFORMATION,           129). %16#81 BC
-define(CEC_ACTIVE_SOURCE,                 130). %16#82 BC
-define(CEC_GIVE_PHYSICAL_ADDRESS,         131). %16#83 DA
-define(CEC_REPORT_PHYSICAL_ADDRESS,       132). %16#84 BC
-define(CEC_REQUEST_ACTIVE_SOURCE,         133). %16#85 BC
-define(CEC_SET_STREAM_PATH,               134). %16#86 BC
-define(CEC_DEVICE_VENDOR_ID,              135). %16#87 BC
-define(CEC_VENDOR_COMMAND,                137). %16#89 DA
-define(CEC_VENDOR_REMOTE_BUTTON_DOWN,     138). %16#8A DA,BC
-define(CEC_VENDOR_REMOTE_BUTTON_UP,       139). %16#8B DA,BC
-define(CEC_GIVE_DEVICE_VENDOR_ID,         140). %16#8C DA
-define(CEC_MENU_REQUEST,                  141). %16#8D DA
-define(CEC_MENU_STATUS,                   142). %16#8E DA
-define(CEC_GIVE_DEVICE_POWER_STATUS,      143). %16#8F DA
-define(CEC_REPORT_POWER_STATUS,           144). %16#90 DA
-define(CEC_GET_MENU_LANGUAGE,             145). %16#91 DA
-define(CEC_SELECT_ANALOGUE_SERVICE,       146). %16#92 DA
-define(CEC_SELECT_DIGITAL_SERVICE,        147). %16#93 DA
-define(CEC_SET_DIGITAL_TIMER,             151). %16#97 DA
-define(CEC_CLEAR_DIGITAL_TIMER,           153). %16#99 DA
-define(CEC_SET_AUDIO_RATE,                154). %16#9A DA
-define(CEC_INACTIVE_SOURCE,               157). %16#9D DA
-define(CEC_CEC_VERSION,                   158). %16#9E DA
-define(CEC_GET_CEC_VERSION,               159). %16#9F DA
-define(CEC_VENDOR_COMMAND_WITH_ID,        160). %16#A0 DA,BC
-define(CEC_CLEAR_EXTERNAL_TIMER,          161). %16#A1 DA
-define(CEC_SET_EXTERNAL_TIMER,            162). %16#A2 DA
-define(CEC_ABORT,                         255). %16#FF DA

%% CEC 1.4
-define(CEC_START_ARC,          192). %16#C0
-define(CEC_REPORT_ARC_STARTED, 193). %16#C1
-define(CEC_REPORT_ARC_ENDED,   194). %16#C2
-define(CEC_REQUEST_ARC_START,  195). %16#C3
-define(CEC_REQUEST_ARC_END,    196). %16#C4
-define(CEC_END_ARC,            197). %16#C5
-define(CEC_CDC,                248). %16#F8

%% when this opcode is set, no opcode will be sent to the device. this
%% is one of the reserved numbers
-define(CEC_NONE, 16#FD).

