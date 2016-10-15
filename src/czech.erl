-module(czech).
-behaviour(gen_server).

-export([start_link/1, start_link/2, stop/0,
         subscribe/1, send/3, broadcast/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("czech.hrl").

-import(error_logger, [error_msg/2, warning_msg/2, info_msg/2]). %loooong names

-define(SERVER, ?MODULE).

-record(state, {subs   = []           :: [pid()],
                mod                   :: module(),
                adpt                  :: pid(),
                active                :: paddr(),
                laddr  = ?LADDR_UNREG :: laddr(),
                devs   = []           :: [dev()]}).

-type state() :: #state{}.

-record(dev, {laddr   :: src(),
              paddr   :: paddr(),
              vendor  :: binary(),
              osdname :: binary(),
              devtype :: devtype(),
              cecvsn  :: cecvsn()}).

-record(cec, {flags  :: [flag()],
              src    :: src(),
              dest   :: dest(),
              op     :: op(),
              params :: params()}).

-type dev() :: #dev{}.

-type paddr()   :: <<_:2>>. % n.n.n.n
-type laddr()   :: 0..14.
-type src()     :: laddr() | ?LADDR_UNREG.
-type dest()    :: laddr() | ?LADDR_BROADCAST.
-type devtype() :: ?DEV_TV | ?DEV_RECDEV | ?DEV_TUNER
                 | ?DEV_PLAYBDEV | ?DEV_AUDIOSYS.
-type cecvsn()  :: ?CEC_VSN_1_1 | ?CEC_VSN_1_2  | ?CEC_VSN_1_2A
                 | ?CEC_VSN_1_3 | ?CEC_VSN_1_3A | ?CEC_VSN_1_4.

-type bint()    :: 0 | 1.
-type timeval() :: 0..255.
-type flag()    :: {idle,timeval()} | {timeout,timeval()} | {ack_p,bint()}.
-type op()      :: 0..255.
-type params()  :: binary().

-type cec() :: #cec{}.

%%====================================================================
%% API
%%====================================================================

start_link(Mod) ->
    start_link(Mod, []).

start_link(Mod, Opts) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [Mod | Opts],
                          [{timeout,5000}]).

stop()                 -> gen_server:stop(?SERVER).
subscribe(Pid)         -> gen_server:call(?SERVER, {subscribe,Pid}).
send(Dest, Op, Params) -> gen_server:call(?SERVER, {send,Dest,Op,Params}).
broadcast(Op, Params)  -> gen_server:call(?SERVER, {broadcast,Op,Params}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mod | _Opts]) ->
    process_flag(trap_exit, true),
    case Mod:open() of
        {ok,H} ->
            State = init_adpt(#state{mod=Mod, adpt=H}),
            handle_broadcast(State, ?CEC_REQUEST_ACTIVE_SOURCE, <<>>),
            {ok,State};
        {error,Reason} ->
            {stop,Reason}
    end.

init_adpt(#state{mod = Mod, adpt = H} = State) ->
    ok      = check_adapter(State),
    Devtype = Mod:get_adapter_type(H),
    Vendor  = Mod:get_vendor(H),
    ok      = set_idle(State, ?LADDR_UNREG, ?LADDR_TV, 3),
    Laddr   = set_laddr(State),
    Paddr   = Mod:get_paddr(H),
    Vsn     = Mod:get_hdmi_vsn(H),
    ok      = Mod:set_ack_mask(H, 1, 0),
    Dev     = #dev{laddr   = Laddr,
                   paddr   = Paddr,
                   vendor  = Vendor,
                   osdname = list_to_binary(?MODULE_STRING),
                   devtype = Devtype,
                   cecvsn  = Vsn},
    State#state{laddr = Laddr,
                devs  = [Dev]}.

check_adapter(#state{mod = Mod, adpt = H}) ->
    FwVsn = Mod:get_firmware_vsn(H),
    true = FwVsn > 2,
    ok = Mod:set_controlled(H, 1),
    true = Mod:get_builddate(H) > 0,
    ok.

handle_call({subscribe,Pid}, _, #state{subs = Subs} = State) ->
    link(Pid),
    Reply = {ok,self()},
    Subs2 = lists:usort([Pid | Subs]),
    {reply,Reply,State#state{subs = Subs2}};
handle_call({send,Dest,Op,Params}, _, State) ->
    Reply = handle_send(State, Dest, Op, Params),
    {reply,Reply,State};
handle_call({broadcast,Op,Params}, _, State) ->
    Reply = handle_broadcast(State, Op, Params),
    {reply,Reply,State}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast(_, State) ->
    {noreply,State}.

-spec handle_info({pid(),cec()},state()) -> {'noreply',state()}.
handle_info({_, #cec{dest = Laddr,
                     op = ?CEC_USER_CONTROL_PRESSED,
                     params = <<Key>>}},
            #state{laddr = Laddr,
                   subs = Subs} = State) ->
    _ = [handle_keypress(Pid, keycode(Key)) || Pid <- Subs],
    {noreply,State};
handle_info({_, #cec{dest = Laddr,
                     op = ?CEC_USER_CONTROL_RELEASED}},
            #state{laddr = Laddr, subs = Subs} = State) ->
    _ = [handle_keyrel(Pid) || Pid <- Subs],
    {noreply,State};
handle_info({_, #cec{op = ?CEC_REPORT_AUDIO_STATUS,
                     params = <<Mute:1,Volume:7>>}},
            #state{subs = Subs} = State) ->
    _ = [handle_volume(Pid, Mute =:= 1, Volume) || Pid <- Subs],
    {noreply,State};
handle_info({_, #cec{src = Src,
                     dest = Laddr,
                     op = ?CEC_GIVE_DECK_STATUS,
                     params = _Params}}, % on | off | once
            #state{laddr = Laddr} = State) ->
    handle_send(State, Src, ?CEC_DECK_STATUS, <<26>>), %stop
    {noreply,State};
handle_info({_, #cec{src = Src,
                     dest = Laddr,
                     op = ?CEC_GET_CEC_VERSION,
                     params = _Params}}, % on | off | once
            #state{laddr = Laddr, devs = Devs} = State) ->
    Vsn = cecvsn(Laddr, Devs),
    handle_send(State, Src, ?CEC_CEC_VERSION, <<Vsn>>),
    {noreply,State};

handle_info({_, #cec{op = ?CEC_SET_STREAM_PATH,
                     params = To}},
            #state{laddr = Laddr, devs = Devs} = State) ->
    case paddr(Laddr, Devs) of
        To ->
            handle_broadcast(State, ?CEC_ACTIVE_SOURCE, To);
        _ ->
            ok
    end,
    {noreply,State#state{active = To}};
handle_info({_, #cec{src = Src,
                     op = ?CEC_ROUTING_CHANGE,
                     params = <<From:2/binary,To:2/binary>>}},
            #state{laddr = Laddr, devs = Devs, subs = Subs} = State) ->
    case paddr(Laddr, Devs) of
        To ->
            %% FIXME: why doesn't this activate the adapter when the TV starts?
            _ = [handle_activate(X) || X <- Subs],
            handle_broadcast(State, ?CEC_ACTIVE_SOURCE, To),
            handle_send(State, Src, ?CEC_TEXT_VIEW_ON, <<>>),
            handle_send(State, Src, ?CEC_MENU_STATUS, <<0>>);
       From ->
            handle_broadcast(State, ?CEC_INACTIVE_SOURCE, From);
       <<_,_>> ->
            ok
    end,
    {noreply,State#state{active = To}};
handle_info({_, #cec{op = ?CEC_ACTIVE_SOURCE,
                     params = To}},
            #state{laddr = Laddr,
                   devs = Devs,
                   active = Active} = State) ->
    Paddr = paddr(Laddr, Devs),
    if Active == Paddr, To == <<0,0>> ->
            handle_broadcast(State, ?CEC_INACTIVE_SOURCE, Paddr);
       true ->
            ok
    end,
    {noreply,State#state{active = To}};

handle_info({_, #cec{src = Src,
                     dest = Laddr,
                     op = ?CEC_GIVE_OSD_NAME}},
            #state{laddr = Laddr, devs = Devs} = State) ->
    handle_send(State, Src, ?CEC_SET_OSD_NAME, osdname(Laddr, Devs)),
    {noreply,State};
handle_info({_, #cec{dest = Laddr,
                     op = ?CEC_GIVE_PHYSICAL_ADDRESS}},
            #state{laddr = Laddr, devs = Devs} = State) ->
    Params = <<(paddr(Laddr, Devs))/binary,(devtype(Laddr, Devs))>>,
    handle_broadcast(State, ?CEC_REPORT_PHYSICAL_ADDRESS, Params),
    {noreply,State};
handle_info({_, #cec{dest = Laddr,
                     op = ?CEC_GIVE_DEVICE_VENDOR_ID}},
            #state{laddr = Laddr, devs = Devs} = State) ->
    handle_broadcast(State, ?CEC_DEVICE_VENDOR_ID, vendor(Laddr, Devs)),
    {noreply,State};
handle_info({_, #cec{src = Src,
                     dest = Laddr,
                     op = ?CEC_GIVE_DEVICE_POWER_STATUS}},
            #state{laddr = Laddr} = State) ->
    handle_send(State, Src, ?CEC_REPORT_POWER_STATUS, <<0>>), %on
    {noreply,State};

handle_info({_, #cec{dest = Laddr, op = undefined}},
            #state{laddr = Laddr} = State) ->
    polling_message(State, Laddr),               %pong
    {noreply,State};
handle_info({_, #cec{op = undefined}}, State) -> %ping from someone else
    {noreply,State};

%% TODO: implement ?CEC_DEVICE_VENDOR_COMMAND and ?CEC_VENDOR_COMMAND_WITH_ID?
%% handle_info({_, #cec{src = Src,
%%                      dest = Laddr,
%%                      op = Op}},
%%             #state{laddr = Laddr} = State) ->
%%     handle_send(State, Src, ?CEC_FEATURE_ABORT, <<Op,0>>),
%%     {noreply,State};

handle_info({_, #cec{src = Src,
                     op = ?CEC_SET_OSD_NAME,
                     params = Osdname}},
            #state{devs = Devs} = State) ->
    [D | Devs2] = take_dev(Src, Devs),
    Devs3 = [D#dev{osdname = Osdname} | Devs2],
    {noreply,State#state{devs = Devs3}};
handle_info({_, #cec{src = Src,
                     op = ?CEC_REPORT_PHYSICAL_ADDRESS,
                     params = <<Paddr:2/binary,DT>>}},
            #state{devs = Devs} = State) ->
    [D | Devs2] = take_dev(Src, Devs),
    Devs3 = [D#dev{paddr = Paddr, devtype = DT} | Devs2],
    {noreply,State#state{devs = Devs3}};
handle_info({_, #cec{src = Src,
                     op = ?CEC_DEVICE_VENDOR_ID,
                     params = VendorId}},
            #state{devs = Devs} = State) ->
    [D | Devs2] = take_dev(Src, Devs),
    Devs3 = [D#dev{vendor = VendorId} | Devs2],
    {noreply,State#state{devs = Devs3}};
handle_info({_, #cec{src = Src,
                     op = ?CEC_CEC_VERSION,
                     params = <<Vsn>>}},
            #state{devs = Devs} = State) ->
    [D | Devs2] = take_dev(Src, Devs),
    Devs3 = [D#dev{cecvsn = Vsn} | Devs2],
    {noreply,State#state{devs = Devs3}};
handle_info({_, #cec{}} = M, State) ->
    warning_msg("unhandled message: ~w~n", [M]),
    {noreply,State};

handle_info({'EXIT',H,Reason}, #state{adpt = H} = State) ->
    error_msg("adapter ~w died (~p), closing down for today~n", [H, Reason]),
    {stop,Reason,State};
handle_info({'EXIT',Pid,Reason}, #state{subs = Subs} = State) ->
    info_msg("subscriber ~w died (~p), removing~n", [Pid, Reason]),
    {noreply,State#state{subs = Subs -- [Pid]}};
handle_info(M, State) ->
    warning_msg("unknown message: ~w~n", [M]),
    {noreply,State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, #state{devs = Devs} = State, _Extra) ->
    Devs2 = [upgrade_devs(X) || X <- Devs],
    State2 = upgrade_state(State#state{devs = Devs2}),
    {ok,State2}.

-spec terminate(any(),state()) -> no_return().
terminate(Reason, #state{mod   = Mod,
                         adpt  = H,
                         laddr = Laddr,
                         devs  = Devs} = State) ->
    warning_msg("GOING DOWN D: ~p~n", [Reason]),
    handle_send(State, ?LADDR_TV, ?CEC_INACTIVE_SOURCE, paddr(Laddr, Devs)),
    Mod:set_ack_mask(H, 0, 0),
    Mod:set_controlled(H, 0),
    Mod:close(H),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec polling_message(state(), laddr()) -> 'ok'.
polling_message(#state{mod = Mod, adpt = H}, Laddr) ->
    Mod:send(H, [], Laddr, Laddr).

-spec set_idle(state(), src(), dest(), integer()) -> 'ok'.
set_idle(#state{mod = Mod, adpt = H}, Src, Dest, Time) ->
    Mod:send(H, [{idle,Time}], Src, Dest).

-spec handle_broadcast(state(), integer(), [binary()]) ->
                              'ok' | {'error',any()}.
handle_broadcast(#state{mod = Mod, adpt = H, laddr = Src}, Op, Params) ->
    Mod:send(H, [{ack_p,1}], Src, ?LADDR_BROADCAST, Op, Params).

-spec handle_send(state(), dest(), integer(), [binary()]) ->
                         'ok' | {'error',any()}.
handle_send(#state{mod = Mod, adpt = H, laddr = Src}, Dest, Op, Params) ->
    Mod:send(H, [{ack_p,0}], Src, Dest, Op, Params).

-spec set_laddr(state()) -> laddr().
set_laddr(State) ->
    L = [?LADDR_PLAYBDEV1,
         ?LADDR_PLAYBDEV2,
         ?LADDR_PLAYBDEV3],
    set_laddr(State, L).

set_laddr(State, [Laddr | T]) ->
    case polling_message(State, Laddr) of
        ok -> set_laddr(State, T);
        {error,tx_nack} ->
            {error,tx_nack} = polling_message(State, Laddr),
            Laddr
    end.

paddr(Laddr, Devs) ->
    [#dev{paddr = Paddr} | _] = take_dev(Laddr, Devs),
    Paddr.

vendor(Laddr, Devs) ->
    [#dev{vendor = Vendor} | _] = take_dev(Laddr, Devs),
    Vendor.

osdname(Laddr, Devs) ->
    [#dev{osdname = Osdname} | _] = take_dev(Laddr, Devs),
    Osdname.

devtype(Laddr, Devs) ->
    [#dev{devtype = DT} | _] = take_dev(Laddr, Devs),
    DT.

cecvsn(Laddr, Devs) ->
    [#dev{cecvsn = Vsn} | _] = take_dev(Laddr, Devs),
    Vsn.

take_dev(Laddr, Devs) ->
    case lists:keytake(Laddr, #dev.laddr, Devs) of
        {value,D,Devs2} ->
            [D | Devs2];
        false ->
            [#dev{laddr = Laddr} | Devs]
    end.

handle_keypress(Pid, Key) ->
    Pid ! {keypress,self(),Key}.

handle_keyrel(Pid) ->
    Pid ! {keyrel,self()}.

handle_volume(Pid, Mute, Volume) ->
    Pid ! {volume,self(),Mute,Volume}.

handle_activate(Pid) ->
    Pid ! {activate,self()}.

keycode(Code) ->
    case Code of
        0 -> enter; %ok
        1 -> up;
        2 -> down;
        3 -> left;
        4 -> right;
        13 -> return;
        %% 14-31 reserved
        32 -> d0;
        33 -> d1;
        34 -> d2;
        35 -> d3;
        36 -> d4;
        37 -> d5;
        38 -> d6;
        39 -> d7;
        40 -> d8;
        41 -> d9;
        44 -> cancel; %exit
        48 -> ch_up;
        49 -> ch_down;
        %% 57-63 reserved
        64 -> power;
        65 -> volup;
        66 -> voldown;
        68 -> play;
        69 -> stop;
        70 -> pause;
        72 -> rew;
        73 -> ff;
        75 -> skip_next;
        76 -> skip_prev;
        %% 86-95 reserved
        %% 110-112 reserved
        113 -> f1_blue;
        114 -> f2_red;
        115 -> f3_green;
        116 -> f4_yellow;
        117 -> f5;
        %% 119-255 reserved
        X -> X
    end.

%% quick and dirty way to expand/decrease records (always add/remove to the end)
upgrade_devs(#dev{} = D) ->
    D;
upgrade_devs(D) when element(1, D) == dev ->
    upgrade_rec(tuple_to_list(D), 1, #dev{}).

upgrade_state(#state{} = S) ->
    S;
upgrade_state(S) when element(1, S) == state ->
    upgrade_rec(tuple_to_list(S), 1, #state{}).

upgrade_rec([H | T], N, R) when N =< tuple_size(R) ->
    R2 = setelement(N, R, H),
    upgrade_rec(T, N + 1, R2);
upgrade_rec(_, _, R) ->
    R.
