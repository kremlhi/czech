-module(czech).
-behaviour(gen_server).

-export([start_link/0, start_link/1, start_link/2, stop/0,
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
                devtype               :: devtype(),
                laddr  = ?LADDR_UNREG :: src(),
                paddr                 :: paddr(),
                vendor = <<0,21,130>>, %pulse-eight
                active                :: paddr(),
                devs   = []           :: [dev()]}).

-type state() :: #state{}.

-record(dev, {laddr :: laddr()}).

-type dev() :: #dev{}.

-type devtype() :: integer().
-type paddr()   :: <<_:2>>. % n.n.n.n
-type laddr()   :: 0..14.
-type src()     :: laddr() | ?LADDR_UNREG.
-type dest()    :: laddr() | ?LADDR_BROADCAST.

-type timeval() :: 0..255.
-type flag()    :: {idle,timeval()} | {timeout,timeval()} | {ack_p,timeval()}.
-type op()      :: 0..255.
-type param()   :: binary().

-type cec() :: {cec,{[flag()],src(),dest(),op(),[param()]}}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(p8).

start_link(Mod) ->
    start_link(Mod, []).

start_link(Mod, Opts) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [Mod | Opts],
                          [{timeout,5000}]).

stop()                 -> gen_server:stop(?SERVER).
subscribe(Pid)         -> gen_server:call(?SERVER, {subscribe,Pid}).
send(Dest, Op, Params) -> gen_server:cast(?SERVER, {send,Dest,Op,Params}).
broadcast(Op, Params)  -> gen_server:cast(?SERVER, {broadcast,Op,Params}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mod | _Opts]) ->
    process_flag(trap_exit, true),
    case Mod:open() of
        {ok,H} ->
            State = init_adpt(#state{mod=Mod, adpt=H}),
            {ok,State};
        {error,Reason} ->
            {stop,Reason}
    end.

init_adpt(#state{mod=Mod, adpt=H} = State) ->
    ok = check_adapter(State),
    DevType = Mod:get_adapter_type(H),
    ok = set_idle(State, ?LADDR_UNREG, ?LADDR_TV, 3),
    Laddr = set_laddr(State),
    Paddr = Mod:get_paddr(H),
    ok = Mod:set_ack_mask(H, 1, 0),
    State#state{devtype = DevType,
                laddr   = Laddr,
                paddr   = Paddr}.

check_adapter(#state{mod=Mod, adpt=H}) ->
    FwVsn = Mod:get_firmware_vsn(H),
    true = FwVsn > 2,
    ok = Mod:set_controlled(H, 1),
    true = Mod:get_builddate(H) > 0,
    ok.

handle_call({subscribe,Pid}, _, #state{subs=Subs} = State) ->
    link(Pid),
    {reply,{ok,self()},State#state{subs=lists:usort([Pid | Subs])}};
handle_call({update_laddr}, _, State) ->
    Laddr = set_laddr(State),
    {reply,Laddr,State#state{laddr=Laddr}}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast({send,Dest,Op,Params}, State) ->
    handle_send(State, Dest, Op, Params),
    {noreply,State};
handle_cast({broadcast,Op,Params}, State) ->
    handle_broadcast(State, Op, Params),
    {noreply,State}.

-spec handle_info({pid(),cec()},state()) -> {'noreply',state()}.
handle_info({_, {cec,{_,_,_,?CEC_USER_CONTROL_PRESSED,[<<Key>>]}}},
            #state{subs=Subs} = State) ->
    _ = [handle_keypress(Pid, keycode(Key)) || Pid <- Subs],
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_USER_CONTROL_RELEASED,[]}}},
            #state{subs=Subs} = State) ->
    _ = [handle_keyrel(Pid) || Pid <- Subs],
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_REPORT_AUDIO_STATUS,[<<M:1,Volume:7>>]}}},
            #state{subs=Subs} = State) ->
    Mute = M =:= 1,
    _ = [handle_volume(Pid, Mute, Volume) || Pid <- Subs],
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_ROUTING_CHANGE,Params}}},
            #state{paddr=Paddr} = State) ->
    <<_From:2/binary,To:2/binary>> = list_to_binary(Params),
    if To =:= Paddr ->
            timer:sleep(3), % >7 nominal data bit periods
            handle_send(State, ?LADDR_TV, ?CEC_TEXT_VIEW_ON, []),
            Params2 = [<<X>> || <<X>> <= Paddr],
            handle_broadcast(State, ?CEC_ACTIVE_SOURCE, Params2),
%% FIXME: should move this to only respond to requests
            handle_send(State, ?LADDR_TV, ?CEC_MENU_STATUS, [<<0>>]),
            os:cmd("caffeinate -u -t 1");
       true ->
            ok
    end,
    {noreply,State#state{active=To}};
handle_info({_, {cec,{_,_,_,?CEC_GIVE_PHYSICAL_ADDRESS,[]}}},
            #state{paddr   = Paddr,
                   devtype = DevType} = State) ->
    Params = [<<X>> || <<X>> <= Paddr] ++ [<<DevType>>],
    handle_broadcast(State, ?CEC_REPORT_PHYSICAL_ADDRESS, Params),
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_GIVE_DEVICE_VENDOR_ID,[]}}},
            #state{vendor=VendorId} = State) ->
    Params = [<<X>> || <<X>> <= VendorId],
    handle_broadcast(State, ?CEC_DEVICE_VENDOR_ID, Params),
    {noreply,State};
handle_info({_, {cec,{_,_,Laddr,undefined,[]}}}, #state{laddr=Laddr} = State)
  when Laddr =/= ?LADDR_UNREG ->
    polling_message(State, Laddr),
    {noreply,State};
handle_info({_, {cec,{_,Src,_,_,_}}} = M, #state{devs=Devs} = State) ->
    info_msg("unhandled message: ~w~n", [M]),
    [D | Devs2] = take_dev(Src, Devs),
    {noreply,State#state{devs=[D | Devs2]}};
handle_info({'EXIT',H,Reason}, #state{adpt=H} = State) ->
    error_msg("adapter ~w died (~p), closing down for today~n", [H, Reason]),
    {stop,Reason,State};
handle_info({'EXIT',Pid,Reason}, #state{subs=Subs} = State) ->
    info_msg("subscriber ~w died (~p), removing~n", [Pid, Reason]),
    {noreply,State#state{subs=Subs -- [Pid]}};
handle_info(M, State) ->
    warning_msg("unknown message: ~w~n", [M]),
    {noreply,State}.

take_dev(Laddr, Devs) ->
    case lists:keytake(Laddr, #dev.laddr, Devs) of
        {value,D,Devs2} ->
            [D | Devs2];
        false ->
            [#dev{laddr=Laddr} | Devs]
    end.

handle_keypress(Pid, Key) ->
    Pid ! {keypress,self(),Key}.

handle_keyrel(Pid) ->
    Pid ! {keyrel,self()}.

handle_volume(Pid, Mute, Volume) ->
    Pid ! {volume,self(),Mute,Volume}.

keycode(Code) ->
    case Code of
        0 -> enter; %ok
        1 -> up;
        2 -> down;
        3 -> left;
        4 -> right;
        13 -> return;
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
        65 -> volup;
        66 -> voldown;
        68 -> play;
        69 -> stop;
        70 -> pause;
        72 -> rew;
        73 -> ff;
        75 -> skip_next;
        76 -> skip_prev;
        X -> X
    end.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

-spec terminate(any(),state()) -> no_return().
terminate(Reason, #state{mod=Mod, adpt=H}) ->
    warning_msg("GOING DOWN D: ~p~n", [Reason]),
    Mod:set_ack_mask(H, 0, 0),
    Mod:set_controlled(H, 0),
    Mod:close(H),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec polling_message(state(), laddr()) -> 'ok'.
polling_message(#state{mod=Mod, adpt=H}, Laddr) ->
    Mod:send(H, [], Laddr, Laddr).

-spec set_idle(state(), src(), dest(), integer()) -> 'ok'.
set_idle(#state{mod=Mod, adpt=H}, Src, Dest, Time) ->
    Mod:send(H, [{idle,Time}], Src, Dest).

-spec handle_broadcast(state(), integer(), [binary()]) ->
                              'ok' | {'error',any()}.
handle_broadcast(#state{mod=Mod, adpt=H, laddr=Src}, Op, Params) ->
    Mod:send(H, [{ack_p,1}], Src, ?LADDR_BROADCAST, Op, Params).

-spec handle_send(state(), dest(), integer(), [binary()]) ->
                         'ok' | {'error',any()}.
handle_send(#state{mod=Mod, adpt=H, laddr=Src}, Dest, Op, Params) ->
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
