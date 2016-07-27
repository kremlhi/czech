-module(czech).
-behaviour(gen_server).

-include("czech.hrl").

-export([start_link/0, start_link/1, start_link/2, stop/0, subscribe/1]).
-export([sub/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {subs   = []           :: [pid()],
                mod                   :: module(),
                pid                   :: pid(),
                devtype               :: devtype(),
                laddr  = ?LADDR_UNREG :: src(),
                paddr                 :: paddr(),
                vendor = <<0,21,130>>, %pulse-eight
                active                :: paddr(),
                warn                  :: fun(),
                info                  :: fun(),
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

stop() -> gen_server:stop(?SERVER).
subscribe(Pid) -> gen_server:cast(?SERVER, {subscribe,Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mod | _Opts]) ->
    case Mod:open() of
        {ok,H} ->
            State = init_adpt(#state{mod=Mod, pid=H}),
            {ok,State};
        {error,Reason} ->
            {stop,Reason}
    end.

init_adpt(#state{mod=Mod, pid=H} = State) ->
    ok = check_adapter(State),
    DevType = Mod:get_adapter_type(H),
    ok = set_idle(State, ?LADDR_UNREG, ?LADDR_TV, 3),
    Laddr = set_laddr(State),
    Paddr = Mod:get_paddr(H),
    ok = Mod:set_ack_mask(H, 1, 0),
    State#state{devtype = DevType,
                laddr   = Laddr,
                paddr   = Paddr}.

check_adapter(#state{mod=Mod, pid=H}) ->
    FwVsn = Mod:get_firmware_vsn(H),
    true = FwVsn > 2,
    ok = Mod:set_controlled(H, 1),
    true = Mod:get_builddate(H) > 0,
    ok.

handle_call(_Req, _From, State) ->
    {reply,{error,nih},State}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast({subscribe,Pid}, #state{subs=Subs} = State) ->
    link(Pid),
    {noreply,State#state{subs=lists:usort([Pid | Subs])}}.

-spec handle_info({pid(),cec()},state()) ->
                         {'noreply',state()}.
handle_info({_, {cec,{_,_,_,?CEC_USER_CONTROL_PRESSED,[<<Key>>]}}},
            #state{subs=Subs} = State) ->
    _ = [handle_keypress(Pid, keycode(Key)) || Pid <- Subs],
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_USER_CONTROL_RELEASE,[]}}},
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
            send(State, ?LADDR_TV, {image_view_on}),
            broadcast(State, {active_source,Paddr}),
            send(State, ?LADDR_TV, {menu_status,true}),
            os:cmd("caffeinate -u -t 1");
       true ->
            ok
    end,
    {noreply,State#state{active=To}};
handle_info({_, {cec,{_,_,_,?CEC_GIVE_PHYSICAL_ADDRESS,[]}}},
            #state{paddr   = Paddr,
                   devtype = DevType} = State) ->
    broadcast(State, {report_physical_address,Paddr,DevType}),
    {noreply,State};
handle_info({_, {cec,{_,_,_,?CEC_GIVE_DEVICE_VENDOR_ID,[]}}},
            #state{vendor=VendorId} = State) ->
    broadcast(State, {device_vendor_id,VendorId}),
    {noreply,State};
handle_info({_, {cec,{_,Src,_,_,_}}} = M, #state{devs=Devs} = State) ->
    info(State, "unhandled message: ~w", [M]),
    [D | Devs2] = take_dev(Src, Devs),
    {noreply,State#state{devs=[D | Devs2]}};
handle_info(M, State) ->
    warn(State, "unknown message: ~w", [M]),
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
terminate(Reason, #state{mod=Mod, pid=H} = State) ->
    warn(State, "GOING DOWN D: ~p", [Reason]),
    Mod:set_ack_mask(H, 0, 0),
    Mod:set_controlled(H, 0),
    Mod:close(H),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec polling_message(state(), laddr()) -> 'ok'.
polling_message(#state{mod=Mod, pid=H}, Laddr) ->
    Mod:send(H, [], Laddr, Laddr).

-spec set_idle(state(), src(), dest(), integer()) -> 'ok'.
set_idle(#state{mod=Mod, pid=H}, Src, Dest, Time) ->
    Mod:send(H, [{idle,Time}], Src, Dest).

-spec broadcast(state(), tuple()) -> 'ok' | {'error',any()}.
broadcast(#state{mod=Mod, pid=H, laddr=Src}, Req) ->
    case Req of
        {active_source,Paddr} ->
            Op = ?CEC_ACTIVE_SOURCE,
            Params = [<<X>> || <<X>> <= Paddr];
        {device_vendor_id,VendorId} ->
            Op = ?CEC_DEVICE_VENDOR_ID,
            Params = [<<X>> || <<X>> <= VendorId];
        {report_physical_address,Paddr,DevType} ->
            Op = ?CEC_REPORT_PHYSICAL_ADDRESS,
            Params = [<<X>> || <<X>> <= Paddr] ++ [<<DevType>>]
    end,
    Mod:send(H, [{ack_p,1}],
             Src, ?LADDR_BROADCAST, Op, Params).

-spec send(state(), dest(), tuple()) -> 'ok' | {'error',any()}.
send(#state{mod=Mod, pid=H, laddr=Src}, Dest, Req) ->
    case Req of
        {image_view_on} ->
            Op = ?CEC_IMAGE_VIEW_ON,
            Params = [];
        {menu_status,Activate} ->
            Op = ?CEC_MENU_STATUS,
            Params =
                if Activate -> [<<0>>];
                   true -> [<<1>>]
                end
    end,
    Mod:send(H, [{ack_p,0}],
             Src, Dest, Op, Params).

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

warn(#state{warn=F}, Fmt, L) when is_function(F, 2) ->
    F(Fmt, L);
warn(_, _, _) ->
    ok.

info(#state{warn=F}, Fmt, L) when is_function(F, 2) ->
    F(Fmt, L);
info(_, _, _) ->
    ok.

sub() ->
    F = fun(G)->
                receive {'EXIT',Pid,Reason} ->
                        io:format("~w got: ~w~n",[self(), {Pid,Reason}]),
                        czech:subscribe(self());
                        X->
                        io:format("~w got: ~w~n",[self(), X])
                end,
                G(G)
        end,
    P = spawn(fun()->
                      process_flag(trap_exit, true),
                      czech:subscribe(self()),
                      F(F)
              end),
    Sub = whereis(sub),
    if Sub =:= undefined ->
            ok;
       true ->
            exit(Sub, kill),
            unregister(sub)
    end,
    register(sub, P),
    P.
