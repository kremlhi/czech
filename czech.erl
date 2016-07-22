-module(czech).
-behaviour(gen_server).

-include("czech.hrl").
-include("p8.hrl").

-export([start_link/1, stop/0, subscribe/1]).
-export([active_source/1, device_vendor_id/1, give_osd_name/1,
         give_device_power_status/1, give_device_vendor_id/1,
         get_menu_language/1, image_view_on/1, menu_status/2,
         report_physical_address/1, request_active_source/0,
         standby/1, set_osd_name/2, set_osd_string/3,
         vendor_command/2]).
-export([engage/0, probe/0, send_cmd_tx/2]).
-compile(export_all).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([check_adapter/1, req_settings/1, open/1, close/1]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, ?MODULE_STRING".log").

-type laddr() :: 0..14.
-type src() :: laddr() | ?LADDR_UNREG.
-type dest() :: laddr() | ?LADDR_BROADCAST.

%% #state{p8 = <0.147.0>,adpttype = 1,laddr = 8,paddr = 4,
%%        lmask = <<16,0>>,
%%        vendor = <<0,21,130>>,
%%        devs = [#dev{laddr = 5,power = true,
%%                     vendor = <<0,160,222>>,
%%                     osdname = <<"Blu-ray">>},
%%                #dev{laddr = 0,power = true,
%%                     vendor = <<0,128,69>>,
%%                     osdname = undefined},

%% paddr = <<32,0>> = <<2:4,0:4,0:4,0:4>>
%%                #dev{laddr = 4,power = true,vendor = undefined,
%%                     osdname = <<"Blu-ray">>}]}

-record(state, {subs   = []           :: [pid()],
                mod                   :: module(),
                adpttype,
                laddr  = ?LADDR_UNREG :: src(),
                lmask                 :: binary(),
                vendor = <<0,21,130>>, % move to p8*?
                devs   = []           :: [dev()]}).

-type state() :: #state{}.

-record(dev, {laddr,
              power,
              vendor,
              osdname,
              lang,
              cecvsn}).

-type dev() :: #dev{}.

%%====================================================================
%% API
%%====================================================================

start_link(Mod) ->
    %% gen_server:start_link({local,?SERVER}, ?MODULE, [Mod],
    %%                       [{timeout,5000},
    %%                        {debug,[trace,{log_to_file,?LOG_FILE}]}]).
    gen_server:start_link({local,?SERVER}, ?MODULE, [Mod],
                          [{timeout,10000}, {debug,[trace]}]).


stop() -> gen_server:stop(?SERVER).
subscribe(Pid) -> gen_server:cast(?SERVER, {subscribe,Pid}).

engage() -> gen_server:cast(?SERVER, {engage}).
probe() -> gen_server:cast(?SERVER, {probe}).

-spec active_source(binary()) -> 'ok'.
active_source(PAddr) -> %BC
    gen_server:cast(?SERVER, {active_source,PAddr}).

-spec device_vendor_id(binary()) -> ok.
device_vendor_id(VendorId) ->
    gen_server:cast(?SERVER, {device_vendor_id,VendorId}).

get_menu_language(Dest) ->
    gen_server:cast(?SERVER, {get_menu_language,Dest}).

give_device_power_status(Dest) ->
    gen_server:cast(?SERVER, {give_device_power_status,Dest}).

give_device_vendor_id(Dest) ->
    gen_server:cast(?SERVER, {give_device_vendor_id,Dest}).

give_osd_name(Dest) ->
    gen_server:cast(?SERVER, {give_osd_name,Dest}).

image_view_on(Dest) ->
    gen_server:cast(?SERVER, {image_view_on,Dest}).

-spec menu_status(dest(), boolean()) -> ok.
menu_status(Dest, Status) ->
    gen_server:cast(?SERVER, {menu_status,Dest,Status}).

report_physical_address(Addr) -> %BC
    gen_server:cast(?SERVER, {report_physical_address,Addr}).

request_active_source() -> %BC
    gen_server:cast(?SERVER, {request_active_source}).

standby(Dest) ->
    gen_server:cast(?SERVER, {standby,Dest}).

set_osd_name(Dest, Name) ->
    gen_server:cast(?SERVER, {set_osd_name,Dest,Name}).

set_osd_string(Dest, Display, String) ->
    gen_server:cast(?SERVER, {set_osd_string,Dest,Display,String}).

-spec vendor_command(dest(), [binary()]) -> ok.
vendor_command(Dest, Commands) ->
    gen_server:cast(?SERVER, {vendor_command,Dest,Commands}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Mod]) ->
%%    ok = error_logger:logfile({open,?MODULE_STRING"-err.log"}),
%%    {ok,_Pid} = Mod:start_link(self()),
    [AdptType, LAddr, LMask] = open(Mod),
%%    int_engage(Mod, LAddr, LMask),
    {ok,#state{mod      = Mod,
               adpttype = AdptType,
               laddr    = LAddr,
               lmask    = LMask}}.

-spec handle_call(_,_,S) -> {'reply','ok',S} when S :: state().
handle_call(_Req, _From, State) ->
    {reply,{error,nih},State}.

int_engage(Mod, LAddr, LMask) ->
    int_send(Mod, LAddr, {image_view_on,?LADDR_TV}),
%% TODO: send them if you have
%%    [<<16>>,<<1>>,<<5>>] = int_send(P, LAddr, {vendor_command,?LADDR_TV,VCmd}),
    int_send(Mod, LAddr, {menu_status,?LADDR_TV,true}),
    int_send(Mod, LAddr, {active_source,LMask}).

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast({engage}, #state{mod=Mod, laddr=LAddr, lmask=LMask} = State) ->
    int_engage(Mod, LAddr, LMask),
    {noreply,State};
handle_cast({probe}, #state{mod=Mod, laddr=LAddr, devs=Devs} = State) ->
    [begin
         set_idle(Mod, LAddr, X, 3),
         int_send(Mod, LAddr, {give_device_power_status,X}),
         int_send(Mod, LAddr, {give_device_vendor_id,X}),
         int_send(Mod, LAddr, {give_osd_name,X})
     end || #dev{laddr=X} <- Devs],
    {noreply,State};
handle_cast({vendor_cmds}, #state{mod=Mod, laddr=LAddr} = State) ->
    VendorId = [<<0,128,69>>],
    int_send(Mod, LAddr, {vendor_command,?LADDR_TV,
                          id_to_commands(list_to_binary(VendorId))}),
    {noreply,State};
handle_cast(Req, #state{mod=Mod, laddr=LAddr} = State) ->
    int_send(Mod, LAddr, Req),
    {noreply,State}.

-spec handle_info({port(),{'data',_}},state()) ->
                         {'noreply',state()}.
handle_info({unsol,_,#ind_rx{op=?CEC_GIVE_DEVICE_VENDOR_ID}},
            #state{mod=Mod, laddr=LAddr, vendor=VendorId} = State) ->
    int_send(Mod, LAddr, {device_vendor_id,VendorId}),
    {noreply,State};
handle_info({unsol,_,#ind_rx{op=?CEC_VENDOR_COMMAND_WITH_ID, src=Src,
                            params=Params}},
            #state{mod=Mod, laddr=LAddr} = State) ->
    VendorId = list_to_binary(lists:sublist(Params, 3)),
    %% int_send(Mod, LAddr, {vendor_command,Src,
    %%                     id_to_commands(VendorId)}),
    {noreply,State};
handle_info({unsol,_,#ind_rx{op=?CEC_USER_CONTROL_PRESSED, params=[<<Key>>]}},
            #state{subs=Subs} = State) ->
    _ = [handle_keypress(Pid, keycode(Key)) || Pid <- Subs],
    {noreply,State};
handle_info({unsol,_,#ind_rx{op=?CEC_USER_CONTROL_RELEASE}},
            #state{subs=Subs} = State) ->
%%    io:format("handle_keyrel(~w)~n", [Pid]),
    _ = [handle_keyrel(Pid) || Pid <- Subs],
    {noreply,State};
handle_info({unsol,_,#ind_rx{op=Op, params=[<<M:1,Volume:7>>]}},
            #state{subs=Subs} = State)
  when Op =:= ?CEC_REPORT_AUDIO_STATUS ->
    Mute = M =:= 1,
%%    io:format("handle_volume(~w, ~w, ~w)~n", [Pid, Mute, Volume]),
    _ = [handle_volume(Pid, Mute, Volume) || Pid <- Subs],
    {noreply,State};
handle_info({unsol,_,#ind_rx{src=Src} = M}, #state{devs=Devs} = State) ->
    case lists:keytake(Src, #dev.laddr, Devs) of
        {value,D,Devs2} -> ok;
        false -> D     = #dev{laddr=Src},
                 Devs2 = Devs
    end,
    D2 = update_dev(M, D),
    {noreply,State#state{devs = [D2 | Devs2]}}.

handle_keypress(Pid, Key) ->
%%    io:format("handle_keypress(~w, ~w)~n", [Pid, Key]),
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
        31 -> d9;
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

-spec terminate(_,state()) -> no_return().
terminate(Reason, #state{mod=Mod}) ->
    io:format("GOING DOWN D: ~p~n", [Reason]),
    close(Mod).

%%====================================================================
%% Internal functions
%%====================================================================

-spec polling_message(module(), laddr()) -> 'ok'.
polling_message(Mod, LAddr) ->
    send_cmd_tx(Mod, #cmd_tx{src=LAddr, dest=LAddr}).

-spec set_idle(module(), src(), dest(), integer()) -> 'ok'.
set_idle(Mod, Src, Dest, Time) ->
    send_cmd_tx(Mod, #cmd_tx{flags=[{idle,Time}], src=Src, dest=Dest}).

-spec set_timeout(module(), src(), dest(), integer()) -> 'ok'.
set_timeout(Mod, Src, Dest, Time) ->
    send_cmd_tx(Mod, #cmd_tx{flags=[{timeout,Time}], src=Src, dest=Dest}).

-spec int_send(module(), src(), cmd_tx()) -> 'ok'.
int_send(Mod, Src, Req) ->
    send_cmd_tx(Mod, int(Req, #cmd_tx{src=Src})).

-spec set_laddr(module()) -> laddr().
set_laddr(Mod) ->
    set_laddr(Mod, [?LADDR_PLAYBDEV1, ?LADDR_PLAYBDEV2, ?LADDR_PLAYBDEV3]).

set_laddr(Mod, [LAddr | T]) ->
    case polling_message(Mod, LAddr) of
        ok -> set_laddr(Mod, T);
        {error,tx_nack} ->
            {error,tx_nack} = polling_message(Mod, LAddr),
            LAddr
    end.

check_adapter(Mod) ->
    ok = Mod:ping(), % ping until adapter is available
    FwVsn = Mod:get_firmware_vsn(),
    true = FwVsn > 2,
    ok = Mod:set_controlled(1),
    true = Mod:get_builddate() > 0,
    ok.

%% // don't read the following settings:
%% // - CEC version (1.3a)
%% // - auto enabled (always enabled)
%% // - default logical address (autodetected)
%% // - logical address mask (autodetected)
req_settings(Mod) ->
    %% true = Mod:get_auto_enabled(P),
    %% HdmiVsn = Mod:get_hdmi_vsn(P),
    %% DefLAddr = Mod:get_def_laddr(P),
    %% OsdName = Mod:get_osd_name(P),
    LMask = Mod:get_paddr(),
    [LMask].

open2() ->
    Mod = p8,
    check_adapter(Mod),
    Mod:subscribe(self()),
    AdptType = Mod:get_adapter_type(),
    ok = set_idle(Mod, ?LADDR_UNREG, ?LADDR_TV, 3),
%% polling_message(p8,8),
%% polling_message(p8,8),
    set_laddr(Mod),
ok.
%%    LAddr = set_laddr(Mod),
%%    [AdptType, LAddr].



%% bool CUSBCECAdapterCommunication::Open(
%%   uint32_t iTimeoutMs /* = CEC_DEFAULT_CONNECT_TIMEOUT */,
%%   bool bSkipChecks /* = false */,
%%   bool bStartListening /* = true */)
open(Mod) ->
    check_adapter(Mod),
    Mod:subscribe(whereis(?SERVER)),
    AdptType = Mod:get_adapter_type(),
    ok = set_idle(Mod, ?LADDR_UNREG, ?LADDR_TV, 3),

    LAddr = set_laddr(Mod),
    [LMask | _] = req_settings(Mod),

    %% /*!
    %%  * @brief Get the ackmask for all devices of the given type.
    %%  * @param type The type to get the ackmask for.
    %%  * @return The ackmask for this type, or 0 of no types match.
    %%  */
    %% static uint16_t GetMaskForType(const cec_device_type type)
    ok = Mod:set_ack_mask(1, 0),

    int_send(Mod, LAddr, {report_physical_address,
                          <<LMask/binary,AdptType>>}), % <<1:4,0:4,0:4,0:4,4>>

    [AdptType, LAddr, LMask].

close(Mod) ->
    ok = Mod:set_ack_mask(0, 0),
    ok = Mod:set_controlled(0),
    Mod:recv(500), %empty message queue
    ok.


update_dev(#ind_rx{op=Op, params=Params} = M, Dev) ->
    case Op of
        ?CEC_REPORT_POWER_STATUS ->
            Dev#dev{power=Params =:= [<<0>>]};
        ?CEC_DEVICE_VENDOR_ID ->
            %% TODO: throw some vendor commands
            %% VCmd = id_to_commands(VendorId),
            %% vendor_command(P, LAddr, ?LADDR_TV, VCmd),
            Dev#dev{vendor=list_to_binary(Params)};
        ?CEC_SET_OSD_NAME ->
            Dev#dev{osdname=list_to_binary(Params)};
        ?CEC_CEC_VERSION ->
            Vsn = case Params of
                      [<<0>>] -> undefined;
                      [<<1>>] -> {1,2};
                      [<<2>>] -> {1,2,a};
                      [<<3>>] -> {1,3};
                      [<<4>>] -> {1,3,a};
                      [<<5>>] -> {1,4};
                      _ -> dont_be_lazy
                  end,
            Dev#dev{cecvsn=Vsn};
        _ -> io:format("what to do? ~w ~w~n", [M,Dev]),
             Dev
    end.

% when turning off
%% {ind_rx,1,0,15,160,
%% [<<0>>, <<128>>, <<"E">>,<<" ">>, <<1>>, <<16>>]}}
%% aka <<0,128,69,32,1,16>>

id_to_commands(Id) ->
    case Id of
        <<0,128,69>> -> %Panasonic
            [<<16>>, <<2>>, <<253,252>>,
             <<253,252>>, <<0>>, <<5>>, <<5>>, <<69>>,
             <<85>>, <<92>>, <<88>>, <<50>>];
        _ -> [] %Dunno
    end.


int({request_active_source}, R) ->
    R#cmd_tx{dest=?LADDR_BROADCAST,
             op=?CEC_REQUEST_ACTIVE_SOURCE};

int({active_source,PAddr}, R) ->
    R#cmd_tx{dest=?LADDR_BROADCAST,
             op=?CEC_ACTIVE_SOURCE,
             params=[<<X>> || <<X>> <= PAddr]};
int({device_vendor_id,VendorId}, R) ->
    R#cmd_tx{dest=?LADDR_BROADCAST,
             op=?CEC_DEVICE_VENDOR_ID,
             params=[<<X>> || <<X>> <= VendorId]};
int({report_physical_address,Addr}, R) ->
    R#cmd_tx{dest=?LADDR_BROADCAST,
             op=?CEC_REPORT_PHYSICAL_ADDRESS,
             params=[<<X>> || <<X>> <= Addr]};
int({Cmd,Dest}, R) ->
    Op = case Cmd of
             get_menu_language        -> ?CEC_GET_MENU_LANGUAGE;
             give_audio_status        -> ?CEC_GIVE_AUDIO_STATUS;
             give_device_power_status -> ?CEC_GIVE_DEVICE_POWER_STATUS;
             give_device_vendor_id    -> ?CEC_GIVE_DEVICE_VENDOR_ID;
             give_osd_name            -> ?CEC_GIVE_OSD_NAME;
             give_physical_address    -> ?CEC_GIVE_PHYSICAL_ADDRESS;
             image_view_on            -> ?CEC_IMAGE_VIEW_ON;
             standby                  -> ?CEC_STANDBY
         end,
    R#cmd_tx{op=Op, dest=Dest};

int({Cmd,Dest,Params}, R) ->
    case Cmd of
        give_deck_status ->
            R#cmd_tx{op=?CEC_GIVE_DECK_STATUS, params=Params};
        device_vendor_id ->
            R#cmd_tx{op=?CEC_DEVICE_VENDOR_ID,
                     params=[<<X>> || <<X>> <= Params]};
        menu_status ->
            R#cmd_tx{op=?CEC_MENU_STATUS,
                     params=if Params -> [<<0>>];
                               true -> [<<1>>]
                            end};
        set_osd_name ->
            R#cmd_tx{op=?CEC_SET_OSD_NAME,
                     params=[<<X>> || <<X>> <= Params]};
        vendor_command ->
            R#cmd_tx{op=?CEC_VENDOR_COMMAND, params=Params}
    end#cmd_tx{dest=Dest};
int({Cmd,Dest,Param1,Params}, R) ->
    case Cmd of
        set_osd_string ->
            Disp = case Param1 of
                       default       -> <<16#00>>;
                       until_cleared -> <<16#40>>;
                       clear_prev    -> <<16#80>>
                   end,
            B = <<Disp/binary,Params/binary>>,
            R#cmd_tx{op=?CEC_SET_OSD_STRING,
                    params=[<<X>> || <<X>> <= B]}
    end#cmd_tx{dest=Dest}.



send_cmd_tx(Mod, #cmd_tx{flags=Flags, src=Src, dest=Dest,
                         op=Op, params=Params}) ->
    Bc = if Dest =:= ?LADDR_BROADCAST -> 1;
            true -> 0
         end,
    Req = #cmd_tx{flags=Flags ++ [{ack_p,Bc}],
                  src=Src, dest=Dest,
                  op=Op, params=Params},
    p8:send(Req),
    wait_for_resp(Mod, Req).

cmd_txs(N) when N > 0 ->
    [?P8_CMD_TX || _ <- lists:seq(1, N - 1)] ++ [?P8_CMD_TX_EOM].

flag_ops(N, Flags) ->
    [p8_packet:cmd_tx_atoi_flag(X) || {X,_} <- Flags] ++ cmd_txs(N).

wait_for_resp(Mod, #cmd_tx{flags=Flags, op=Op, params=Params}) ->
    N = 1 % src/dest
        + if Op =:= undefined -> 0;
             true -> 1 + length(Params)
          end,
    Ops = flag_ops(N, Flags),
    Rx1 = [Mod:recv(2000) || _ <- Ops],
    Rx2 = [#ind_ack{ack=ok, op=X} || X <- Ops],
%%    io:format("Rx1 ~s Rx2~n", [if Rx1=:=Rx2->"=:=";true->"=/="end]),
    #ind_tx_ack{ack=Ack} = Mod:recv(2000),
    Ack.
