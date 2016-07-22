-module(p8).
-behaviour(gen_server).

-export([start_link/0, stop/0, subscribe/1, send/1, recv/0, recv/1]).
-export([ping/0, get_firmware_vsn/0, get_builddate/0,
         get_adapter_type/0, set_controlled/1, set_ack_mask/2,
         set_active_source/1, get_auto_enabled/0, get_hdmi_vsn/0,
         get_def_laddr/0, get_dev_type/0, get_laddr_mask/0, get_osd_name/0,
         get_paddr/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([send_cmd/1, send_cmd/2, send_cmd_get/1, send_cmd_set/2]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, ?MODULE_STRING".log").
-define(PING_INTERVAL, 25000).

-include("p8.hrl").

-record(state, {debug    = true  :: boolean(),
                port             :: port(),
                subs     = []    :: [pid()],
                recvbuf  = <<>>  :: binary(),
                recept   = []    :: [{pid(),reference(),integer()}],
                mbox     = [],
                ping             :: ping | pong}).

-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    %% gen_server:start_link({local,?SERVER}, ?MODULE, [],
                          %% [{debug,[trace,{log_to_file,?LOG_FILE}]}]).
    gen_server:start_link({local,?SERVER}, ?MODULE, [], [{debug,[trace]}]).

stop() ->
    gen_server:stop(?SERVER).

subscribe(Pid) ->
    gen_server:cast(?SERVER, {subscribe,Pid}).

send(Msg) ->
    gen_server:call(?SERVER, {send,Msg}).

recv() -> recv(infinity).

recv(Time) ->
    case async_recv(Time) of
        {ok,Tag} ->
            receive
                {p8_async,_Pid,Tag,Status} ->
                    Status;
                {'EXIT',_Pid,_Reason} ->
                    {error,closed}
            end;
        {error,Reason} ->
            {error,Reason}
    end.

ping() -> send_cmd(?P8_CMD_PING).

get_firmware_vsn() ->
    <<Vsn:16>> = send_cmd_get(?P8_CMD_FIRMWARE_VSN),
    Vsn.

get_builddate() ->
    <<BDate:32>> = send_cmd_get(?P8_CMD_GET_BUILDDATE),
    BDate.

get_adapter_type() ->
    <<Type>> = send_cmd_get(?P8_CMD_GET_ADAPTER_TYPE),
    Type.

set_controlled(Mode) ->
    send_cmd_set(?P8_CMD_SET_CONTROLLED, <<Mode>>).

set_ack_mask(V1, V2) ->
    send_cmd_set(?P8_CMD_SET_ACK_MASK, <<V1,V2>>).

set_active_source(X) ->
    send_cmd_set(?P8_CMD_SET_ACTIVE_SOURCE, <<X>>).

get_auto_enabled() ->
    case send_cmd_get(?P8_CMD_GET_AUTO_ENABLED) of
        <<0>> -> false;
        <<1>> -> true
    end.

get_hdmi_vsn() ->
    <<Vsn>> = send_cmd_get(?P8_CMD_GET_HDMI_VSN),
    Vsn.

get_def_laddr() ->
    <<LAddr>> = send_cmd_get(?P8_CMD_GET_DEF_LADDR),
    LAddr.

get_dev_type() ->
    <<Type>> = send_cmd_get(?P8_CMD_GET_DEV_TYPE),
    Type.

get_laddr_mask() ->
    <<V1,V2>> = send_cmd_get(?P8_CMD_GET_LADDR_MASK),
    <<V1,V2>>. %% I.e. <<9,16>>

get_osd_name() ->
    <<Name/binary>> = send_cmd_get(?P8_CMD_GET_OSD_NAME),
    Name.

get_paddr() ->
    <<A,B>> = send_cmd_get(?P8_CMD_GET_PADDR),
    <<A,B>>. % I.e. <<16,0>>

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Args) ->
    process_flag(trap_exit, true),
%%    ok = error_logger:logfile({open,"p8-err.log"}),
%%    error_logger:tty(false),
    ExtPrg = "./p8adpt",
    Dev = "/dev/cu.usbmodemv2_r1",
    Port = open_port({spawn_executable,ExtPrg},
                     [{args,[Dev]}, {packet,2}, binary]),
    receive X -> X
    after 1000 -> ok  %flush the pipe
    end,
    erlang:send_after(?PING_INTERVAL, self(), ping),
    {ok,#state{port=Port}}.

-spec handle_call(_,From,state()) -> {reply,term(),state()}
                                         when From :: {pid(),reference()}.
%% TODO: resend?
%% TODO: delay ping?
handle_call({send,Msg}, _, #state{debug=Debug, port=Port} = State) ->
    p8_pretty:print(Debug, "==> ", [Msg]),
    try
        port_command(Port, p8_packet:encode([Msg])),
        {reply,ok,State}
    catch
        error:Reason -> {reply,{error,Reason},State}
    end;
handle_call({async_recv,_}, {Pid,Tag}, #state{mbox=[H | Mbox]} = State) ->
    send_async(Pid, Tag, H),
    {reply,{ok,Tag},State#state{mbox=Mbox}};
handle_call({async_recv,Time}, {Pid,Tag}, #state{recept=Rs} = State) ->
    TRef = if Time =:= infinity -> undefined;
              true -> erlang:start_timer(Time, self(), async_recv)
           end,
    R = {Pid,Tag,Time,TRef},
    {reply,{ok,Tag},State#state{recept=Rs ++ [R]}}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast({subscribe,Pid}, #state{subs = Subs} = State) ->
    {noreply,State#state{subs=lists:usort([Pid | Subs])}};
handle_cast(_Req, State) ->
    {noreply,State}.

-spec handle_info({port(),{'data',_}},state()) ->
                         {'noreply',state()}.
%% ping the adapter every 25 seconds, if it doesn't receive any ping
%% for 30 seconds, it'll switch to auto mode
handle_info(ping, #state{port=Port} = State) ->
    port_command(Port, p8_packet:encode([#cmd{op=?P8_CMD_PING}])),
    erlang:send_after(?PING_INTERVAL, self(), ping, []),
    {noreply,State#state{ping=ping}};
handle_info({timeout,TRef,async_recv}, #state{recept=Rs} = State) ->
    case lists:keytake(TRef, 4, Rs) of
        {value,{Pid,Tag,_,_},Rs2} ->
            send_async(Pid, Tag, {error,timeout});
        false -> % response already sent while timeout message was in inqueue
            Rs2 = Rs
    end,
    {noreply,State#state{recept=Rs2}};
handle_info({Port,{data,Data}},
            #state{debug   = Debug,
                   port    = Port,
                   recvbuf = Rbuf,
                   recept  = Rs,
                   subs    = Subs,
                   mbox    = Mbox,
                   ping    = Ping} = State) ->
    {L,Rbuf2} = p8_packet:decode(<<Rbuf/binary,Data/binary>>),
    p8_pretty:print(Debug, "<== ", Mbox ++ L),
    {Mbox2,Rs2,Ping2} = send_while(Mbox ++ L, Rs, Subs, Ping),
    {noreply,State#state{recvbuf=Rbuf2, recept=Rs2, mbox=Mbox2, ping=Ping2}};
handle_info({'EXIT',Port,Reason}, #state{port=Port} = State) ->
    {stop, {port_terminated,Reason}, State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

-spec terminate(_,state()) -> no_return().
terminate({port_terminated,_Reason}, _) ->
    ok;
terminate(_Reason, #state{port=Port}) ->
    port_close(Port).

%%====================================================================
%% Internal functions
%%====================================================================

send_while([#ind_ack{op=?P8_CMD_PING} | Ms], Rs, Subs, ping) ->
    send_while(Ms, Rs, Subs, pong);
send_while([#ind_rx{} = Msg | Ms], Rs, Subs, Pong) ->
    _ = [send_unsol(Pid, Msg) || Pid <- Subs],
    send_while(Ms, Rs, Subs, Pong);
send_while([Msg | Ms], [{Pid,Tag,_,TRef} | Rs], Subs, Pong) ->
    send_async(Pid, Tag, Msg),
    if TRef =:= undefined -> ok;
       true -> erlang:cancel_timer(TRef, [{async,true}, {info,false}])
    end,
    send_while(Ms, Rs, Subs, Pong);
send_while(Ms, Rs, _, Pong) ->
    {Ms,Rs,Pong}.

send_unsol(Pid, Msg) ->
    Pid ! {unsol,self(),Msg}.

send_async(Pid, Tag, Msg) ->
    Pid ! {p8_async,self(),Tag,Msg}.

-spec async_recv(integer()) -> {ok,reference()} | {error,term()}.
async_recv(Time) -> gen_server:call(?SERVER, {async_recv,Time}).

send_cmd(Op) -> send_cmd(Op, 500).

send_cmd(Op, Time) ->
    send(#cmd{op = Op}),
    #ind_ack{ack = Resp, op = Op} = recv(Time),
    Resp.

send_cmd_get(Op) ->
    send(#cmd{op = Op}),
    #cmd{op = Op, param = Param} = recv(500),
    Param.

send_cmd_set(Op, Param) ->
    send(#cmd{op = Op, param = Param}),
    #ind_ack{ack = Resp, op = Op} = recv(500),
    Resp.
