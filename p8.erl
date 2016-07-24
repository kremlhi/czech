-module(p8).
-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0, subscribe/1]).

-export([ping/0, get_firmware_vsn/0, get_builddate/0,
         get_adapter_type/0, set_controlled/1, set_ack_mask/2,
         set_active_source/1, get_auto_enabled/0, get_hdmi_vsn/0,
         get_def_laddr/0, get_dev_type/0, get_laddr_mask/0, get_osd_name/0,
         get_paddr/0]).

-export([cec_send/3, cec_send/4, cec_send/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, ?MODULE_STRING".log").

%% ping the adapter every 25 seconds, if it doesn't receive any ping
%% for 30 seconds, it'll switch to auto mode
-define(PING_INTERVAL, 25000).

-include("p8.hrl").

-record(p8, {fd                 :: port(),
             timeout = infinity :: infinity | integer(),
             subs    = []       :: [pid()]}).

-type state() :: #p8{}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link([]).

start_link(Opts) ->
    %% gen_server:start_link({local,?SERVER}, ?MODULE, [],
                          %% [{debug,[trace,{log_to_file,?LOG_FILE}]}]).
    gen_server:start_link({local,?SERVER}, ?MODULE, Opts, []).

stop() ->
    gen_server:stop(?SERVER).

subscribe(Pid) -> gen_server:cast(?SERVER, {subscribe,Pid}).

ping() -> gen_server:call(?SERVER, {ping}).

get_firmware_vsn() -> gen_server:call(?SERVER, {get_firmware_vsn}).
get_builddate()    -> gen_server:call(?SERVER, {get_builddate}).
get_adapter_type() -> gen_server:call(?SERVER, {get_adapter_type}).
get_auto_enabled() -> gen_server:call(?SERVER, {get_auto_enabled}).
get_hdmi_vsn()     -> gen_server:call(?SERVER, {get_hdmi_vsn}).
get_def_laddr()    -> gen_server:call(?SERVER, {get_def_laddr}).
get_dev_type()     -> gen_server:call(?SERVER, {get_dev_type}).
get_laddr_mask()   -> gen_server:call(?SERVER, {get_laddr_mask}).
get_osd_name()     -> gen_server:call(?SERVER, {get_osd_name}).
get_paddr()        -> gen_server:call(?SERVER, {get_paddr}).

set_controlled(Mode) -> gen_server:call(?SERVER, {set_controlled,Mode}).
set_ack_mask(X, Y)   -> gen_server:call(?SERVER, {set_ack_mask,X,Y}).
set_active_source(X) -> gen_server:call(?SERVER, {set_active_source,X}).

cec_send(Flags, Src, Dest) ->
    Req = {cec_send,Flags,Src,Dest},
    gen_server:call(?SERVER, Req).

cec_send(Flags, Src, Dest, Op) ->
    Req = {cec_send,Flags,Src,Dest,Op},
    gen_server:call(?SERVER, Req).

cec_send(Flags, Src, Dest, Op, Params) ->
    Req = {cec_send,Flags,Src,Dest,Op,Params},
    gen_server:call(?SERVER, Req).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    process_flag(trap_exit, true),
    case proplists:get_value(debug, Opts) of
        true ->
            S = undefined;
        _ ->
            ExtPrg = "./echo",
            Dev = "/dev/cu.usbmodemv2_r1",
            S = init_port(ExtPrg, Dev)
    end,
    {ok,#p8{fd=S}}.

init_port(ExtPrg, Dev) ->
    S = open_port({spawn_executable,ExtPrg},
                  [{args,[Dev]}, {packet,2}, binary]),
    receive _ -> ok
    after 500 -> ok  %flush the pipe
    end,
    erlang:send_after(0, self(), ping),
    S.

%% TODO: resend?
handle_call({ping}, _, #p8{fd=S} = State) ->
    R = handle_cmd(S, State, {cmd,?P8_CMD_PING}),
    {reply,R,State};
handle_call({get_firmware_vsn}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_FIRMWARE_VSN},
    R = case handle_cmd_get(S, State, Req) of
            <<Vsn:16>> ->
                Vsn;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_builddate}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_BUILDDATE},
    R = case handle_cmd_get(S, State, Req) of
        <<Date:32>> ->
                Date;
        {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_adapter_type}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_ADAPTER_TYPE},
    R = case handle_cmd_get(S, State, Req) of
            <<Type>> ->
                Type;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_auto_enabled}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_AUTO_ENABLED},
    R = case handle_cmd_get(S, State, Req) of
            <<0>> ->
                false;
            <<1>> ->
                true;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_hdmi_vsn}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_HDMI_VSN},
    R = case handle_cmd_get(S, State, Req) of
            <<Vsn>> ->
                Vsn;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_def_laddr}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_DEF_LADDR},
    R = case handle_cmd_get(S, State, Req) of
            <<LAddr>> ->
                LAddr;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_dev_type}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_DEV_TYPE},
    R = case handle_cmd_get(S, State, Req) of
            <<Type>> ->
                Type;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_laddr_mask}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_LADDR_MASK},
    R = handle_cmd_get(S, State, Req),
    {reply,R,State};
handle_call({get_osd_name}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_OSD_NAME},
    R = handle_cmd_get(S, State, Req),
    {reply,R,State};
handle_call({get_paddr}, _, #p8{fd=S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_PADDR},
    R = handle_cmd_get(S, State, Req),
    {reply,R,State};
handle_call({set_controlled,Mode}, _, #p8{fd=S} = State) ->
    Req = {cmd_set,?P8_CMD_SET_CONTROLLED,<<Mode>>},
    R = handle_cmd_set(S, State, Req),
    {reply,R,State};
handle_call({set_ack_mask,X,Y}, _, #p8{fd=S} = State) ->
    Req = {cmd_set,?P8_CMD_SET_ACK_MASK,<<X,Y>>},
    R = handle_cmd_set(S, State, Req),
    {reply,R,State};
handle_call({set_active_source,X}, _, #p8{fd=S} = State) ->
    Req = {cmd_set,?P8_CMD_SET_ACTIVE_SOURCE,<<X>>},
    R = handle_cmd_set(S, State, Req),
    {reply,R,State};
handle_call({cec_send,Flags,Src,Dest}, _, #p8{fd=S} = State) ->
    M = {cec_send,Flags,Src,Dest,undefined,[]},
    Res = handle_cec_send(S, State, M),
    {reply,Res,State};
handle_call({cec_send,Flags,Src,Dest,Op}, _, #p8{fd=S} = State) ->
    M = {cec_send,Flags,Src,Dest,Op,[]},
    Res = handle_cec_send(S, State, M),
    {reply,Res,State};
handle_call({cec_send,_,_,_,_,_} = M, _, #p8{fd=S} = State) ->
    Res = handle_cec_send(S, State, M),
    {reply,Res,State}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast({subscribe,Pid}, #p8{subs = Subs} = State) ->
    {noreply,State#p8{subs=lists:usort([Pid | Subs])}};
handle_cast(_Req, State) ->
    {noreply,State}.

-spec handle_info({port(),{'data',_}},state()) ->
                         {'noreply',state()}.
handle_info(ping, #p8{fd=S} = State) ->
    handle_cmd(S, State ,{cmd,?P8_CMD_PING}),
    erlang:send_after(?PING_INTERVAL, self(), ping, []),
    {noreply,State};
handle_info({S, {data,B}}, #p8{fd   = S,
                               subs = Subs} = State) ->
    io:format("<<< ~w~n", [B]),
    {[M],<<>>} = p8_packet:decode(B),
    p8_pretty:print("<<< ", [M]),
    #ind_rx{src=Src, dest=Dest, op=Op, params=Params} = M,
    M2 = {cec,[],Src,Dest,Op,Params},
    _ = [notify(Pid, M2) || Pid <- Subs], % TODO: change to gen_event
    {noreply,State};
handle_info({'EXIT',S,Reason}, #p8{fd=S} = State) ->
    {stop, {port_terminated,Reason}, State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

-spec terminate(_,state()) -> no_return().
terminate({port_terminated,_Reason}, _) ->
    ok;
terminate(_Reason, #p8{fd=S}) ->
    port_close(S).

%%====================================================================
%% Internal functions
%%====================================================================

notify(Pid, Msg) ->
    Pid ! {self(),Msg}.

handle_cec_send(S, State, {cec_send,Flags,Src,Dest,Op,Params}) ->
    Req = #cmd_tx{flags=Flags,
                  src=Src, dest=Dest,
                  op=Op, params=Params},
    [Op2 | Exp] = ack_ops(Req),
    case request(S, State, Req) of
        #ind_ack{ack=Ack, op=Op2} ->
            cec_recv_rest(S, State, Exp, Ack);
        {error,Reason} ->
            {error,Reason}
    end.

ack_ops(#cmd_tx{flags=Flags, op=Op, params=Params}) ->
    F = [p8_packet:cmd_tx_atoi_flag(X) || {X,_} <- Flags],
    O = [?P8_CMD_TX || X <- [Op], X =/= undefined],
    P = [?P8_CMD_TX || _ <- Params],
    S = [?P8_CMD_TX_EOM], %src/dest
    F ++ O ++ P ++ S.

cec_recv_rest(S, State, [], ok) ->
    case recv_response(S, State) of
        #ind_tx_ack{ack=ok} ->
            ok;
        #ind_tx_ack{ack=Nack} ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end;
cec_recv_rest(_, _, [], Nack) ->
    {error,Nack};
cec_recv_rest(S, State, [Op | T], Result) ->
    case recv_response(S, State) of
        #ind_ack{ack=ok, op=Op} ->
            cec_recv_rest(S, State, T, Result);
        #ind_ack{ack=Ack, op=Op} ->
            cec_recv_rest(S, State, T, Ack)
    end.

handle_cmd(S, State, {cmd,Op}) ->
    case request(S, State, #cmd{op=Op}) of
        #ind_ack{ack=ok, op=Op} ->
            ok;
        #ind_ack{ack=Nack, op=Op} ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end.

handle_cmd_get(S, State, {cmd_get,Key}) ->
    case request(S, State, #cmd{op=Key}) of
        #cmd{op=Key, param=V} ->
            V;
        {error,Reason} ->
            {error,Reason}
    end.

handle_cmd_set(S, State, {cmd_set,Key,V}) ->
    case request(S, State, #cmd{op=Key, param=V}) of
        #ind_ack{ack=ok, op=Key} ->
            ok;
        #ind_ack{ack=Nack, op=Key} ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end.

request(S, State, Req) ->
    send_request(S, State, Req),
    recv_response(S, State).

send_request(S, State, M) ->
    p8_pretty:print("==> ", [M]),
    do_send(S, State, p8_packet:encode([M])).

do_send(S, _, B) ->
    io:format("==> ~w~n", [B]),
    port_command(S, B).

recv_response(S, State) ->
    case do_recv(S, State, 0) of
        {ok,B} ->
            {[M],<<>>} = p8_packet:decode(B),
            p8_pretty:print("<== ", [M]),
            M;
        {error,Reason} ->
            {error,Reason}
    end.

do_recv(S, #p8{timeout=T}, _Len) ->
    receive
        {S, {data,B}} ->
            io:format("<== ~w~n", [B]),
            {ok,B};
        {'EXIT',S,Reason} -> % FIXME: should be handled by handle_info/2?
            {error,Reason}
    after T ->
            {error,timeout}
    end.
