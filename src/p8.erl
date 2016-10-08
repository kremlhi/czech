-module(p8).
-behaviour(gen_server).

%% TODO: add mandatory callbacks as -callback

-export([open/0, close/1, controlling_process/2]).
-export([send/4, send/5, send/6]).
-export([get_adapter_type/1, get_builddate/1, get_firmware_vsn/1,
         get_paddr/1, get_hdmi_vsn/1, get_vendor/1]).
-export([set_ack_mask/3, set_controlled/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-import(error_logger, [error_msg/2]). %loooong names

-include("p8.hrl").

%% ping the adapter every 25 seconds, if it doesn't receive any ping
%% for 30 seconds, it'll switch to auto mode
-define(PING_INTERVAL, 25000).

-record(state, {fd             :: port(),
                timeout = 1000 :: integer() | infinity,
                ctlproc = []   :: pid(),
                recvbuf = <<>> :: binary(),
                pretty         :: fun()}).

-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

open()   -> gen_server:start_link(?MODULE, [self()], []).
close(H) -> gen_server:stop(H).

controlling_process(H, Cpid) ->
    gen_server:call(H, {controlling_process,Cpid}).

send(H, Flags, Src, Dest) ->
    gen_server:call(H, {send,Flags,Src,Dest,undefined,<<>>}).

send(H, Flags, Src, Dest, Op) ->
    gen_server:call(H, {send,Flags,Src,Dest,Op,<<>>}).

send(H, Flags, Src, Dest, Op, Params) ->
    gen_server:call(H, {send,Flags,Src,Dest,Op,Params}).

get_adapter_type(H) -> gen_server:call(H, {get_adapter_type}).
get_builddate(H)    -> gen_server:call(H, {get_builddate}).
get_firmware_vsn(H) -> gen_server:call(H, {get_firmware_vsn}).
get_paddr(H)        -> gen_server:call(H, {get_paddr}).
get_hdmi_vsn(H)     -> gen_server:call(H, {get_hdmi_vsn}).
get_vendor(H)       -> gen_server:call(H, {get_vendor}).

set_controlled(H, Mode) -> gen_server:call(H, {set_controlled,Mode}).
set_ack_mask(H, X, Y)   -> gen_server:call(H, {set_ack_mask,X,Y}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

extprog() ->
    ExtPrg = "./p8adpt",
    filename:join(czech_app:priv_dir(), ExtPrg).

init([Cpid]) ->
    process_flag(trap_exit, true),
    Dev = "/dev/cu.usbmodemv2_r1",
    S = init_port(extprog(), Dev),
    {ok,#state{fd = S, ctlproc = Cpid}}.

init_port(ExtPrg, Dev) ->
    S = open_port({spawn_executable,ExtPrg},
                  [{args,[Dev]}, {packet,2}, binary]),
    receive _ -> ok
    after 500 -> ok %flush the pipe
    end,
    erlang:send_after(0, self(), ping),
    S.

handle_call({controlling_process,Ncpid}, {Pid,_},
            #state{ctlproc = Ocpid} = State) ->
    if Pid /= Ocpid ->
            {reply,{error,not_owner},State};
       true ->
            link(Ncpid),
            unlink(Ocpid),
            {reply,ok,State#state{ctlproc = Ncpid}}
    end;
handle_call({send,Flags,Src,Dest,Op,Params}, _, #state{fd = S} = State) ->
    Res = handle_send(S, State, {cec,Flags,Src,Dest,Op,Params}),
    {reply,Res,State};
handle_call({get_adapter_type}, _, #state{fd = S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_ADAPTER_TYPE},
    R = case handle_cmd_get(S, State, Req) of
            <<Type>> ->
                Type;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_builddate}, _, #state{fd = S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_BUILDDATE},
    R = case handle_cmd_get(S, State, Req) of
        <<Date:32>> ->
                Date;
        {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_firmware_vsn}, _, #state{fd = S} = State) ->
    Req = {cmd_get,?P8_CMD_FIRMWARE_VSN},
    R = case handle_cmd_get(S, State, Req) of
            <<Vsn:16>> ->
                Vsn;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_paddr}, _, #state{fd = S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_PADDR},
    R = handle_cmd_get(S, State, Req),
    {reply,R,State};
handle_call({get_hdmi_vsn}, _, #state{fd = S} = State) ->
    Req = {cmd_get,?P8_CMD_GET_HDMI_VSN},
    R = case handle_cmd_get(S, State, Req) of
            <<Vsn>> ->
                Vsn;
            {error,Reason} ->
                {error,Reason}
        end,
    {reply,R,State};
handle_call({get_vendor}, _, State) ->
    R = <<0,21,130>>, %pulse-eight
    {reply,R,State};
handle_call({set_controlled,Mode}, _, #state{fd = S} = State) ->
    Req = {cmd_set,?P8_CMD_SET_CONTROLLED,<<Mode>>},
    R = handle_cmd_set(S, State, Req),
    {reply,R,State};
handle_call({set_ack_mask,X,Y}, _, #state{fd = S} = State) ->
    Req = {cmd_set,?P8_CMD_SET_ACK_MASK,<<X,Y>>},
    R = handle_cmd_set(S, State, Req),
    {reply,R,State}.

-spec handle_cast(_,state()) -> {'noreply',state()}.
handle_cast(_Req, State) ->
    {noreply,State}.

-spec handle_info({port(),{'data',_}},state()) ->
                         {'noreply',state()}.
handle_info(ping, #state{fd = S} = State) ->
    handle_cmd(S, State ,{cmd,?P8_CMD_PING}),
    erlang:send_after(?PING_INTERVAL, self(), ping, []),
    {noreply,State};
handle_info({S, {data,B}}, #state{fd      = S,
                                  ctlproc = Cpid,
                                  recvbuf = Buf} = State) ->
    B2 = <<Buf/binary,B/binary>>,
    try p8_packet:decode(B2) of
        {Ms,Buf2} ->
            pretty(State, "<<< ", Ms),
            _ = [notify(Cpid, indtocec(M)) || M <- Ms],
            {noreply,State#state{recvbuf = Buf2}}
    catch
        error:Error ->
            error_msg("~w could not decode ~w, discarding~n", [Error, B2]),
            {noreply,State#state{recvbuf = <<>>}}
    end;
handle_info({'EXIT',S,Reason}, #state{fd = S} = State) ->
    {stop, {port_terminated,Reason}, State}.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

-spec terminate(_,state()) -> no_return().
terminate({port_terminated,_Reason}, _) ->
    ok;
terminate(_Reason, #state{fd = S}) ->
    port_close(S).

%%====================================================================
%% Internal functions
%%====================================================================

indtocec(#ind_rx{ack = Ack,
                 src = Src, dest = Dest,
                 op = Op, params = Params}) ->
    {cec,[{ack_p,Ack}],Src,Dest,Op,Params};
indtocec(#ind_err{type = Type, line = Line, time = Time}) ->
    {error,{Type,Line,Time}}.

notify(Pid, Msg) ->
    Pid ! {self(),Msg}.

handle_send(S, State, {cec,Flags,Src,Dest,Op,Params}) ->
    Req = #cmd_tx{flags = Flags,
                  src = Src, dest = Dest,
                  op = Op, params = Params},

    case request(S, State, Req) of
        Ms when is_list(Ms) ->
            Exp = p8_packet:ack_ops(Req),
            cec_recv_rest(S, State, Exp, Ms, ok);
        {error,Reason} ->
            {error,Reason}
    end.

cec_recv_rest(_, _, [], [M], ok) ->
    case M of
        #ind_tx_ack{ack = ok} ->
            ok;
        #ind_tx_ack{ack = Nack} ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end;
cec_recv_rest(_, _, [], _, Nack) ->
    {error,Nack};
cec_recv_rest(S, State, [Op | T], [M | Ms], Result) ->
    case M of
        #ind_ack{ack = ok, op = Op} ->
            cec_recv_rest(S, State, T, Ms, Result);
        #ind_ack{ack = Nack, op = Op} ->
            cec_recv_rest(S, State, T, Ms, Nack);
        #ind_tx_ack{ack = Nack} when Nack /= ok ->
            {error,Nack}
    end;
cec_recv_rest(S, State, Exp, [], Result) ->
    Ms = recv_response(S, State),
    cec_recv_rest(S, State, Exp, Ms, Result).

handle_cmd(S, State, {cmd,Op}) ->
    case request(S, State, #cmd{op = Op}) of
        [#ind_ack{ack = ok, op = Op}] ->
            ok;
        [#ind_ack{ack = Nack, op = Op}] ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end.

handle_cmd_get(S, State, {cmd_get,Key}) ->
    case request(S, State, #cmd{op = Key}) of
        [#cmd{op = Key, param = V}] ->
            V;
        {error,Reason} ->
            {error,Reason}
    end.

handle_cmd_set(S, State, {cmd_set,Key,V}) ->
    case request(S, State, #cmd{op = Key, param = V}) of
        [#ind_ack{ack = ok, op = Key}] ->
            ok;
        [#ind_ack{ack = Nack, op = Key}] ->
            {error,Nack};
        {error,Reason} ->
            {error,Reason}
    end.

request(S, State, Req) ->
    send_request(S, State, Req),
    recv_response(S, State).

send_request(S, State, M) ->
    pretty(State, "==> ", [M]),
    do_send(S, State, p8_packet:encode([M])).

do_send(S, _State, B) ->
    port_command(S, B).

recv_response(S, State) ->
    case do_recv(S, State, 0) of
        {ok,B} ->
            try p8_packet:decode(B) of
                {Ms,<<>>} ->
                    pretty(State, "<== ", Ms),
                    Ms
            catch
                error:Reason -> {error,Reason}
            end;
        {error,Reason} ->
            {error,Reason}
    end.

do_recv(S, #state{timeout = T}, _Len) ->
    receive
        {S, {data,<<?BEG,_:2,X:6,_/binary>> = B}}
          when X =/= ?P8_IND_RX_START, %skip #ind_rx{}
               X =/= ?P8_IND_RX_NEXT,
               X =/= ?P8_IND_RX_FAILED ->
            {ok,B}
    after T ->
            {error,timeout}
    end.

pretty(#state{pretty = F}, Prefix, L) when is_function(F, 2) ->
    F(Prefix, L);
pretty(_, _, _) ->
    ok.
