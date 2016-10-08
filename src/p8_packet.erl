-module(p8_packet).

-compile(export_all).

%% TODO: the payload (params) of #cmd_tx{} and #ind_rx{} seem to be
%% able to be bigger than one byte, optimization would be to pack more
%% bits

-include("p8.hrl").
-include("czech.hrl").

-export([encode/1, decode/1, cmd_tx_flag/1, ack_ops/1]).

-spec encode([packet()]) -> binary().
encode(L) ->
    << <<(encode1(X))/binary>> || X <- L >>.

-spec decode(binary()) -> {[packet()],binary()}.
decode(B) ->
    decode(B, [], <<>>).

-spec ack_ops(cmd_tx()) -> [byte()].
ack_ops(#cmd_tx{flags = Flags, op = Op, params = Params}) ->
    F = [cmd_tx_flag(X) || {X,_} <- Flags],
    O = [?P8_CMD_TX || X <- [Op], X =/= undefined],
    P = [?P8_CMD_TX || <<_>> <= Params],
    S = [?P8_CMD_TX_EOM], %src/dest is mandatory
    F ++ O ++ P ++ S.

-spec cmd_tx_flag(atom()) -> byte().
cmd_tx_flag(X) ->
    case X of
        idle    -> ?P8_CMD_TX_SET_IDLE;
        ack_p   -> ?P8_CMD_TX_SET_ACK_P;
        timeout -> ?P8_CMD_TX_SET_TIMEOUT
    end.


-spec encode1(packet()) -> {<<_:32,_:_*8>>,binary()}.
encode1(R) ->
    case R of
        #cmd{op = Op, param = Param} ->
            cmd_encode(Op, Param);
        #cmd_tx{flags = Flags,
                src = Src, dest = Dest,
                op = Op, params = Params} ->
            cmd_tx_encode(Flags, Src, Dest, Op, Params);
        #ind_ack{ack = Ack, op = Op} ->
            ind_ack_encode(Ack, Op);
        #ind_err{type = Type, line = Line, time = Time} ->
            ind_err_encode(Type, Line, Time);
        #ind_tx_ack{ack = Ack} ->
            ind_tx_ack_encode(Ack);
        #ind_rx{ack = Ack, src = Src, dest = Dest,
                op = Op, params = Params} ->
            ind_rx_encode(Ack, Src, Dest, Op, Params)
    end.

ind_ack_encode(Ack, Op) ->
    X = case Ack of
            ok   -> ?P8_IND_ACK;
            nack -> ?P8_IND_NACK
        end,
    <<?BEG,X,Op,?END>>.

ind_err_encode(Type, Line, Time) ->
    X = case Type of
            timeout -> ?P8_IND_ERR_TIMEOUT;
            high    -> ?P8_IND_ERR_HIGH;
            low     -> ?P8_IND_ERR_LOW
        end,
    B = case {Line,Time} of
            {undefined,undefined} -> <<>>;
            {_,undefined} ->  <<Line:16>>;
            {_,_} ->  <<Line:16,Time:32>>
        end,
    <<?BEG,X,B/binary,?END>>.

ind_tx_ack_encode(Ack) ->
    X = case Ack of
            ok        -> ?P8_IND_TX_ACK;
            tx_nack   -> ?P8_IND_TX_NACK;
            fail_line -> ?P8_IND_TX_FAIL_LINE;
            timeout_d -> ?P8_IND_TX_TIMEOUT_D;
            timeout_l -> ?P8_IND_TX_TIMEOUT_L
        end,
    <<?BEG,X,?END>>.


%% ack = 0|1 src dest op [params]

ind_rx_encode(Ack, Src, Dest, Op, Params) ->
    ind_rx_enc_addr(Ack, Src, Dest, Op, Params).

ind_rx_enc_addr(Ack, Src, Dest, undefined, <<>>) ->
    ind_rx_enc_start(Ack, <<Src:4,Dest:4>>);
ind_rx_enc_addr(Ack, Src, Dest, Op, Params) ->
    B1 = ind_rx_enc_start(Ack, <<Src:4,Dest:4>>),
    B2 = ind_rx_enc_op(Ack, Op, Params),
    <<B1/binary,B2/binary>>.

ind_rx_enc_start(Ack, B) ->
    <<?BEG,0:1,Ack:1,?P8_IND_RX_START:6,B/binary,?END>>.

ind_rx_enc_op(Ack, Op, <<>>) ->
    ind_rx_enc_end(Ack, <<Op>>);
ind_rx_enc_op(Ack, Op, Params) ->
    B = ind_rx_enc_params(Ack, Params),
    <<?BEG,0:1,Ack:1,?P8_IND_RX_NEXT:6,Op,?END,B/binary>>.

ind_rx_enc_params(Ack, Param = <<_>>) ->
    ind_rx_enc_end(Ack, Param);
ind_rx_enc_params(Ack, <<B,Params/binary>>) ->
    B2 = ind_rx_enc_params(Ack, Params),
    <<?BEG,0:1,Ack:1,?P8_IND_RX_NEXT:6,B,?END,B2/binary>>.

ind_rx_enc_end(Ack, B) ->
    Eom = 1,
    <<?BEG,Eom:1,Ack:1,?P8_IND_RX_NEXT:6,B/binary,?END>>.

cmd_tx_encode(Flags, Src, Dest, Op, Params) ->
    B1 = cmd_tx_enc_flags(Flags),
    B2 = cmd_tx_enc_addr(Src, Dest, Op, Params),
    <<B1/binary,B2/binary>>.

cmd_tx_enc_flags(Flags) ->
    <<<<?BEG,(cmd_tx_flag(X)),Y,?END>> || {X,Y} <- Flags>>.

cmd_tx_enc_addr(Src, Dest, undefined, <<>>) ->
    cmd_tx_enc_end(<<Src:4,Dest:4>>);
cmd_tx_enc_addr(Src, Dest, Op, Params) ->
    L = [<<X>> || <<X>> <= Params],
    cmd_tx_enc_rest([<<Src:4,Dest:4>>, <<Op>> | L]).

cmd_tx_enc_rest([B]) ->
    cmd_tx_enc_end(B);
cmd_tx_enc_rest([B | T]) ->
    Rest = cmd_tx_enc_rest(T),
    <<?BEG,?P8_CMD_TX,B/binary,?END,Rest/binary>>.

cmd_tx_enc_end(B) ->
    <<?BEG,?P8_CMD_TX_EOM,B/binary,?END>>.


cmd_encode(Op, Param) ->
    <<?BEG,Op,Param/binary,?END>>.


decode(<<>>, Acc, Inc) ->
    {lists:reverse(Acc),Inc};
decode(B, Acc, Inc) ->
    case decode1(B) of
        {R,Rest} ->
            decode(Rest, [R | Acc], Inc);
        incomplete ->
            decode(<<>>, Acc, B)
    end.


-spec decode1(<<_:32,_:_*8>>) -> {cmd() | cmd_tx() | ind_rx(),binary()}.
decode1(<<?BEG,_:2,X:6,_/binary>> = B) ->
    if X =:= ?P8_IND_ERR_TIMEOUT;
       X =:= ?P8_IND_ERR_HIGH;
       X =:= ?P8_IND_ERR_LOW ->
            ind_err_decode(B);
       X =:= ?P8_IND_RX_START ->
            ind_rx_decode(B);
       X =:= ?P8_IND_ACK;
       X =:= ?P8_IND_NACK ->
            ind_ack_decode(B);
       X =:= ?P8_CMD_TX_SET_IDLE;
       X =:= ?P8_CMD_TX_SET_ACK_P;
       X =:= ?P8_CMD_TX_SET_TIMEOUT ->
            cmd_tx_decode(B);
       X =:= ?P8_IND_TX_ACK;
       X =:= ?P8_IND_TX_FAIL_LINE;
       X =:= ?P8_IND_TX_NACK;
       X =:= ?P8_IND_TX_TIMEOUT_D;
       X =:= ?P8_IND_TX_TIMEOUT_L ->
            ind_tx_ack_decode(B);
       X =:= ?P8_IND_RX_NEXT ->   % only after ?P8_IND_RX_START
            erlang:error({invalid,B});
       X =:= ?P8_IND_RX_FAILED -> % not implemented
            erlang:error({nih,B});
       true ->
          cmd_decode(B)
  end.

ind_ack_decode(<<?BEG,X,Op,?END,Rest/binary>>) ->
    V = case X of
            ?P8_IND_ACK -> ok;
            ?P8_IND_NACK -> nack
        end,
    {#ind_ack{ack = V, op = Op},Rest}.

ind_err_decode(<<?BEG,X,Rest/binary>>) ->
    T = case X of
            ?P8_IND_ERR_TIMEOUT -> timeout;
            ?P8_IND_ERR_HIGH    -> high;
            ?P8_IND_ERR_LOW     -> low
        end,
    ind_err_dec_line(Rest, #ind_err{type = T}).

ind_err_dec_line(<<?END,Rest/binary>>, R) ->
    {R,Rest};
ind_err_dec_line(<<Line:16,Rest/binary>>, R) ->
    ind_err_dec_time(Rest, R#ind_err{line = Line}).

ind_err_dec_time(<<?END,Rest/binary>>, R) ->
    {R,Rest};
ind_err_dec_time(<<Time:32,?END,Rest/binary>>, R) ->
    {R#ind_err{time = Time},Rest}.

ind_tx_ack_decode(<<?BEG,X,?END,Rest/binary>>) ->
    V = case X of
            ?P8_IND_TX_ACK       -> ok;
            ?P8_IND_TX_NACK      -> tx_nack;
            ?P8_IND_TX_FAIL_LINE -> fail_line;
            ?P8_IND_TX_TIMEOUT_D -> timeout_d;
            ?P8_IND_TX_TIMEOUT_L -> timeout_l
        end,
    {#ind_tx_ack{ack = V},Rest}.


%% [flags] addr [op [params]]

cmd_tx_decode(B) ->
    cmd_tx_dec_flags(B, #cmd_tx{}).

cmd_tx_dec_flags(<<?BEG,X,Y,?END,Rest/binary>> = B,
                 #cmd_tx{flags = Flags} = R) ->
    case X of
        ?P8_CMD_TX_SET_IDLE ->
            R2 = R#cmd_tx{flags = [{idle,Y} | Flags]},
            cmd_tx_dec_flags(Rest, R2);
        ?P8_CMD_TX_SET_ACK_P ->
            R2 = R#cmd_tx{flags = [{ack_p,Y} | Flags]},
            cmd_tx_dec_flags(Rest, R2);
        ?P8_CMD_TX_SET_TIMEOUT ->
            R2 = R#cmd_tx{flags = [{timeout,Y} | Flags]},
            cmd_tx_dec_flags(Rest, R2);
        _ ->
            R2 = R#cmd_tx{flags = lists:reverse(Flags)},
            cmd_tx_dec_addr(B, R2)
    end.

cmd_tx_dec_addr(<<?BEG,X,Src:4,Dest:4,?END,Rest/binary>>, R) ->
    R2 = R#cmd_tx{src = Src, dest = Dest},
    case X of
        ?P8_CMD_TX ->
            cmd_tx_dec_op(Rest, R2);
        ?P8_CMD_TX_EOM ->
            cmd_tx_dec_end(Rest, R2)
    end.

cmd_tx_dec_op(<<?BEG,X,Op,?END,Rest/binary>>, R) ->
    R2 = R#cmd_tx{op = Op},
    case X of
        ?P8_CMD_TX ->
            cmd_tx_dec_params(Rest, R2);
        ?P8_CMD_TX_EOM ->
            cmd_tx_dec_end(Rest, R2)
    end.

%% TODO: better to cons binaries on a list and then reverse it?
cmd_tx_dec_params(<<?BEG,X,B/binary>>, #cmd_tx{params = Params} = R) ->
    [P, Rest] = binary:split(B, <<?END>>),
    R2 = R#cmd_tx{params = <<Params/binary,P/binary>>},
    case X of
        ?P8_CMD_TX ->
            cmd_tx_dec_params(Rest, R2);
        ?P8_CMD_TX_EOM ->
            cmd_tx_dec_end(Rest, R2)
    end.

cmd_tx_dec_end(Rest, #cmd_tx{flags = Flags} = R) ->
    {R#cmd_tx{flags = lists:reverse(Flags)},Rest}.


-spec ind_rx_decode(<<_:32,_:_*8>>) -> {ind_rx(),binary()}.

ind_rx_decode(<<?BEG,Eom:1,Ack:1,
                ?P8_IND_RX_START:6,Src:4,Dest:4,
                ?END,Rest/binary>>) ->
    R = #ind_rx{ack = Ack, src = Src, dest = Dest},
    if Eom =:= 0 ->
            ind_rx_dec_op(Rest, R);
       true ->
            ind_rx_dec_end(Rest, R)
    end.

ind_rx_dec_op(<<?BEG,Eom:1,_:1,
                ?P8_IND_RX_NEXT:6,Op,
                ?END,Rest/binary>>, R) ->
    R2 = R#ind_rx{op = Op},
    if Eom =:= 0 ->
          ind_rx_dec_next(Rest, R2);
       true ->
          ind_rx_dec_end(Rest, R2)
    end;
ind_rx_dec_op(<<>>, _) ->
    incomplete.

ind_rx_dec_next(<<?BEG,Eom:1,_Ack:1,?P8_IND_RX_NEXT:6,B/binary>>,
                #ind_rx{params = Params} = R) ->
    [P, Rest] = binary:split(B, <<?END>>),
    R2 = R#ind_rx{params = <<Params/binary,P/binary>>},
    if Eom =:= 0 ->
          ind_rx_dec_next(Rest, R2);
       true ->
          ind_rx_dec_end(Rest, R2)
    end;
ind_rx_dec_next(<<>>, _) ->
    incomplete.

ind_rx_dec_end(Rest, R) ->
    {R,Rest}.

-spec cmd_decode(<<_:32,_:_*8>>) -> {cmd(),binary()}.
cmd_decode(<<?BEG,Op,B/binary>>) ->
    [Param, Rest] = binary:split(B, <<?END>>),
    {#cmd{op = Op, param = Param},Rest}.
