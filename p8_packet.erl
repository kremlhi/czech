-module(p8_packet).

-include("p8.hrl").
-include("czech.hrl").

-export([decode/1, encode/1, hex_to_bin/1, hexdump_to_tuple/1, test/0]).
-export([cmd_tx_atoi_flag/1]).

-spec encode([packets()]) -> binary().
encode(L) ->
    << <<(encode1(X))/binary>> || X <- L >>.

-spec encode1(packets()) -> {<<_:32,_:_*8>>,binary()}.
encode1(R) ->
    case R of
        #cmd{op = Op,param = Param} ->
            cmd_encode(Op, Param);
        #cmd_tx{flags = Flags,
                src = Src, dest = Dest,
                op = Op, params = Params} ->
            cmd_tx_encode(Flags, Src, Dest, Op, Params);
        #ind_ack{ack = Ack, op = Op} ->
            ind_ack_encode(Ack, Op);
        #ind_err{type = Type, param = Param} ->
            ind_err_encode(Type, Param);
        #ind_tx_ack{ack = Ack} ->
            ind_tx_ack_encode(Ack);
        #ind_rx{ack = Ack, src = Src, dest = Dest,
                op = Op, params = Params} ->
            ind_rx_encode(Ack, Src, Dest, Op, Params)
    end.

ind_ack_ttoi(X) ->
    case X of
        ok -> ?P8_IND_ACK;
        {error,nack} -> ?P8_IND_NACK
    end.

ind_err_ttoi(X) ->
    case X of
        timeout -> ?P8_IND_ERR_TIMEOUT;
        high -> ?P8_IND_ERR_HIGH;
        low -> ?P8_IND_ERR_LOW
    end.

ind_tx_ack_ttoi(X) ->
    case X of
        ok -> ?P8_IND_TX_ACK;
        tx_nack -> ?P8_IND_TX_NACK;
        fail_line -> ?P8_IND_TX_FAIL_LINE;
        timeout_d -> ?P8_IND_TX_TIMEOUT_D;
        timeout_l -> ?P8_IND_TX_TIMEOUT_L
    end.

ind_ack_encode(Ack, Op) ->
    <<?BEG,(ind_ack_ttoi(Ack)),Op,?END>>.

ind_err_encode(Type, Param) ->
    <<?BEG,(ind_err_ttoi(Type))/binary,Param/binary,?END>>.

ind_tx_ack_encode(Ack) ->
    <<?BEG,(ind_tx_ack_ttoi(Ack)),?END>>.


%% ack=0|1 src dest op [params]

ind_rx_encode(Ack, Src, Dest, Op, Params) ->
    ind_rx_enc_addr(Ack, Src, Dest, Op, Params).

ind_rx_enc_addr(Ack, Src, Dest, undefined, []) ->
    ind_rx_enc_start(Ack, <<Src:4,Dest:4>>);
ind_rx_enc_addr(Ack, Src, Dest, Op, Params) ->
    B1 = ind_rx_enc_start(Ack, <<Src:4,Dest:4>>),
    B2 = ind_rx_enc_op(Ack, Op, Params),
    <<B1/binary,B2/binary>>.

ind_rx_enc_start(Ack, B) ->
    <<?BEG,0:1,Ack:1,?P8_IND_RX_START:6,B/binary,?END>>.

ind_rx_enc_op(Ack, Op, []) ->
    ind_rx_enc_end(Ack, <<Op>>);
ind_rx_enc_op(Ack, Op, Params) ->
    B = ind_rx_enc_params(Ack, Params),
    <<?BEG,0:1,Ack:1,?P8_IND_RX_NEXT:6,Op,?END,B/binary>>.

ind_rx_enc_params(Ack, [Param]) ->
    ind_rx_enc_end(Ack, Param);
ind_rx_enc_params(Ack, [P | Params]) ->
    B = ind_rx_enc_params(Ack, Params),
    <<?BEG,0:1,Ack:1,?P8_IND_RX_NEXT:6,P/binary,?END,B/binary>>.

ind_rx_enc_end(Ack, B) ->
    Eom = 1,
    <<?BEG,Eom:1,Ack:1,?P8_IND_RX_NEXT:6,B/binary,?END>>.

cmd_tx_encode(Flags, Src, Dest, Op, Params) ->
    B1 = cmd_tx_enc_flags(Flags),
    B2 = cmd_tx_enc_addr(Src, Dest, Op, Params),
    <<B1/binary,B2/binary>>.

cmd_tx_itoa_flag(X) ->
    case X of
        ?P8_CMD_TX_SET_IDLE -> idle;
        ?P8_CMD_TX_SET_ACK_P -> ack_p;
        ?P8_CMD_TX_SET_TIMEOUT -> timeout
    end.

cmd_tx_atoi_flag(X) ->
    case X of
        idle -> ?P8_CMD_TX_SET_IDLE;
        ack_p -> ?P8_CMD_TX_SET_ACK_P;
        timeout -> ?P8_CMD_TX_SET_TIMEOUT
    end.

cmd_tx_enc_flags(Flags) ->
    <<<<?BEG,(cmd_tx_atoi_flag(X)),Y,?END>> || {X,Y} <- Flags>>.

cmd_tx_enc_addr(Src, Dest, undefined, []) ->
    cmd_tx_enc_end(<<Src:4,Dest:4>>);
cmd_tx_enc_addr(Src, Dest, Op, Params) ->
    cmd_tx_enc_rest([<<Src:4,Dest:4>>, <<Op>> | Params]).

cmd_tx_enc_rest([B]) ->
    cmd_tx_enc_end(B);
cmd_tx_enc_rest([B | T]) ->
    Rest = cmd_tx_enc_rest(T),
    <<?BEG,?P8_CMD_TX,B/binary,?END,Rest/binary>>.

cmd_tx_enc_end(B) ->
    <<?BEG,?P8_CMD_TX_EOM,B/binary,?END>>.


cmd_encode(Op, Param) ->
    <<?BEG,Op,Param/binary,?END>>.

%% throw_away(0, L) ->
%%     L;
%% throw_away(N, [H | T]) when byte_size(H) > N ->
%%     <<_:N/binary,Rest/binary>> = H,
%%     throw_away(0, [Rest | T]);
%% throw_away(N, [H | T]) ->
%%     throw_away(N - byte_size(H), T).


%% decode_list([], Acc) ->
%%     lists:reverse(Acc);
%% decode_list([<<>>|Bs], Acc) ->
%%     decode_list(Bs, Acc);
%% decode_list([B|Bs], Acc) ->
%%     try  B0 = list_to_binary([B|Bs]),
%%          {R,Rest} = decode1(B0),
%%          Bs2 = throw_away(byte_size(B0) - byte_size(Rest), [B|Bs]),
%%          decode_list(Bs2, [R|Acc])
%%     catch _:_ -> decode_list(Bs, [{err,B} | Acc])
%%     end.


-spec decode(binary()) -> {[packets()],binary()}.
decode(B) ->
    decode(B, [], <<>>).

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
decode1(<<?BEG,B0/binary>> = B) ->
  case B0 of
      <<X,_/binary>>
        when X =:= ?P8_IND_ERR_TIMEOUT;
             X =:= ?P8_IND_ERR_HIGH;
             X =:= ?P8_IND_ERR_LOW ->
          ind_err_decode(B);
      <<_:2,?P8_IND_RX_START:6,_/binary>> ->
          ind_rx_decode(B);
      <<X,_/binary>>
        when X =:= ?P8_IND_ACK;
             X =:= ?P8_IND_NACK ->
          ind_ack_decode(B);
      <<X,_/binary>>
        when X =:= ?P8_CMD_TX_SET_IDLE;
             X =:= ?P8_CMD_TX_SET_ACK_P;
             X =:= ?P8_CMD_TX_SET_TIMEOUT ->
          cmd_tx_decode(B);
      <<X,_/binary>>
        when X =:= ?P8_IND_TX_ACK;
             X =:= ?P8_IND_TX_FAIL_LINE;
             X =:= ?P8_IND_TX_NACK;
             X =:= ?P8_IND_TX_TIMEOUT_D;
             X =:= ?P8_IND_TX_TIMEOUT_L ->
          ind_tx_ack_decode(B);
      _ ->
          cmd_decode(B)
  end.

ind_ack_itot(X) ->
    case X of
        ?P8_IND_ACK -> ok;
        ?P8_IND_NACK -> nack
    end.

ind_err_itot(X) ->
    case X of
        ?P8_IND_ERR_TIMEOUT -> timeout;
        ?P8_IND_ERR_HIGH -> high;
        ?P8_IND_ERR_LOW -> low
    end.

ind_tx_ack_itot(X) ->
    case X of
        ?P8_IND_TX_ACK -> ok;
        ?P8_IND_TX_NACK -> tx_nack;
        ?P8_IND_TX_FAIL_LINE -> fail_line;
        ?P8_IND_TX_TIMEOUT_D -> timeout_d;
        ?P8_IND_TX_TIMEOUT_L -> timeout_l
    end.


ind_ack_decode(<<?BEG,X,Op,?END,Rest/binary>>)
  when X =:= ?P8_IND_ACK;
       X =:= ?P8_IND_NACK ->
    {#ind_ack{ack = ind_ack_itot(X), op = Op},Rest}.

ind_err_decode(<<?BEG,X,B/binary>>)
  when X =:= ?P8_IND_ERR_TIMEOUT;
       X =:= ?P8_IND_ERR_HIGH;
       X =:= ?P8_IND_ERR_LOW ->
    [B2, Rest] = binary:split(B, <<?END>>),
    {#ind_err{type = ind_err_itot(X), param = B2},Rest}.

ind_tx_ack_decode(<<?BEG,X,?END,Rest/binary>>)
  when X =:= ?P8_IND_TX_ACK;
       X =:= ?P8_IND_TX_NACK;
       X =:= ?P8_IND_TX_FAIL_LINE;
       X =:= ?P8_IND_TX_TIMEOUT_D;
       X =:= ?P8_IND_TX_TIMEOUT_L ->
    {#ind_tx_ack{ack = ind_tx_ack_itot(X)},Rest}.



%% [flags] ack_p addr [op [params]]

cmd_tx_decode(B) ->
    cmd_tx_dec_flags(B, #cmd_tx{}).

cmd_tx_dec_flags(<<?BEG,X,Y,?END,Rest/binary>>, #cmd_tx{flags=Flags} = R)
  when X =:= ?P8_CMD_TX_SET_ACK_P ->
    F = {cmd_tx_itoa_flag(X),Y},
    cmd_tx_dec_addr(Rest, R#cmd_tx{flags = lists:reverse([F | Flags])});
cmd_tx_dec_flags(<<?BEG,X,Y,?END,Rest/binary>>, #cmd_tx{flags = Flags} = R) ->
    F = {cmd_tx_itoa_flag(X),Y},
    cmd_tx_dec_flags(Rest, R#cmd_tx{flags = [F | Flags]}).


cmd_tx_dec_addr(<<?BEG,?P8_CMD_TX,Src:4,Dest:4,?END,Rest/binary>>, R) ->
    cmd_tx_dec_op(Rest, R#cmd_tx{src = Src, dest = Dest});
cmd_tx_dec_addr(<<?BEG,?P8_CMD_TX_EOM,Src:4,Dest:4,?END,Rest/binary>>, R) ->
    cmd_tx_dec_end(Rest, R#cmd_tx{src = Src, dest = Dest}).

cmd_tx_dec_op(<<?BEG,?P8_CMD_TX,Op,?END,Rest/binary>>, R) ->
    cmd_tx_dec_params(Rest, R#cmd_tx{op = Op});
cmd_tx_dec_op(<<?BEG,?P8_CMD_TX_EOM,Op,?END,Rest/binary>>, R) ->
    cmd_tx_dec_end(Rest, R#cmd_tx{op = Op}).

cmd_tx_dec_params(<<?BEG,?P8_CMD_TX,B/binary>>, #cmd_tx{params = Acc} = R) ->
    [B2, Rest] = binary:split(B, <<?END>>),
    cmd_tx_dec_params(Rest, R#cmd_tx{params = [B2 | Acc]});
cmd_tx_dec_params(<<?BEG,?P8_CMD_TX_EOM,B/binary>>,
            #cmd_tx{params = Acc} = R) ->
    [B2, Rest] = binary:split(B, <<?END>>),
    cmd_tx_dec_end(Rest, R#cmd_tx{params = [B2 | Acc]}).

cmd_tx_dec_end(Rest, #cmd_tx{params = Params, flags = Flags} = R) ->
    {R#cmd_tx{params = lists:reverse(Params),
              flags = lists:reverse(Flags)},Rest}.


-spec ind_rx_decode(<<_:32,_:_*8>>) -> {ind_rx(),binary()}.

ind_rx_decode(<<?BEG,_:1,Ack:1,?P8_IND_RX_START:6,_/binary>> = B) ->
    ind_rx_dec_addr(B, #ind_rx{ack = Ack}).

ind_rx_dec_addr(<<?BEG,Eom:1,_Ack:1,?P8_IND_RX_START:6,Src:4,Dest:4,?END,
                Rest/binary>>, #ind_rx{ack = Ack} = R) ->
    if Eom =:= 1 ->
            ind_rx_dec_end(Rest, R#ind_rx{src = Src, dest = Dest});
       true ->
            ind_rx_dec_op(Rest, R#ind_rx{src = Src, dest = Dest})
    end.

ind_rx_dec_op(<<?BEG,Eom:1,_Ack:1,?P8_IND_RX_NEXT:6,Op,?END,Rest/binary>>,
              #ind_rx{ack = Ack} = R) ->
    if Eom =:= 1 ->
          ind_rx_dec_end(Rest, R#ind_rx{op = Op});
       true ->
          ind_rx_dec_next(Rest, R#ind_rx{op = Op})
    end;
ind_rx_dec_op(<<>>, _) ->
    incomplete.

ind_rx_dec_next(<<?BEG,Eom:1,_Ack:1,?P8_IND_RX_NEXT:6,B/binary>>,
                #ind_rx{ack = Ack, params = Acc} = R) ->
    [B2, Rest] = binary:split(B, <<?END>>),
    if Eom =:= 1 ->
          ind_rx_dec_end(Rest, R#ind_rx{params = [B2 | Acc]});
       true ->
          ind_rx_dec_next(Rest, R#ind_rx{params = [B2 | Acc]})
  end;
ind_rx_dec_next(<<>>, _) ->
    incomplete.

ind_rx_dec_end(Rest, #ind_rx{params = Acc} = R) ->
    {R#ind_rx{params = lists:reverse(Acc)},Rest}.

-spec cmd_decode(<<_:32,_:_*8>>) -> {cmd(),binary()}.
cmd_decode(<<?BEG,Op,B/binary>>) ->
    [Param, Rest] = binary:split(B, <<?END>>),
    {#cmd{op = Op, param = Param},Rest}.

uncrap(B) ->
    lists:last(binary:split(B, <<" ">>, [global])).

hex_to_bin(B) ->
    L = binary:split(B, <<":">>, [global]),
    << <<(binary_to_integer(<<X,Y>>, 16))>> || <<X,Y>> <- L >>.

hexdump_to_tuple(B) ->
    L = binary:split(B, <<"\n">>, [global]),
    [{[Op],hex_to_bin(uncrap(FdSzHex))} ||
        <<Op,_:10/binary," ",FdSzHex/binary>> <- L].


cmp(<<>>, <<>>, N) ->
    {N,{<<>>,<<>>}};
cmp(<<B,X/binary>>, <<B,Y/binary>>, N) ->
    cmp(X, Y, N+1);
cmp(X, Y, N) ->
    {N,{X,Y}}.


rb() ->
    {ok,B1} = file:read_file("dtrace.hex"),
    list_to_binary([X || {"r",X} <- hexdump_to_tuple(B1)]).

encdec_r_test() ->
    B = rb(),
    {L,<<>>} = decode(B),
    {_,{<<>>,<<>>}} = cmp(B, encode(L), 0).

wb() ->
    {ok,B1} = file:read_file("dtrace.hex"),
    list_to_binary([X || {"w",X} <- hexdump_to_tuple(B1)]).

%% b() ->
%%     {ok,B1} = file:read_file("dtrace.hex"),
%%     list_to_binary([X || {_,X} <- group(hexdump_to_tuple(B1))]).

encdec_w_test() ->
    B = wb(),
    {L,<<>>} = decode(B),
    {_,{<<>>,<<>>}} = cmp(B, encode(L), 0).

test() ->
    [encdec_r_test(),
     encdec_w_test()].
