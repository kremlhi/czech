-module(el_proxy).

-export([start_link/0, add_handler/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {left, right}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

add_handler(Pid) ->
    gen_server:call(?SERVER, {add_handler,Pid}).

stop() ->
    gen_server:stop(?SERVER).

    
init(_Args) ->
    process_flag(trap_exit, true),
    {ok,Rpid} = czech:subscribe(self()),
    {ok,#state{right=Rpid}}.

handle_call({add_handler,Lpid}, _, State) ->
    link(Lpid),
    {reply,{ok,self()},State#state{left=Lpid}}.

handle_cast(_, State) ->
    {noreply,State}.

handle_info({'EXIT',_Pid,normal}, #state{left=Lpid} = State) ->
    if Lpid =/= undefined ->
            unlink(Lpid),
            exit(Lpid, normal);
       true -> ok
    end,
    {stop,normal,State};
handle_info({'EXIT',_Pid,_}, State) ->
    timer:sleep(50),
    {ok,Pid} = czech:subscribe(self()),
    {noreply,State#state{right=Pid}};
handle_info(Msg, #state{left=Lpid} = State) when Lpid /= undefined ->
    Lpid ! Msg,
    {noreply,State};
handle_info(Msg, State) ->
    error_logger:warning_msg("dropping ~w~n", [Msg]),
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

terminate(_, _) ->
    ok.
