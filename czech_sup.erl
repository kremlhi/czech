-module(czech_sup).
-behavior(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local,?SERVER}, ?MODULE, []).

init(_Args) ->
    Child1 = #{id       => czech,
               start    => {czech,start_link,[]},
               restart  => permanent,
               shutdown => 1000,
               type     => worker,
               modules  => [czech]},
    Child2 = #{id       => el_proxy,
               start    => {el_proxy,start_link,[]},
               restart  => permanent,
               shutdown => 1000,
               type     => worker,
               modules  => [el_proxy]},
    Flags = #{strategy  => one_for_one,
              intensity => 1,
              period    => 5},
    {ok, {Flags,[Child1, Child2]}}.
