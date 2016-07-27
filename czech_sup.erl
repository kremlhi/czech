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
    Child = #{id       => czech,
              start    => {czech,start_link,[]},
              restart  => permanent,
              shutdown => 1000,
              type     => worker,
              modules  => [czech]},
    Flags = #{strategy  => one_for_one,
              intensity => 1,
              period    => 5},
    {ok, {Flags,[Child]}}.
