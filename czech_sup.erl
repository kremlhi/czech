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
    Mod = p8,
    Adpt = {Mod, {Mod,start_link,[]},
            permanent, 1000, worker, [Mod]},
    Czech = {czech, {czech,start_link,[Mod]},
             permanent, 1000, worker, [czech]},
    {ok, {{one_for_one,3,10},
          [Adpt, Czech]}}.
