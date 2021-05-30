-module(example).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
% API
-export([ask/1]).

%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%

ask(Question) ->
    case poolboy:transaction(operator_pool,
                            fun(Worker) ->
                                gen_server:call(Worker, {answer, Question})
                            end,
                            1000)
    of
        busy -> "Operators are busy!~n";
        Ans -> Ans
    end.

%%%%%%%%%%%%%%%
%% REQUIRED
%%%%%%%%%%%%%%%

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, example_sup}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, Pools} = application:get_env(example, pools),
    PoolSpecs =
        lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                     PoolArgs =
                         [{name, {local, Name}}, {worker_module, example_worker}] ++ SizeArgs,
                     poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                  end,
                  Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
