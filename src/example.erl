-module(example).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
% API
-export([ask/1, ask/2]).

%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%

ask(Question) ->
    spawn_link(fun() ->
                  case poolboy:transaction(operator_pool,
                                           fun(Worker) ->
                                              gen_server:call(Worker, {answer, Question})
                                           end,
                                           1000)
                  of
                      busy -> send_to_user("Operators are busy!~n");
                      Ans -> send_to_user(Ans)
                  end
               end),
    ok.

ask(Question, N) when N > 0 ->
    ask(Question),
    ask(Question, N - 1);
ask(_Question, _N) ->
    ok.

send_to_user(Msg) ->
    io:format("~p~n", [Msg]). % TODO

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
