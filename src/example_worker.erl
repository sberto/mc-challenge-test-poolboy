-module(example_worker).
-behaviour(gen_statem).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_info/2, terminate/2,
         code_change/3]).
-export([idle/3, associated/3, callback_mode/0]).
-export([warn_user_timeout/1]).
-record(state, {client = undefined}).

%%%%%%%%%%%%%%%%%%%%
%% OPERATOR-CLIENT
%%%%%%%%%%%%%%%%%%%%

warn_user_timeout(Pid) ->
    io:format("To ~p: your time is finished!~n", [Pid]). % TODO

%%%%%%%%%%%%%%%
%% STATES
%%%%%%%%%%%%%%%

idle({call, {Pid, _Ref}}, {answer, _Question}, State) ->
    {next_state, associated, State#state{client = Pid}, [postpone, {state_timeout, 100, back_to_idle}]};
idle(B,C,D) ->
    io:format("~p:~p:~p~n", [B,C,D]),
    {keep_state, D}.
            

associated({call, From = {Pid, _Ref}}, {answer, N}, State=#state{client=Pid}) when is_number(N) ->
    Ans = case N rem 2 of
            0 -> "even";
            1 -> "odd"
    end,
    {next_state, associated, State#state{client = From}, {reply, From, Ans}};
associated({call, From = {Pid, _Ref}}, {answer, _Question}, State=#state{client=Pid}) ->
    {next_state, associated, State#state{client = From}, {reply, From, "My pid is "++pid_to_list(self())}};
associated({call, From = {_OtherPid, _Ref}}, {answer, _Question}, State=#state{client=Pid}) ->
    {keep_state, State#state{client = From}, {reply, From, busy}};
associated(state_timeout,back_to_idle,State = #state{client=Pid}) ->
    io:format("Time ended.~n"),
    {next_state, idle, State#state{client = undefined}};
associated(B,C,D) ->
    io:format("~p:~p:~p~n", [B,C,D]),
    {keep_state, D}.
    
%%%%%%%%%%%%%%%
%% REQUIRED
%%%%%%%%%%%%%%%

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

init(_Args) ->
    {ok, idle, #state{}}.

callback_mode() ->
    state_functions.


handle_info(Info, State) ->
    io:format('Info ~p', [Info]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.