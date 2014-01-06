-module(fixed_rate).

-export([run_parallel/4, run_and_call_back/6, run_parallel/5, run/3, run/4, run_and_wait/2]).

run_parallel(Processes, Fun, Args, Rate) ->
    run_parallel(Processes, Fun, Args, Rate, 1).

run_parallel(Processes, Fun, Args, Rate, CheckEvery) ->
    ArgGroups = split(Processes, Args),
    Ref = make_ref(),
    [spawn(?MODULE, run_and_call_back, [self(), Ref, Fun, ArgGroup, (Rate*1.0)/Processes, CheckEvery])
     || ArgGroup <- ArgGroups],
    WaitForGroup = fun(_, {Times, Results}) ->
        receive
            {Ref, {Elapsed, Result}} -> {[Elapsed|Times], [Result|Results]}
        end
    end,
    {Times, Results} = lists:foldl(WaitForGroup, {[], []}, lists:seq(1, Processes)),
    Elapsed = lists:max(Times),
    io:format("\n", []),
    report(length(Args), Elapsed),
    {Elapsed, Results}.

run_and_call_back(From, Ref, Fun, Args, Rate, CheckEvery) ->
    {Elapsed, Results} = run(Fun, Args, Rate, CheckEvery),
    From ! {Ref, {Elapsed, Results}}.

run(Fun, Args, Rate) ->
    run(Fun, Args, Rate, 1).

run(Fun, ArgsList, Rate, CheckEvery) ->
    TimePerMsg = 1.0 / Rate * 1000000.0, %micros
    {_, _, N, StartTime, _, Results} =
    lists:foldl(fun run_and_wait/2, {Fun, TimePerMsg, 0, os:timestamp(), CheckEvery, []}, ArgsList),
    Elapsed    = timer:now_diff(os:timestamp(), StartTime) / 1000000.0,
    report(N, Elapsed),
    {Elapsed, Results}.

run_and_wait(Args, {Fun, TimePerMsg, N, StartTime, CheckEvery, Acc}) when is_list(Args) ->
    case N rem CheckEvery =:= 0 of
        true ->
            TimeElapse = timer:now_diff(os:timestamp(), StartTime),
            case (TimePerMsg * (N+1) * 1.0) - TimeElapse * 1.0 of
                TTS when TTS > 0.0 ->
                    timer:sleep(round(TTS/1000.0));
                _ -> noop
            end;
        false -> noop
    end,
    Result = apply(Fun, Args),
    {Fun, TimePerMsg, N + 1, StartTime, CheckEvery, [Result|Acc]};

run_and_wait(Arg, {Fun, TimePerMsg, N, StartTime, CheckEvery, Acc}) ->
    run_and_wait([Arg], {Fun, TimePerMsg, N, StartTime, CheckEvery, Acc}).


split(N, List) ->
    split_internal(List, lists:duplicate(N, [])).

split_internal([], Groups) ->
    Groups;
split_internal(Source, Groups) ->
    Fun = fun
        (Group, {[],               GroupAcc}) -> {[],               [Group|GroupAcc]};
        (Group, {[Val|SourceRest], GroupAcc}) -> {SourceRest, [[Val|Group]|GroupAcc]}
    end,
    {SourceNew, GroupsNew} = lists:foldr(Fun, {Source, []}, Groups),
    split_internal(SourceNew, GroupsNew).

report(N, Elapsed) ->
    io:format("~p samples took ~p s.\nRate: ~p ops\n", [N, Elapsed, N/Elapsed]).

