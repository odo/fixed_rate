-module(fixed_rate).

-export([run/3, run/4, run_and_wait/2]).

run(Fun, Args, Rate) ->
    run(Fun, Args, Rate, 1).

run(Fun, ArgsList, Rate, CheckEvery) ->
    TimePerMsg = 1.0 / Rate * 1000000.0, %micros
    {_, _, N, StartTime, _, Results} =
    lists:foldl(fun run_and_wait/2, {Fun, TimePerMsg, 0, os:timestamp(), CheckEvery, []}, ArgsList),
    Elapsed    = timer:now_diff(os:timestamp(), StartTime) / 1000000.0,
    RateActual = N / Elapsed,
    io:format("~p samples took ~p s.\nRate: ~p ops\n", [N, Elapsed, RateActual]),
    {RateActual, Results}.

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
