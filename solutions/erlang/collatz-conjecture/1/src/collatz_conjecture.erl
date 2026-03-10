-module(collatz_conjecture).

-export([steps/1]).

loop(Count, N) ->
    if N < 1 ->
           error(badarg);
       N =:= 1 ->
           Count;
       N rem 2 =:= 0 ->
           loop(Count + 1, N div 2);
       true ->
           loop(Count + 1, N * 3 + 1)
    end.

steps(N) ->
    loop(0, N).
