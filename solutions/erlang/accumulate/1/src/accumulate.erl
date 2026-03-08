-module(accumulate).

-import(lists, [reverse/1]).

-export([loop/3, accumulate/2]).

% Internal helper function to perform the accumulation
loop(Acc, _, []) ->
    reverse(Acc);
loop(Acc, Fn, [Hd | Tl]) ->
    Copy = [Fn(Hd) | Acc],
    loop(Copy, Fn, Tl).

%%
%% given a fun and a list, apply fun to each list item replacing list item with fun's return value.
%%
-spec accumulate(fun((A) -> B), [A]) -> [B].
accumulate(Fn, List) ->
    loop([], Fn, List).
