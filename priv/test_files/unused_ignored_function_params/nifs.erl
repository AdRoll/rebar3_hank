-module(nifs).

-export([hank_should_ignore_this_function_unused_params/2, and_also_this_one/3, and_this_one_too/3]).

hank_should_ignore_this_function_unused_params(_Ignore, _Me) ->
    erlang:nif_error(undefined).

and_also_this_one(_Ignore, Me, _Too) ->
    mylog:info(Me), % This line should not affect anything!
    erlang:nif_error(undefined, []).

and_this_one_too(_Ignore, _Me, Again) ->
    Msg = case Again of
      something -> "A message!";
      _ -> "Another message!"
    end,
    erlang:nif_error(Msg, [ignore, Again]).
