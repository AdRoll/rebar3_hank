-module(warnings_A).

-export([single_fun/2, multi_fun/3, 'unicode_αβåö'/1, with_nif_stub/2]).

-doc """
`_Arg2` is unused.
""".
single_fun(Arg1, _Arg2) ->
    Arg1.

-doc """
A multi-clause function with unused 3rd param.
""".
multi_fun(undefined, _, _) ->
    ok;
multi_fun(Arg1, Arg2, _Arg3) when is_binary(Arg1) ->
    Arg2;
multi_fun(Arg1, _, _) ->
    Arg1.

-doc """
Unicode should be supported and not break!
""".
'unicode_αβåö'(_Config) ->
    unused_param.

-doc """
NIF stub should not affect the warning.
""".
with_nif_stub(_A, B) ->
    {only, B, is_used};
with_nif_stub(_, _) ->
    erlang:nif_error('A_is_unused').
