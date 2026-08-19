-module(parse_transf).

-export([parse_transform/2]).

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: [compile:option()].

-doc """
Options is purposefully left unused, here.
""".
parse_transform(Forms, _Options) ->
    Forms.
