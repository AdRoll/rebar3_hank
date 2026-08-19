-module(weird).
-moduledoc """
This module contains weird function calls that used to crash Hank.
""".

-compile([export_all]).

-record(to, {a}).

record(Access) ->
    (Access#to.a):record(field).

a_case(Statement) ->
    using:case Statement of
        to ->
            determine;
        the ->
            function
    end().
