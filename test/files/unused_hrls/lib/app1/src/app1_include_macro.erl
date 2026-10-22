-module(app1_include_macro).

-define(HEADER, "some_header.hrl").

-include(?HEADER).
