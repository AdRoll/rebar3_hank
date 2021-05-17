-module(macros).

%% There must be a paresable record
-record(a_rec, {unused_field}).
%% But also an "unparseable" one
-record(?MODULE, {}).
