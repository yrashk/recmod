-module(extrecmod).
-compile({parse_transform, recmod}).
-extends(baserecmod).
-include("records.hrl").
-export([somefun/1, recguarded/0]).

somefun(Arg) when Otherfield == "password" ->
    Arg.

recguarded() when Field1 > 1 ->
    emore;
recguarded() when Field1 =< 1 ->
    eless.    
