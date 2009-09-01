-module(extrecmod).
-compile({parse_transform, recmod}).
-extends(baserecmod).
-include("records.hrl").
-export([somefun/1]).

somefun(Arg) when Otherfield == "password" ->
    Arg.

