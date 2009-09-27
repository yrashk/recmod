-module(baserecmod).
-compile({parse_transform, recmod}).
-include("records.hrl").
-export([argless/0,somefun/1,someotherfun/0, defaultimports/0,localcall/0,localcall/1]).

argless() ->
    [{this, THIS},
     {self, SELF},
     {field1, Field1},
     {field2, Field2},
     {field3, Field3}].

somefun(Arg) ->
    [proplists:property(arg,Arg)|argless()].

someotherfun() ->
    somefun(otherfun).

defaultimports() ->
   a = element(1, {a}).

localcall() ->
   this().

localcall(A) ->
   this(A).

this() ->
    THIS.

this(A) ->
    {THIS,A}.
