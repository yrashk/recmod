-module(baserecmod).
-compile({parse_transform, recmod}).
-include("records.hrl").
-export([st/0,argless/0,somefun/1,someotherfun/0, defaultimports/0,localcall/0,localcall/1, recguarded/0,multiarg/2]).
-static([st/0]).

st() ->
    static.

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

recguarded() when Field1 > 1 ->
    more;
recguarded() when Field1 =< 1 ->
    less.    

multiarg(A1,A2) ->
    {A1,A2}.
