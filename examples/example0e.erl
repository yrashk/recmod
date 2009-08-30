-module(example0e).
-extends(example0).
-compile({parse_transform, recmod}).
-record(example0,
	{
	  par1,
	  par2 = "Default"
	 }).
-record(example0e,
        { par1, par3 }).

-export([test1/0]).

test1()  ->
    record_fields().
