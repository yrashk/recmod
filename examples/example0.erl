-module(example0).
-compile({parse_transform, recmod}).
-record(example0,
	{
	  par1,
	  par2 = "Default"
	 }).
-export([test/0]).

test()  ->
    record_fields().