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

-export([test/0]).

test() when Par3 == "Pass" ->
	"You did it!".
