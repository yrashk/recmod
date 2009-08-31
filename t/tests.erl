-module(tests).
-include_lib("epitest/include/epitest.hrl").
-include_lib("eunit/include/eunit.hrl").

test("Empty recmod should have record_fields/0 returning record names") ->
    [{f,
      fun () ->
	      ?assertEqual([data],emptyrecmod:record_fields())
      end}
    ].
      
