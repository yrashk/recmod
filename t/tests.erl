-module(tests).
-include_lib("epitest/include/epitest.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

test("Empty recmod should have record_fields/0 returning record names") ->
    [{f,
      fun () ->
	      ?assertEqual([data],emptyrecmod:record_fields())
      end}
    ];

test("Empty recmod should have readers") ->
    [{f,
      fun () ->
	      ?assertEqual("test", (#emptyrecmod{data="test"}):data())
      end}
    ];

test("Empty recmod should have default object construction") ->
    [{f,
      fun () ->
	      ?assertEqual(#emptyrecmod{}, (emptyrecmod:new()))
      end}
    ];

test("Base recmod should have record_fields/0 returning record names") ->
    [{f,
      fun () ->
	      ?assertEqual([field1,field2,field3],baserecmod:record_fields())
      end}
    ];

test("EmBasepty recmod should have readers") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual("test1", Mod:field1()),
	      ?assertEqual("test2", Mod:field2()),
	      ?assertEqual("test3", Mod:field3())
      end}
    ];

test("Base recmod should have default object construction") ->
    [{f,
      fun () ->
	      ?assertEqual(#baserecmod{}, (baserecmod:new()))
      end}
    ];

test("Argumentless function should have THIS assigned") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual(Mod, proplists:get_value(this, Mod:argless()))
      end}];

test("Argumentless function should have all parameters assigned") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual("test1", proplists:get_value(field1, Mod:argless())),
	      ?assertEqual("test2", proplists:get_value(field2, Mod:argless())),
	      ?assertEqual("test3", proplists:get_value(field3, Mod:argless()))
      end}];

test("In base recmod, argumentless function should have SELF == THIS") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual(proplists:get_value(this, Mod:somefun(somearg)), proplists:get_value(self, Mod:somefun(somearg)))
      end}];


test("Function should have THIS assigned") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual(Mod, proplists:get_value(this, Mod:somefun(somearg)))
      end}];

test("Function should have all parameters assigned") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual("test1", proplists:get_value(field1, Mod:somefun(somearg))),
	      ?assertEqual("test2", proplists:get_value(field2, Mod:somefun(somearg))),
	      ?assertEqual("test3", proplists:get_value(field3, Mod:somefun(somearg)))
      end}];

test("Function should have all arguments assigned") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual(somearg, proplists:get_value(arg, Mod:somefun(somearg)))
      end}];

test("In base recmod, function should have SELF == THIS") ->
    [{f,
      fun () ->
	      Mod = #baserecmod{field1="test1",field2="test2",field3="test3"},
	      ?assertEqual(proplists:get_value(this, Mod:somefun(somearg)), proplists:get_value(self, Mod:somefun(somearg)))
      end}];

?EOT.
