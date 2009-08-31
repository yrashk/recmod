-record(emptyrecmod,
	{data}).

-record(baserecmod,
	{field1,
	 field2,
	 field3 = "default"
	 }).
-record(extrecmod,
	{field1,
	 field2 = "default",
	 otherfield
	 }).
