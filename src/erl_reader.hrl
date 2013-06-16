-record(er_feed,
		{uri,		
         lastUpdated,
         nextCheck,
		 entries=gb_sets:new()}).

-record(er_entry,
		{title,
		 date,
		 link,
		 content
		 }).
