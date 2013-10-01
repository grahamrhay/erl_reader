-record(er_feed,
		{id,
         feed,
         uri,		
         lastUpdated,
         nextCheck,
		 entries=gb_sets:new(),
         users=[]}).

-record(er_entry,
		{title,
		 date,
		 link,
		 content
		 }).
