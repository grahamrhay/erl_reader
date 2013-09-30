-record(er_feed,
		{id,
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
