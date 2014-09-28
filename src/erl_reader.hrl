-record(er_feed,
        {feed,
         uri,
         lastUpdated,
         nextCheck,
         entries=[],
         users=[]}).

-record(er_entry,
        {title,
         date,
         link,
         content
        }).
