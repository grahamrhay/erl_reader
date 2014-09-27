-record(er_feed,
        {id,
         feed,
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
