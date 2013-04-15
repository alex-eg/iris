[
 {plugins, [
	    external_interface
	   ]
 },
 
 {denshi_jisho, [
		 {request_url, "http://jisho.org/words?jap=~s&eng=~s&dict=edict"}
		]},
 
 {twitter_api, [
		{consumer_key, "your-consumer-key"},
		{consumer_secret, "your-consumer-secret"},
		{access_token, "your-access-token"},
		{access_token_secret, "your-access-token-secret"}
	       ]},

 {google_search, [
		  {api_key, "your-api-key"},
		  {engine_id, "your-custom-search-engine-id"}
		 ]},
 
 {jid_config, [
	       {jid, "somejid@server.do"},
	       {resource, "any_resource"},
	       {status, "status text here~"},
	       {password, "very_secret_password"},
	       {rooms,[{"room1@conference.server.do", "Room Nick"},
		       {"room2@conference.server.do", "Nick", "room_password"}]},
	       {modules, [last, exit]},
	      ]},
 
 {jid_config, [
	       {jid, "another_jid@server.do"},
	       {resource, "another_resource"},
	       {status, "status~"},
	       {password, "very_very_secret_password"},
	       {rooms,[{"room1@conference.server.do", "Noom Rick"},
		       {"room2@conference.server.do", "Kick", "room_password"}]},
	       {modules, [last, exit]}
	      ]
 }
].

