{jid_config, [
	      {jid, "somejid@server.do"},
	      {resource, "any_resource"},
	      {status, "status text here~"},
	      {password, "very_secret_password"},
	      {rooms,[{"room1@conference.server.do", "Room Nick"},
		      {"room2@conference.server.do", "Nick", "room_password"}],
	      {modules, [last, exit]},
]}.
