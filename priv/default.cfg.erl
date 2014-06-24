[
 {shrinker, [
             {request_url, "http://xn--jj0a.jp/"},
             {params, "fUrl=~s&id=~s"}
            ]},

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

 {jids, [{"somejid@server.do", 
          [{resource, "any_resource"},
           {status, "status text here~"},
           {password, "very_secret_password"},
           {rooms,[{"room1@conference.server.do", 
                    [{nick, "Room Nick"},
                     {commands, [last, exit]},
                     {logging, on},
                     {banlist, [{nick, "romeo"},
                                {jid, "juliet@example.net"}]}
                     ]},
                   {"room2@conference.server.do",
                    [{nick, "Nick"},
                     {password, "secretroompassword"},
                     {commands, []},
                     {logging, off},
                     {banlist, []}]}
                  ]},
           {commands, [last, exit]},
           {plugins, [chat_command]}
          ]},
         {"another_jid@server.do",
          [{resource, "resource"},
           {status, "status~"},
           {password, "very_very_secret_password"},
           {rooms,[{"room1@conference.server.do", [{nick, "Noom Rick"}]},
                   {"room2@conference.server.do", [{nick, "Kick"},
                                                   {password, "room_password"}]}
                  ]},
           {commands, [last, exit]}
          ]}]}
].
