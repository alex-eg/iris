{application, iris,
 [{description, "Scalable xmpp bot"},
  {vsn, "0.1.0"},
  {registered, [root, main_sup]},
  {modules, [iris, config]},
  %% Dependences
  {mod, {iris,[]}}
]}.
