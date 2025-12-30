{application, chat_app,
  [
    {description, "Module 12 Distributed Chat System"},
    {vsn, "0.1.0"},
    {applications, [
      kernel,
      stdlib,
      sasl,
      crypto,
      mnesia
    ]},
    {mod, {chat_app, []}}
  ]}.