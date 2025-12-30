{application, module11_1,
  [
    {description, "Module 11_1 - REST API with Cowboy and ETS"},
    {vsn, "0.1.0"},

    {registered, []},

    {mod, {module11_1_app, []}},

    {applications,
      [
        kernel,
        stdlib,
        ranch,   %% REQUIRED for Cowboy listeners
        cowboy,
        jsx
      ]},

    {env, []}
  ]}.