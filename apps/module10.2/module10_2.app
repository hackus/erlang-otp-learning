{application, module10_2,
  [
    {description, "Module 10.2 - ETS GenServer Wrappers"},
    {vsn, "0.1.0"},
    {modules, [
      module10_2_app,
      module10_2_sup,
      ets_safe_store,
      ets_fast_store,
      ets_counter_store,
      ets_advanced_counter_store
    ]},
    {registered, [
      module10_2_sup,
      ets_safe_store,
      ets_fast_store,
      ets_counter_store,
      ets_advanced_counter_store
    ]},
    {applications, [
      kernel,
      stdlib
    ]},
    {mod, {module10_2_app, []}},
    {env, [
      {ets_table_type, set},
      {ets_named_table, true},
      {read_concurrency, true},
      {write_concurrency, true},
      {cleanup_interval_ms, 60000},
      {default_ttl_ms, infinity}
    ]}
  ]}.
