{application, module7,
  [
    {description, "Module 7 – Supervisor"},
    {vsn, "0.1.0"},
    {modules, ['RunSupervisor7App','RunSupervisor7','WorkerGenServer']},
    {registered, ['RunSupervisor7','WorkerGenServer']},
    {applications, [kernel, stdlib]},
    {mod, {'RunSupervisor7App', []}},
    {type, permanent}
  ]}.
