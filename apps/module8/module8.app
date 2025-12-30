{application, module8,
  [
    {description, "Module 8 – Delivery"},
    {vsn, "0.1.0"},
    {modules, ['RunRelease', 'RunSupervisor', 'RunWorker']},
    {registered, ['RunRelease']},
    {applications, [kernel, stdlib]},
    {mod, {'RunRelease', []}}
  ]}.
