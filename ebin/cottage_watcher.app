{application, cottage_watcher,
 [{description, "Application to monitor the environment of a cottage and 
                report as directed"},
  {vsn, "1.0"},
  {modules, ['cottage_watcher','cottage_watcher_app','cottage_watcher_sup']},
  {registered,[cottage_watcher_sup]},
  {applications, [kernel,
		  stdlib,
		  sasl,
		  erlang_ale,
		  bmp085]},
  {mod, {cottage_watcher_app, []}},
  {start_phases , []}]}.
