{application, cottage_alarm,
 [{description, "Application to process data about the environment of a cottage and 
                send alarms as directed"},
  {vsn, "1.0"},
  {modules, ['cottage_alarm','cottage_alarm_app','cottage_alarm_sup']},
  {registered,[cottage_alarm_sup]},
  {applications, [kernel,
		  stdlib,
		  sasl,
		  erlang_ale,
		  bmp085,
		  cottage_watcher]},
  {mod, {cottage_alarm_app, []}},
  {start_phases , []}]}.
