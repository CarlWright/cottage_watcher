{application, cottage_beacon,
 [{description, "Application to process data about the environment of a cottage and 
                send beacons as directed"},
  {vsn, "1.0"},
  {modules, ['cottage_beacon','cottage_beacon_app','cottage_beacon_sup']},
  {registered,[cottage_beacon_sup]},
  {applications, [kernel,
		  stdlib,
		  sasl,
		  inets,
		  erlang_ale,
		  bmp085]},
  {mod, {cottage_beacon_app, []}},
  {start_phases , []}]}.
