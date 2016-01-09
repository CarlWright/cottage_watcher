## cottage_watcher (A Erlang app to monitor cottage environment and report as directed)

This erlang application works on the Raspberry PI using the "bmp085" and "erlang_ale" library to interface using I2C to the BMP085 device. The BMP085 device measures both air temperature and air pressure.

THe cottage_watcher checks air temperature and air pressure once a minute and records the measurements. After midnight each day, it prepares a report and line plot for each type of measurement and sends it to the destination you define.

It also checks the temperature every minute and will send a warning message the first time the termperature is outside a range you define. If the measurements persist, it sends URGENT messages detailing the temperature problem. If the problem goes away for a period you designate, it will forget any previous out-of-range measurements.

### Initialization

Before you can get air temperatures or pressures, you need to start a process to communicate with the BMP085 device. Call `cottage_watcher:start_link()` and get `{ok, <sensor pid>}`. Use the <sensor pid> value in all your calls to get temperatures and pressures.

### Getting measurements on command

`cottage_watcher:minute_measures(<sensor ID>)` returns a list of 60 tuples. The tuples are the date-time, temperature (Fahrenheit) and the air pressure (Pa).


### Environment

All the development and testing of this software was done on a Raspberry PI model B using Erlang version 18. The sensor was an Adafruit Bosch BMP180 pressure sensor. The software environment for this implementation requires "mutt" to send email messages with attachments and the R language so that we can run an R script to create the line plot attached to the emails we send.

### Examples

An example of the results for a daily temperature report

![Example temperature chart](examples/temperature-plot-2016-01-03.png)


An example of the results for a daily temperature data file

![Example temperature chart](examples/temps.txt)

An example of the results for a daily pressure report

![Example temperature chart](examples/pressure-plot-2016-01-03.png)