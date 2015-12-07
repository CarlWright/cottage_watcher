# cottage_watcher (A Erlang app to monitor cottage environment and report as directed)

This erlang application works on the Raspberry PI using the "bmp085" and "erlang_ale" library to interface using I2C to the BMP085 device.

## Initialization

Before you can get air temperatures or pressures, you need to start a process to communicate with the BMP085 device. Call `cottage_watcher:start_link()` and get `{ok, <sensor pid>}`. Use the <sensor pid> value in all your calls to get temperatures and pressures.

### Getting measurements

`cottage_watcher:minute_measures(<sensor ID>)` returns a list of 60 tuples. The tuples are the date-time, temperature (Fahrenheit) and the air pressure (Pa).


### Environment

All the development and testing of this software was done on a Raspberry PI model B using Erlang version 18. The sensor was an Adafruit Bosch BMP180 pressure sensor. 