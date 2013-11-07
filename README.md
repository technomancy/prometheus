# Prometheus

XMPP-powered heater unit on a BeagleBone Black.

## Hardware Setup

In addition to a
[BeagleBone Black](http://beagleboard.org/Products/BeagleBone%20Black),
you'll need a
[temperature sensor](https://www.adafruit.com/products/165) and
[solid-state relay](http://www.fotek.com.hk/solid/SSR-1.htm). Be sure
the relay you get is rated for over the amount of current your heater
takes; in my case I needed the SSR-25DA since my heater draws
13A. You'll want three different colors of solid-core wire; the length
will depend on how far you plan on positioning the board from the
heater and sensor. If you don't want to use solid-core wire you can
solder your wire to a strip of male header.

<img src="https://github.com/technomancy/prometheus/raw/master/pinout.jpg" align="right" />

The pin numbering on the BeagleBone is very confusing as there a
number of different names for each pin. This project only requires the
P9 header, which is the one reaching from next to the full-sized USB
port to the DC power jack. Most of what you need is at end near the
USB port, but you'll need to draw from the 3.3V pin at the other end,
(either pin second from the DC jack end of P9 will do) for the
temperature sensor.

## Software Setup

Put [Debian](http://elinux.org/BeagleBoardDebian#eMMC:_BeagleBone_Black) on it, obviously.

    $ aptitude install build-essential erlang-nox erlang-dev libxml2-dev libexpat1-dev libssl-dev git tmux

Do a manual install of [rebar](https://github.com/rebar/rebar) and
pull in dependencies:

```
$ curl -L https://github.com/rebar/rebar/wiki/rebar > /usr/local/bin/rebar
$ chmod 755 /usr/local/bin/rebar
$ cd /path/to/prometheus
$ rebar get-deps
```

Set up the GPIO pins; both the digital out for the relay and the
analog in for the temperature sensor. This will need to be done once
per boot.

```
$ echo 7 > /sys/class/gpio/export
$ echo out > /sys/devices/virtual/gpio/gpio7/direction
$ echo cape-bone-iio > /sys/devices/bone_capemgr.*/slots
```

Run `erl -pa deps/exmpp/ebin` from your checkout.

```erlang
c("src/prometheus.erl").
c("src/prometheus_regulator.erl").
P1 = prometheus:start("bot@hagelb.org", Password, "xmpp1.hosted.im",
                      "/sys/devices/ocp.3/helper.15/AIN5",
                      "/sys/devices/virtual/gpio/gpio7/value").
```

Or if testing on a machine without GPIO:

```erlang
P1 = prometheus:start("bot@hagelb.org", Password, "xmpp1.hosted.im",
                      "/tmp/sensor", "/tmp/relay").
```

You'll need an XMPP account to connect to, obviously. Log in with
another client to add your regular account as a contact before running
the above.

## License

Copyright © 2013 Phil Hagelberg. Licensed under the Erlang
Public License, version 1.1 or later. See COPYING for details.

