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

The
[pin numbering](http://stuffwemade.net/hwio/beaglebone-pin-reference/)
on the BeagleBone is very confusing as there a number of different
names for each pin. This project only requires the P9 header, which is
the one reaching from next to the full-sized USB port to the DC power
jack. Most of what you need is at end near the USB port, but you'll
need to draw from the 3.3V pin at the other end, (either pin second
from the DC jack end of P9 will do) for the temperature sensor.

The
[data sheet for the relay](http://www.fotek.com.hk/solid/SSR-1.htm)
claims that it only draws 7.5ma at 12V; however in my observation it
draws 9ma at 3.3V, which is well over what the BeagleBone can supply
from a single gpio pin. Because of this it's necessary to use the gpio
to toggle a signal from the Beaglebone's SYS 5V pin using another
relay or a transistor. (not shown on the pinout diagram) Theoretically
you could use either VDD voltage instead, but this didn't work for me,
and the SYS pin provides up to 250ma, which is plenty for our
purposes.

## Software Setup

Put [Debian](http://elinux.org/BeagleBoardDebian#eMMC:_BeagleBone_Black) on it, obviously.

    $ aptitude install build-essential erlang-nox erlang-dev libxml2-dev libexpat1-dev libssl-dev git tmux

Do a manual install of [rebar](https://github.com/rebar/rebar) and
pull in dependencies:

```
$ curl -L https://github.com/rebar/rebar/wiki/rebar > /usr/local/bin/rebar
$ chmod 755 /usr/local/bin/rebar
$ cd /path/to/prometheus
$ rebar get-deps && rebar compile
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
prometheus:start("bot@hagelb.org", Password, "xmpp1.hosted.im",
                 "/sys/devices/ocp.3/helper.15/AIN5",
                 "/sys/devices/virtual/gpio/gpio7/value").
```

You may have to poke around to find the `AIN5` pin; from what I've
observed the `ocp.N` and `helper.N` directories jumping around
following no predictable logic unfortunately.

Or if testing on a machine without GPIO:

```erlang
prometheus:start("bot@hagelb.org", Password, "xmpp1.hosted.im",
                 "/tmp/sensor", "/tmp/relay").
```

You'll need an XMPP account for the bot to connect to, obviously, and
one for yourself. Log in with another client to add your personal
account as a contact before running the above.

## Usage

Set the temperature by sending a `temp 650` message to the bot's XMPP
account. Temperature is currently represented as voltage read directly
from the ADC; no conversion to celsius is done yet. Read the
temperature with just `temp`. Sending `stop` will shut down the erlang
processes but not the erlang shell.

## License

Copyright © 2013 Phil Hagelberg. Licensed under the Erlang
Public License, version 1.1 or later. See COPYING for details.

