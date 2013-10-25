# Prometheus

XMPP-powered heater unit on a BeagleBone Black.

Put [Debian](http://elinux.org/BeagleBoardDebian#eMMC:_BeagleBone_Black) on it.

    $ aptitude install build-essential libxml2-dev libexpat1-dev erlang-nox git zile tmux tree unzip zip curl

## Setup

```
$ echo 33 > /sys/class/gpio/export
$ echo out > /sys/devices/virtual/gpio/gpio33/direction
```

```erlang
P1 = prometheus:start("bot@hagelb.org", "password", "xmpp1.hosted.im",
                      "/sys/devices/ocp.2/helper.14/AIN0",
                      "/sys/devices/virtual/gpio/gpio33/value").
```

## License

Copyright © 2013 Phil Hagelberg. Licensed under the Erlang
Public License, version 1.1 or later. See COPYING for details.

