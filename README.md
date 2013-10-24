# Prometheus

XMPP-powered heater unit.

Put [Debian](http://elinux.org/BeagleBoardDebian#eMMC:_BeagleBone_Black) on it.

    $ aptitude install build-essential libxml2-dev libexpat1-dev erlang-nox git zile tmux tree unzip zip curl

Handy during development:

```lisp
(setq inferior-erlang-machine-options
      '("-pa" "/home/phil/src/prometheus/deps/exmpp/ebin"))
```

## License

Copyright © 2013 Phil Hagelberg. Licensed under the Erlang
Public License, version 1.1 or later. See COPYING for details.

