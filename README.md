[![Build Status](https://travis-ci.org/ppelleti/hs-powermate.svg?branch=master)](https://travis-ci.org/ppelleti/powermate)

This library is for interfacing the [Griffin PowerMate USB][1] with
Haskell on Linux.  Besides reading events from the PowerMate, you
can also control the brightness, pulse speed, and pulse waveform of
the built-in LED.

(The library also contains an implementation of the [MPD][2]
protocol.)

Besides the library, this package includes two example programs,
`powermate-print` and `powermate-pulse`.  `powermate-print` simply
prints out the events it receives from the PowerMate.
`powermate-pulse` lets you control the LED pulse speed by
turning the knob, and change the pulse waveform by clicking the knob.

[1]: https://griffintechnology.com/us/powermate
[2]: https://musicpd.org/doc/protocol/
