[![Travis][travis badge]][travis]
[![Erlang/OTP Versions][erlang version badge]][erlang]
[![Build Tool][build tool]][reabr3]

mutex_problem
=====

An **experimental** OTP application to simulate ordering events in a distributed system.
This system are created by *Leslie Lamport*. You could read [*Time, Clocks and the Ordering of Events in a Distributed System*](http://lamport.azurewebsites.net/pubs/pubs.html#time-clocks) to get more detail.

Build
-----

    $ rebar3 compile
    
Test
-----

    $ rebar3 eunit

<!-- Badges -->
[travis]: https://travis-ci.org/Lustres/mutex_problem
[travis badge]: https://travis-ci.org/Lustres/mutex_problem.svg?branch=master
[erlang]: https:www.erlang.org
[reabr3]: https://www.rebar3.org
[erlang version badge]: https://img.shields.io/badge/erlang%2Fotp-18.3--20.0-990033.svg?style=flat
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-blue.svg?style=flat


