# About

This repository hosts the source code to aptxshows.moe.
It is written with a Clojure backend and a ClojureScript
frontend. It is created mostly for my own personal interest
and learning, but I suppose you can contribute if anyone
is actually reading this.

## Installation

Consult `setup.sh`.

## Usage

To run as a web server, consult `run.sh`.
For debuging, I run `seasonal-chart.handler/start` in
one REPL and run `lein fig` to start a figwheel session
in another terminal.
You might notice that the namespace for the project is
uncreatively called `seasonal-chart`. I picked this name
when creating the Leiningen project and was too lazy later
to change it.
