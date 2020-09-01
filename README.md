# dang, an acme-pcf in Clojure

[PCF](http://homepages.inf.ed.ac.uk/gdp/publications/LCF.pdf) is simply typed lambda calculus + booleans + numbers + a few builtins + `fix`.

[`acme-pcf`](https://github.com/seagreen/acme-pcf-specification) (courtesy of my boy [@seagreen](https://github.com/seagreen)) is PCF + let + if/then/else + a real syntax, intended to be the [RealWorld](https://github.com/gothinkster/realworld) of language interpreters.

As of writihg, my crack at it here `cloc`s in at 340 lines of Clojure+[instaparse](https://github.com/Engelberg/instaparse)+[meander](https://github.com/noprompt/meander).

Going forward, I want to use this repo as a playground for different interpretation and compilation techniques.

See [notes.md](/notes.md) for more implementational geekery.

## Run and Test

To facilitate testing against [`acme-pcf`'s provided test harness](https://github.com/seagreen/acme-pcf-specification#testing-your-implementation), there are three steps involved.

In breif:

```bash
# in acme-pcf-specification/
stack install

# then, in dang/
clj -O:dang-socket
acme-pcf-test run.sh
```

In more detail:

1. Clone the original `acme-pcf` repo and `stack install` to get the `acme-pcf-test` executable.
1. Start up the socket server configured by the `:dang-server` alias in `deps.edn`. This server just listens for TCP connections on port `5575` and on connect reads one line of PCF from standard in, prints a result or error, and closes the connection.
1. Run the tests with the `run.sh` wrapper script. `run.sh` implements the interface expected by `acme-pcf-test` by calling up the socket server and parsing its answer. Convoluted and circuitous perhaps, but better than starting up and stopping the JVM in a for loop!
