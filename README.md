Neks
=======

##### A dead simple key/value server

Neks is an in-memory networked key/value server written in ~200 lines of Haskell.

It is intended to be very easy to modify.

##### Features

- Very simple
- Pretty fast (>1M transactions per second is easy, see [Benchmark](#benchmark))
- Highly concurrent 
- Atomic transactions (e.g. atomic read-and-swap)
- Optional disk persistence (with atomic snapshotting)

##### To install using Cabal:

    cabal install neks
    Server <opt-args>
    Client <args>

##### To build and run from source (recommended):

    cabal sandbox init
    cabal install --only-dependencies
    cabal run Server
    cabal run Client -- <args>

or, with dependencies installed:

    ghc -O2 -threaded Network/Neks/Server.hs
    ghc -O2 -threaded Network/Neks/Client.hs
    ./Network/Neks/Server +RTS -N<number of cores>
    ./Network/Neks/Client <args>

To run the Python client:

    python3 Client.py

#### To view instructions:

    Server --help
    Client --help

##### Benchmark:

The server and client run on two cores. The client runs

- 50 threads
- 200 requests per thread
- 50 reads and 50 writes per request

Speed depends on latency and bandwidth. Here's what I get on my home machines:

| Avg. Latency | Transactions / Second |
---------------|------------------------
| .1ms (localhost)  | 1,150,000 |
| .3ms (ethernet) | 1,100,000 |
| 3.5ms (wireless) | 750,000 |

##### Protocol:

All network encoding is done using [msgpack](http://msgpack.org).

Messages are preceded by the length of the message, transmitted as a 
64-bit big-endian unsigned integer.

The client sends requests to the server, and the server responds with the results of the requests.

There are 4 kinds of requests:

- Requests to Get key `K`. These are formatted as `[0, K]`. The server sends a response.
- Requests to Set key `K` to value `V`. These are formatted as `[1, K, V]`. The server does not send a response.
- Requests to Delete key `K`. These are formatted as `[2, K]`. The server does not send a response.
- Requests to Atomically evaluate a list of requests `R`. This is formatted as `[3, R]`. The server sends the response as if all requests in `R` had been evaluated normally.

There are 2 kinds of responses:

- Response that the value `V` for requested key `K` was found. This is formatted as `[-1, V]`.
- Response that the value for requested key `K` was not found. This is formatted as `[-2]`.

Example conversation:

    Client: <message length>[[1,"status","OK"],[0,"Jim"],[0,"Dwight"]]
    Server: <message length>[[-1,"Halpert"],[-1,"Schrute"]]
