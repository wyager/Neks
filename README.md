KVStore
=======

##### A dead simple key/value server

KVStore is an in-memory key/value server written in ~200 lines of Haskell.

##### Features

- Very simple
- Pretty darn fast (>1M transactions per second is easy, see [Benchmark](#benchmark))
- Highly concurrent 
- Atomic transactions (e.g. atomic read-and-swap)
- Optional disk persistence (with atomic snapshotting)

##### To build and run:

    cabal sandbox init
    cabal install --only-dependencies
    cabal run KVServer
    cabal run KVClient

or, with dependencies installed:

    ghc -O2 -threaded Client.hs
    ghc -O2 -threaded Server.hs
    ./Server +RTS -N<number of cores>
    ./Client +RTS -N<number of cores>

To run the Python client:

    python3 Client.py

##### Benchmark:

Running the server with 2 cores and a client with

- 100 threads
- 500 requests per thread
- 50 reads and 50 writes per request

takes ~4.3 seconds on my laptop. That's `(100*500*50*2)/4.3 = 1,162,790` transactions per second.

##### Protocol:

All network encoding is done using [msgpack](http://msgpack.org).

Messages are preceded by the length of the message, transmitted as a 
64-bit big-endian unsigned integer.

The client sends requests to the server, and the server responds with the results of the requests.

There are 4 kinds of requests:

- Requests to Get key `K`. These are formatted as `[0, K]`. The server sends a response.
- Requests to Set key `K` to value `V`. These are formatted as `[1, K, V]`. The server does not send a response.
- Requests to Delete key `K`. These are formatted as `[2, K]`. The server does not send a response.
- Requests to Atomically evaluate a list of requests `R`. This formatted as `[3, R]`. The server sends the response as if all requests in `R` had been evaluated normally.

There are 2 kinds of responses:

- Response that the value `V` for requested key `K` was found. This is formatted as `[-1, V]`.
- Response that the value for requested key `K` was not found. This is formatted as `[-2]`.

Example conversation:

    Client: <message length>[[1,"status","OK"],[0,"Jim"],[0,"Dwight"]]
    Server: <message length>[[-1,"Halpert"],[-1,"Schrute"]]
