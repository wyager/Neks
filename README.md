KVStore
=======

##### A dead simple key/value store

KVStore is an in-memory key/value store written in 175 lines of Haskell. (150 lines without empty lines!)

It's highly concurrent and pretty fast.

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

##### Protocol:

All network encoding is done using [msgpack](http://msgpack.org).

Messages are preceded by the length of the message, transmitted as a 
64-bit big-endian unsigned integer.

Messages are arrays of requests or responses.

- Requests to Set key `K` to value `V` are sent as `[1, K, V]` 
(where `K` and `V` are binary arrays). 
- Requests to Get key `K` are sent as `[0, K]`. The server sends a response.
- If the value `V` of requested key `K` is found, the response is `[-1, K, V]`.
- If the value of requested key `K` is not found, the response is `[-2, K]`.

Example conversation:

    Client: <64-bit big-endian length>[[1,"status","OK"],[0,"Jim"],[0,"Dwight"]]
    Server: <64-bit big-endian length>[[-1,"Jim","Halpert"],[-1,"Dwight","Schrute"]]

The key is sent back with the response to facilitate easier asynchronous
programming on the client end.
