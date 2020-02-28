# stm-incremental

This library is a simple interface to incremental computation using Haskell's
software transactional memory system. Here is a snippet of the sort of thing
you might do with this model, using an imaginary server library:

```haskell
server = do
  model <- inc massiveServerState
  modelA <- imap convenienceA model
  modelB <- imap convenienceB model
  endpoints [ endpointA modelA, endpointB modelB ] 
```

Here, we are replicating a very common pattern in Haskell, modifying a global
state for local consumption. The difference is, typically you would just read
the global state and pass it through `convenienceA` and `convenienceB`, but
here these are only ever updated when the global state is changed, and they
can otherwise be freely read.

Okay, well that's not so amazing, as we can just include the models for `A`
and `B` in the global model and this will get around the contention involved
in reading different `TVar`s. Well, what if we took a different tact? What if
we defined our entire application context as a number of `Incremental` values
and then merged them together? Well, we can! That's the purpose of `iliftA2`:

```haskell
server = do
  bit1 <- inc smallBit1
  forkIO $ forever $ threadDelay 10000 >> (pollBit1 >>= imodify bit1)
  bit2 <- inc smallBit2
  forkIO $ forever $ threadDelay 10000 >> (pollBit2 >>= imodify bit2)
  bit3 <- inc smallBit3
  forkIO $ forever $ threadDelay 10000 >> (pollBit3 >>= imodify bit3)
  modelA <- iliftA2 combine1And2 bit1 bit2
  modelB <- iliftA2 combine2And3 bit2 bit3
  endpoints [ endpointA modelA, endpointB modelB ]
```

Above, we show some code that polls various bits of the application state,
updating the in-memory representation, and the models are now constructed
via `iliftA2`, and will only be updated when an `Incremental` they were built
from is updated.
