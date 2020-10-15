# stm-incremental

[![Hackage](https://img.shields.io/hackage/v/stm-incremental.svg)](https://hackage.haskell.org/package/stm-incremental)

This library is meant to expose an interface for incremental computation
using software transactional memory in Haskell.

```haskell
import Control.Concurrent.STM.Incremental

main = do
  (salutation, name, greeting) <- atomically do
    salutation <- incremental "Hello"
    name <- incremental "Samuel"
    greeting <- combine (\s n -> s <> ", " n) salutation name
    pure (salutation, name, greeting)
  -- Will print "Hello, Samuel"
  atomically (observe greeting) >>= print
  atomically (set salutation "Hiya")
  -- Will print "Hiya, Samuel"
  atomically (observe greeting) >>= print
```

There are three operations, `map`, `combine`, and `choose`. They sort of
correspond to the operations of `fmap`, `liftA2`, and `(>>=)`, but not exactly,
and it isn't really worth belaboring that point if it isn't clear. `map` allows
you to construct an incremental computation depending on one other, which only
ever gets updated when this single dependency does. `combine` allows you to
construct an incremental computation depending on two others, which gets updated
whenever _either_ does. `choose` allows you to switch your dependency structure
depending on live values in the incremental computation. In other words, this
allows you to have dynamic dependencies, whereas the former two functions only
allowed you to have static dependencies.

The `choose` combinator is the most computationally expensive, requiring in the
worst case time proportional to the size of the image of the choice function
passed in. That being said, it can be made logarithmic in the size of the image
in a not-so-difficult way, but it must be noted that it must retain space
proportional to the entire image in the heap in the worst case. In general,
I would argue that this does not typically come up in the use case that I am
interested in. The use case for this library is when you have a data structure
you want to maintain based on some input data, but it is expensive to compute
in full, and you will not be changing it extremely frequently. If you can factor
your dependencies carefully, perhaps changing one piece of your computation will
not require substantial work to incrementally compile the rest.
