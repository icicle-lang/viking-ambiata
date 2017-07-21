viking
======

```
The Vikings were daring masters of the sea. The shallow draught of their
swift wooden longships meant that they were able to reach far inland by
river and stream, striking and moving on before local forces could
muster.
```

<img src="https://github.com/ambiata/viking/raw/master/img/viking.jpg" width="480" align="right"/>

## Overview

Viking is an opinionated wrapper around the
[michaelt](https://github.com/michaelt)'s
[streaming](http://hackage.haskell.org/package/streaming) and
[streaming-bytestring](http://hackage.haskell.org/package/streaming-bytestring)
libraries. The motivation is twofold, to rename some problematic data
types and modules so that usage is a bit more seamless, and to provide
additional functionality on top in an `x-` like fashion. In particular,
functions which throw exceptions are replaced with ones which expose
errors in `EitherT`.

## Alternatives

Viking, and indeed the `streaming` library, differs from libraries like
`conduit` and `pipes` in that it models streams as lists rather than
list transformers.

Using the analogy of pure lists, you can think of a `Conduit a m b` as
a function from `[a] -> [b]`, a `Source m a` can be thought of as
a `[a]` and a `Sink m b` can be thought of as a `[b] -> ()`. In Viking
on the other hand, a `Stream (Of a) m ()` is equivalent to a `[a]`, and
everything else is just functions. The implication of this is that
writing streaming operations with Viking feels a lot like programming
with lists.

For example, if you wanted to write a function which adds one to an
integer stream and then turns it into a string:

#### Data.List

```hs
transform :: [Int] -> [String]
transform =
  fmap show . fmap (+ 1)
```

#### Viking

```hs
transform :: Monad m => Stream (Of Int) m r -> Stream (Of Int) m r
transform =
  Stream.map show . Stream.map (+ 1)
```

#### Conduit

```hs
transform :: Monad m => Conduit Int m String
transform =
  Conduit.map (+ 1) =$= Conduit.map show
```

Note from the example above, that conduits compose the wrong way. This
makes conduit composition hard to follow when it is used in combination
with function composition, because one needs to follow data flow in two
different directions.

For example, if we replace the example above with a single `Conduit.map`
then we'd have to flip the pipeline order, shortly followed by flipping
the nearest table:

```hs
transform :: Monad m => Conduit Int m String
transform =
  Conduit.map (show . (+ 1))
```
