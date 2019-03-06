---
title: 'Implementing Caches in Haskell'
author: 'Jasper Van der Jeugt'
date: 2019-04-06
patat:
  incrementalLists: true
  slideLevel: 2
  wrap: true
  images:
    backend: w3m
  theme:
    codeBlock: [vividBlack, onRgb#eeeeee]
    code: [vividBlack, onRgb#eeeeee]
...

Introduction
============

About me
--------

<!--
Minimum columns required: around 82
Minimum lines required: around 25
-->

Hi, I'm Jasper

- Got started with Haskell a while ago
- I use Haskell at work (Fugue)
- I use Haskell in my free time
- I try to be involved in the community (Haskell.Org, Zurihac...)

@jaspervdj -- jaspervdj.be

About this talk
---------------

Asynchronous exceptions are unmasked: interrupt me at any time if something does
not make sense to you, you think something is wrong...

About this talk
---------------

This is about building caches into your applications.

. . .

Not about L1, L2, etc.

. . .

For example, we have some code at work that prevents us from duplicating API
calls to AWS when describing resources

. . .

According to my scientific™ estimates™, every application contains at least one
of these for every 1000 lines of code

About this talk
---------------

Why do so many people implement their own caches?

 -  Highly application-specific
 -  Needs to integrate with custom logging, metrics...
 -  Hacks like: don't update last access time if it was less than 10 seconds ago
 -  We like to reinvent the wheel

. . .

`psqueues` is a toolbox library, so you can still have the **FUN** of
reinventing the wheel, but get it fast and correct and the same time.

About this talk
---------------

![](images/ekg.png)

About this talk
---------------

<!--
Who knows what PATRICIA stands for?
-->

It's mostly about `psqueues`: finite map + priorities associated with elements

 -  Uses a data structure popularized in FP by Ralf Hinze:

    _A Simple Implementation Technique for Priority Search Queues (ICFP 2001)_

 -  Another data structure that uses PATRICIA trees *and* has the min-heap
    property

    . . .

    _**P**ractical **A**lgorithm **T**o **R**etrieve **I**nformation **C**oded
    **I**n **A**lphanumeric_

    . . .

    Donald R. Morrison and Gernot Gwehenberger (independently)

. . .

Haskell package by Simon Meier, myself, various ZuriHac 2014 attendees

About this talk
---------------

Teaser first!  And Literate Haskell, like always...

<!--
I think this is the first talk I've given where I only needed one language
extension...
-->

> {-# LANGUAGE BangPatterns #-}
> import           Data.Bifunctor (first)
> import           Data.Int       (Int64)
> import           Data.IORef     (IORef)
> import qualified Data.IORef     as IORef
> import           Data.Hashable  (Hashable (..))
> import           Data.List      (foldl')
> import           Data.Map       (Map)
> import qualified Data.Map       as Map
> import           Data.Maybe     (fromMaybe, isNothing)
> import           Data.HashPSQ   (HashPSQ)
> import qualified Data.HashPSQ   as HashPSQ
> import           Data.OrdPSQ    (OrdPSQ)
> import qualified Data.OrdPSQ    as PSQ
> import qualified Data.Vector    as V

Teaser problem
--------------

![](images/pathfinding.png)

Teaser problem
--------------

Sample Dijkstra implementations usually suffer from a bunch of problems:

 -  They don't work on infinite graphs (need to be able to iterate all the
    vertices initially)
 -  They don't do bookkeeping to remember the shortest path, you just get the
    distance
 -  They liberally use values like `maxBound`
 -  On my polymorphism scale™ from golang to polykinds, they are usually a 2

Teaser problem: such polymophism, wow, very type
------------------------------------------------

<!--
Can we implement dijkstra's algorithm in one slide?  Well, we can if we put
the types on a different slide...
-->

> discover
>     :: (Ord vertex, Ord distance)
>     => vertex -> distance -> path
>     -> OrdPSQ vertex distance path
>     -> OrdPSQ vertex distance path

> dijkstra
>     :: (Ord vertex, Ord distance, Monoid distance)
>     => (vertex -> [(distance, vertex)])
>     -> vertex
>     -> Map vertex (distance, [vertex])

Teaser problem: implementation
------------------------------

> discover vertex distance path = snd . PSQ.alter decrease vertex
>   where
>     decrease (Just (d, p)) | d <= distance = ((), Just (d, p))
>     decrease _                             = ((), Just (distance, path))

. . .

> dijkstra graph start = loop Map.empty (PSQ.singleton start mempty [])
>   where
>     loop distances queue0 = case PSQ.minView queue0 of
>         Nothing -> distances
>         Just (vertex, dist, path, queue1) -> loop
>             (Map.insert vertex (dist, path) distances)
>             (foldl'
>                 (\q (d, n) -> discover n (dist <> d) (vertex : path) q)
>                 queue1
>                 (filter ((`Map.notMember` distances) . snd) (graph vertex)))

From Dijkstra's algorithm to caches
-----------------------------------

The requirements for the data structure used in Dijkstra's algorithm resemble
those of caches.  Informally:

1.  Need to store some data under a key, insert/retrieve
2.  Pop the oldest/smallest/prettiest item (~ `p`)
3.  Adjust the `p` for some key

. . .

In a mutable world, you can mix a finite map and a priority queue to do this,
but we don't want to go mucking around with `IORef`s (for now).

. . .

Enter priority _search_ queues.

Tournament trees (OrdPSQ)
=========================

Tournament trees: core idea
---------------------------

Start from a searchable binary tree and turn it into a _tournament_.

Idea: knockout plays

<!--
Some of my favourite pokemon.  Farfetch'd was not included because I'm
using a combination of CPP, Dot and Makefiles...
-->

<!--
We do need a total ordering of all the priorities.
-->

Tournament trees: part 1
------------------------

![](images/tournament-01.dot.png)

Tournament trees: issues
------------------------

 -  The key and value of the winner appear on _every_ level
 -  In general, winners appear one additional time for every match they win
 -  Every pokemon can win N - 1 matches, where N is the number of levels
 -  But...
 -  Every pokemon (except the winner) loses exactly once!

Tournament trees: part 1
------------------------

![](images/tournament-01.dot.png)

Tournament trees: part 2
------------------------

![](images/tournament-02.dot.png)

Tournament trees: part 3
------------------------

![](images/tournament-03.dot.png)

Tournament trees: semi-heap
---------------------------

Rather than having a min-heap property, we have created something called a
semi-heap.  Properties:

  - The loser dominates the left or the right subtree (the one they originated
    from)
  - We have a valid binary search tree
  - There are no two nodes with the same key
  - Every node also the "split key", the max key of its subtree

Tournament trees: part 3
------------------------

![](images/tournament-03.dot.png)

Tournament trees: Haskell
-------------------------

<!--
Parameterized around k p v, since we want to be able to talk about more
than pokemon.
-->

```haskell
data Elem k p v = E !k !p !v

data OrdPSQ k p v
    = Void
    | Winner !(Elem k p v) !(LTree k p v) !k
```

Tournament trees: Haskell
-------------------------

```haskell
data LTree k p v
    = Start
    | LLoser {-# UNPACK #-} !Size
             {-# UNPACK #-} !(Elem k p v)
                            !(LTree k p v)
                            !k
                            !(LTree k p v)
    | RLoser {-# UNPACK #-} !Size
             {-# UNPACK #-} !(Elem k p v)
                            !(LTree k p v)
                            !k
                            !(LTree k p v)
```

Tournament trees: popping
-------------------------

```haskell
minView :: (Ord k, Ord p) -> Maybe (k, p, v, OrdPSQ k p v)
minView Void                    = Nothing
minView (Winner (E k p v) t m)) = Just (k, p, v, secondBest t m)
```

. . .

Since the winner is already "floating at the top", the challenge is really in
pulling out the second best participant, so the next `minView` will also be
fast.

Tournament trees: secondBest
----------------------------

Intuitively:

 -  The second best value could be at any depth in the tree!
 -  But they could only have lost against the champion
 -  So we just need to check the path from which the champion "originated"

. . .

```haskell
secondBest :: (Ord k, Ord p) => LTree k p v -> k -> OrdPSQ k p v
secondBest Start _                 = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'
```

Tournament trees: playing matches
---------------------------------

```haskell
play :: (Ord p, Ord k) => OrdPSQ k p v -> OrdPSQ k p v -> OrdPSQ k p v
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p v) t m `play` Winner e'@(E k' p' v') t' m'
    | (p, k) `beats` (p', k') = Winner e (rbalance k' p' v' t m t') m'
    | otherwise               = Winner e' (lbalance k p v t m t') m'
```

. . .

For this construct we've build, you are actually free to choose a balancing
scheme!

. . .

`psqueues` uses S. Adams' weight-balanced trees.

PATRICIA trees with min-heap (IntPSQ)
=====================================

Issues with tournament trees
----------------------------

 -  Is `Data.Map` the fastest we can get?  Assymptotically, more or less...
 -  But, there's `IntMap` and `HashMap`...

. . .

`IntPSQ` is to `OrdPSQ` as `Data.IntMap` is to `Data.Map`

IntPSQ: 0
---------

![](images/patricia-00.dot.png)

IntPSQ: definition
------------------

```haskell
type Key = Int
type Mask = Int

data IntPSQ p v
    = Bin {-# UNPACK #-} !Key
                         !p
                         !v
          {-# UNPACK #-} !Mask
                         !(IntPSQ p v)
                         !(IntPSQ p v)
    | Tip {-# UNPACK #-} !Key
                         !p
                         !v
    | Nil
    deriving (Foldable, Functor, Show, Traversable)
```

IntPSQ: 1
---------

![](images/patricia-01.dot.png)

IntPSQ: 2
---------

![](images/patricia-02.dot.png)

IntPSQ: 3
---------

![](images/patricia-03.dot.png)

IntPSQ: 4
---------

![](images/patricia-04.dot.png)

IntPSQ: 5
---------

![](images/patricia-05.dot.png)

IntPSQ: lookup
--------------

```haskell
lookup :: Int -> IntPSQ p v -> Maybe (p, v)
lookup k = go
  where
    go t = case t of
        Nil                -> Nothing

        Tip k' p' x'
          | k == k'        -> Just (p', x')
          | otherwise      -> Nothing

        Bin k' p' x' m l r
          | nomatch k k' m -> Nothing
          | k == k'        -> Just (p', x')  -- This is special
          | zero k m       -> go l
          | otherwise      -> go r
```

IntPSQ: minView
---------------

```haskell
minView :: Ord p => IntPSQ p v -> Maybe (Int, p, v, IntPSQ p v)
minView t = case t of
    Nil             -> Nothing
    Tip k p x       -> Just (k, p, x, Nil)
    Bin k p x m l r -> Just (k, p, x, merge m l r)
```

IntPSQ: properties
------------------

Do we list all the properties here in English?  Or...

. . .

```haskell
valid :: Ord p => IntPSQ p v -> Bool
valid psq =
    not (hasBadNils psq) &&
    not (hasDuplicateKeys psq) &&
    hasMinHeapProperty psq &&
    validMask psq
```

IntPSQ: testing sidetrack
-------------------------

How do we ensure these are checked?

. . .

Coming up with a naive `Arbitrary` instance is really hard!

. . .

```haskell
data Action k p v
    = Insert k p v
    | DeleteRandomMember
    | DeleteMin
    deriving (Show, Eq)
```

. . .

```haskell
arbitraryAction :: (Arbitrary k, Arbitrary v) => Gen (Action k Int v)
arbitraryAction = ...
```

IntPSQ: testing sidetrack
-------------------------

```haskell
apply
    :: PSQ psq
    => Action (Key psq) Int v -> psq Int v -> Gen (psq Int v)
apply (Insert k p x) t = return $ insert k p x t
apply _ = ...
```

. . .

```haskell
arbitraryPSQ
    :: forall psq v. (Arbitrary (Key psq), Arbitrary v, PSQ psq)
    => Gen (psq Int v)
arbitraryPSQ = do
    numActions <- choose (0, 100)
    actions    <- replicateM numActions arbitraryAction
    foldM (\t a -> apply a t) (empty :: psq Int v) actions
```

HashPSQ
=======

IntPSQ: the problem
-------------------

It fixes `k` to `Int`...

. . .

<!--
We can use the pokedex number but I guess not everyone knows these by heart...
-->

```haskell
newtype HashPSQ k p v = HashPSQ (IntPSQ.IntPSQ p (Bucket k p v))
    deriving (Foldable, Functor, NFData, Show, Traversable)
```

```haskell
data Bucket k p v = B !k !v !(OrdPSQ.OrdPSQ k p v)
    deriving (Foldable, Functor, Show, Traversable)
```

Questions about these datastructures?
=====================================

PureCache
=========

PureCache: logical time
-----------------------

We can fix the priority type but keep `k` and `v` polymorphic.  We're going to
use monotonically increasing "logical timestamps" rather than real timestamps to
keep things simple.

> type Priority = Int64

PureCache: data definition
--------------------------

> data PureCache k v = PureCache
>     { pcCapacity :: !Int
>     , pcSize     :: !Int
>     , pcTick     :: !Priority
>     , pcQueue    :: !(HashPSQ k Priority v)
>     } deriving (Eq, Show)

Invariant: all priorities in `cQueue` are smaller than `Priority`!

PureCache: empty
----------------

> empty :: Int -> PureCache k v
> empty capacity
>     | capacity < 1 = error "You must be the funniest kid in the school"
>     | otherwise    = PureCache
>         { pcCapacity = capacity
>         , pcSize     = 0
>         , pcTick     = 0
>         , pcQueue    = HashPSQ.empty
>         }

PureCache: trim
---------------

We can write a function that trims our queue, so we don't need to bother with
that in `lookup` and `insert`.

> trim :: (Hashable k, Ord k) => PureCache k v -> PureCache k v
> trim pc
>     | pcTick pc == maxBound =

. . .

Yep, we're going there!

PureCache: maxBound reached
---------------------------

<!--
Often there's something that seems daunting at first, and you feel like you
would struggle implementing it, but it turns out to be easier than you thought.
That's the power of Haskell!
-->

>         let minPrio = case HashPSQ.minView (pcQueue pc) of
>                 Nothing                  -> 0
>                 Just (_k, p, _v, _queue) -> p
>             maxPrio = HashPSQ.fold'
>                 (\_k p _v acc -> max p acc) 0 (pcQueue pc) in

. . .

>         trim $! pc
>             { pcTick  = maxPrio + 1
>             , pcQueue = HashPSQ.unsafeMapMonotonic
>                 (\_k p v -> (p - minPrio, v)) (pcQueue pc)
>             }

. . .

That actually wasn't too hard...

PureCache: trim
---------------

>     | pcSize pc > pcCapacity pc = trim $! pc
>         { pcSize  = pcSize pc - 1
>         , pcQueue = HashPSQ.deleteMin (pcQueue pc)
>         }
>
>     | otherwise = pc

PureCache: insert
-----------------

`insertView` from `HashPSQ` gives us a "view" of how the insert happened.

```haskell
insertView
    :: (Hashable k, Ord p, Ord k)
    => k -> p -> v -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
```

PureCache: insert
-----------------

`insertView` from `HashPSQ` gives us a "view" of how the insert happened.

> insert :: (Hashable k, Ord k) => k -> v -> PureCache k v -> PureCache k v
> insert key val pc = trim $!
>     let (mbOld, q) = HashPSQ.insertView key (pcTick pc) val (pcQueue pc) in
>     trim $! pc
>         { pcSize  = if isNothing mbOld then pcSize pc + 1 else pcSize pc
>         , pcTick  = pcTick pc + 1
>         , pcQueue = q
>         }

PureCache: lookup
-----------------

`alter` allows us to modify a value (in this case, bump its priority) and return
something (the value, if found) at the same time:

```haskell
alter
    :: (Hashable k, Ord k, Ord p)
    => (Maybe (p, v) -> (b, Maybe (p, v)))
    -> k -> HashPSQ.HashPSQ k p v -> (b, HashPSQ.HashPSQ k p v)
```

PureCache: lookup
-----------------

> lookup
>     :: (Hashable k, Ord k)
>     => k -> PureCache k v -> Maybe (v, PureCache k v)
> lookup k pc = case HashPSQ.alter lookupAndBump k (pcQueue pc) of
>     (Nothing, _) -> Nothing
>     (Just x, q)  ->
>         let !pc' = trim $ pc {pcTick = pcTick pc + 1, pcQueue = q}
>         in Just (x, pc')
>   where
>     lookupAndBump Nothing       = (Nothing, Nothing)
>     lookupAndBump (Just (_, x)) = (Just x,  Just ((pcTick pc), x))

PureCache: alter
----------------

Alter might be _"slow"_: we might need to move an element from the bottom all
the way to the top, or vice versa.

. . .

When we do a `lookup`, we always set the priority to `pcTick` -- a "constant",
not depending on the value or priority of the item that's already in the cache.

PureCache: (ab)using invariants
-------------------------------

```haskell
alter
    :: (Hashable k, Ord k, Ord p)
    => (Maybe (p, v) -> (b, Maybe (p, v)))
    -> k -> HashPSQ.HashPSQ k p v -> (b, HashPSQ.HashPSQ k p v)
```

. . .

```haskell
unsafeLookupIncreasePriority
    :: (Hashable k, Ord k, Ord p)
    => k -> p -> HashPSQ k p v -> (Maybe (p, v), HashPSQ k p v)
```

PureCache: (ab)using invariants
-------------------------------

> lookup2
>     :: (Hashable k, Ord k)
>     => k -> PureCache k v -> Maybe (v, PureCache k v)
> lookup2 k pc =
>     case HashPSQ.unsafeLookupIncreasePriority k (pcTick pc) (pcQueue pc) of
>         (Nothing, _) -> Nothing
>         (Just (_, x), q) ->
>             let !pc' = trim $ pc {pcTick = pcTick pc + 1, pcQueue = q}
>             in Just (x, pc')

There is also an `unsafeInsertIncreasePriorityView` that we could have used to
make `insert` faster.

PureCache: more unsafety
------------------------

Plenty of **FUN** opportunities for those whole like to shoot themselves in the
foot!

- `unsafeInsertIncreasePriorityView`
- `unsafeInsertNew`
- `unsafeInsertWithIncreasePriority`
- `unsafeInsertWithIncreasePriorityView`
- `unsafeLookupIncreasePriority`
- `unsafeMapMonotonic`
- ...

PureCache: summary
------------------

Assuming `(Hashable k, Ord k)`:

```haskell
data PureCache k v
empty :: Int -> PureCache k v
insert :: k -> v -> PureCache k v -> PureCache k v
lookup :: k -> PureCache k v -> Maybe (v, PureCache k v)
```

SimpleCache
===========

SimpleCache: pure to IO
-----------------------

If we're only writing pure code, we're good to go!

. . .

... but often caches are used alongside IO.

. . .

> newtype SimpleCache k v = SimpleCache (IORef (PureCache k v))

SimpleCache: concurrency primitives
-----------------------------------

    FAST        <->        SAFE
    
    IORef  ...  MVar  ...  TVar
    
    UNSAFE      <->      "SLOW"

. . .

In most common cases, `TVar` is usually fast enough...

SimpleCache: initializing
-------------------------

> newSimpleCache :: Int -> IO (SimpleCache k v)
> newSimpleCache capacity =
>     SimpleCache <$> IORef.newIORef (empty capacity)

SimpleCache: interface
----------------------

```haskell
listS3 :: BucketName -> IO [ObjectName]
listS3 bucketName =
    simpleCached globalCache bucketName $ do
    env <- Aws.newEnv Aws.Discover
    rsp <- Aws.runResourceT $ Aws.runAWS hAwsEnv $ Aws.send $
        S3.listObjects bucketName
    return $ map (view S3.oKey) $ rsp ^. S3.lorContent
```

SimpleCache: one function
-------------------------

```haskell
simpleCached
    :: (Hashable k, Ord k)
    => SimpleCache k v -> k -> IO v -> IO v
simpleCached (SimpleCache ref) k io = do
```

SimpleCache: one function
-------------------------

> simpleCached
>     :: (Hashable k, Ord k)
>     => SimpleCache k v -> k -> IO v -> IO v
> simpleCached (SimpleCache ref) k io = do
>     lookupRes <- IORef.atomicModifyIORef' ref $ \c -> case lookup2 k c of
>         Nothing      -> (c,  Nothing)
>         Just (v, c') -> (c', Just v)
>     case lookupRes of
>         Just v  -> return v
>         Nothing -> do
>             v <- io
>             IORef.atomicModifyIORef' ref $ \c -> (insert k v c, ())
>             return v

<!--
We may execute requests more than once.  That's okay, caches are highly
application-specific.  In our case, doing something twice is better than
blocking.
-->

Q1: What can go wrong with the code above?

. . .

Q2: Is it exception safe?

StripedCache
============

StripedCache: contention
------------------------

How does `atomicModifyIORef'` work?

```haskell
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' ref f = do
    x <- readIORef ref
    let (!x', !y) = f x

    -- Atomically write x' if value is still x
    swapped <- compareAndSwap ref x x'
    if swapped
        then return y
        else atomicModifyIORef' ref f  -- Retry
```

StripedCache: contention
------------------------

Imagine a really shitty queueing system at some administrative office:

 -  You can take a numbered form whenever you want
 -  Every form in the stack has the same number
 -  Whenever they call a number, the first one to hand in the form gets to
    submit it
 -  They bring in a new stack of forms with the next number

. . .

If filling in the form is fast, and there are not that many people, this is
a surprisingly good solution...

. . .

Since we can't make filling out the form faster, can we decrease the amount of
people queueing?

StripedCache: one slide
-----------------------

> newtype StripedCache k v = StripedCache (V.Vector (SimpleCache k v))

. . .

> newStripedCache :: Int -> Int -> IO (StripedCache k v)
> newStripedCache numStripes capacityPerStripe = StripedCache <$>
>     V.replicateM numStripes (newSimpleCache capacityPerStripe)

. . .

> stripedCached
>     :: (Hashable k, Ord k)
>     => StripedCache k v -> k -> IO v -> IO v
> stripedCached (StripedCache v) k =
>     simpleCached (v V.! idx) k
>   where
>     idx = hash k `mod` V.length v

Conclusion
==========

Six data structures
-------------------

![](images/sixpack.jpg)

Six data structures
-------------------

<!--
Let's go through them in case you decided to take a nap.
-->

 -  OrdPSQ (think tournament trees)
 -  IntPSQ (PATRICIA)
 -  HashPSQ (OrdPSQ + IntPSQ)
 -  PureCache (HashPSQ + capacity)
 -  SimpleCache (IORef + PureCache)
 -  StripedCache (Vector + SimpleCache)

Questions?
==========

benchmarks
----------

![](images/benchmarks-00.png)

benchmarks
----------

![](images/benchmarks-01.png)

patat
-----

<http://github/jaspervdj/patat>

Appendices
==========

> newtype Distance = Distance Double deriving (Eq, Ord, Show)
> instance Semigroup Distance where Distance x <> Distance y = Distance (x + y)
> instance Monoid Distance where mempty = Distance 0

> test :: Map Int (Distance, [Int])
> test = dijkstra
>     (\x -> map (first Distance) $ fromMaybe [] (Map.lookup x table))
>     1
>   where
>     table = Map.fromList
>         [ (1, [(14.0, 6), (9.0, 3), (7.0, 2)])
>         , (2, [(7.0, 1), (10.0, 3), (15.0, 4)])
>         , (3, [(2.0, 6), (11.0, 4), (10.0, 2), (9.0, 1)])
>         , (4, [(6.0, 5), (11.0, 3), (15.0, 2)])
>         , (5, [(9.0, 6), (6.0, 4)])
>         , (6, [(14.0, 1), (2.0, 3), (9.0, 5)])
>         ]

> main :: IO ()
> main = print test
