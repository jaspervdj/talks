---
title: 'Binomial Heap 101'
author: 'Jasper Van der Jeugt'
date: 2018-10-11
patat:
  incrementalLists: true
  slideLevel: 2
  wrap: true
  images:
    backend: w3m
  theme:
    codeBlock: [vividBlack, onRgb#eeeeee]
...

Introduction
============

About me
--------

Hi, I'm Jasper

- Got started with Haskell a while ago
- I use Haskell at work (Fugue)
- I use Haskell in my free time (open source)
- I try to be involved in the community (SoH, Zurihac)

@jaspervdj -- jaspervdj.be

About this talk
---------------

This is a talk about dependent types.

It was not (originally) really meant for beginners but I've tried to make it as
accessible as possible.

. . .

We will spend some quality time with `GHCi` and try to get a feel for what it is
like to do dependently typed programming in Haskell.

This slides are literate Haskell and contain all required code (but it is
unlikely that we will look at all required code).

Stop me at any time to ask questions.

Origin of this talk
-------------------

- Originated from a practical origin
- Started as a LC interpreter that was suspiciously fast
- Tried to convince myself of its correctness by using some
  dependent types
- Used list for accessing arguments which is asymptotically slow
- ???
- A well typed binomial heap

About this talk
---------------

Dependent types: why?

. . .

Does a specific programming language have types?

- Anne: I moved from bash to Python because I wanted to have types
- Brett: I moved from Python to Java because I wanted to have types
- Caroline: I moved from Java to Haskell because I wanted to have types
- Dennis: I moved from Haskell to Agda because I wanted to have types

. . .

"Having types" is not really a yes/no question and more of a taking a specific
place in the design space

This talk is literate Haskell
-----------------------------

> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}

This talk is literate Haskell
-----------------------------

> import           Data.List          (intercalate, minimumBy)
> import           Data.List.NonEmpty (NonEmpty (..))
> import qualified Data.List.NonEmpty as NonEmpty
> import           Data.Ord           (comparing)

Let's get started!
------------------

Simple natural numbers:

> data Nat = Zero | Succ Nat deriving (Show)
>
> type NZero  = 'Zero
> type NOne   = 'Succ NZero
> type NTwo   = 'Succ NOne
> type NThree = 'Succ NTwo

Let's get started!
------------------

~~~~~{.haskell}
data Nat = Zero | Succ Nat deriving (Show)
~~~~~

> data Vec (n :: Nat) a where
>     VNull :: Vec 'Zero a
>     VCons :: a -> Vec n a -> Vec ('Succ n) a

. . .

__GHCi time!__

A well-typed list?
------------------

> instance Show a => Show (Vec n a) where
>     show = show . vecToList
>
> vecToList :: Vec n a -> [a]
> vecToList VNull       = []
> vecToList (VCons x v) = x : vecToList v

. . .

> vecHead :: Vec ('Succ n) a -> a
> vecHead (VCons x _) = x

. . .

__GHCi time!__

A well-typed list
-----------------

In this talk, we will go over a similar way to deal with _binomial heaps_.

. . .

A binomial heap is a structure that can be used to implement priority queues.
Hence, the most important operations are `push` and `pop`.

. . .

There is a correspondence in between a binomial heap of size _n_ and the binary
notation of the number _n_.  That is why we will be lifting _binary numbers to
the type level_.

. . .

__GHCi time!__

How to prove things
===================

Two approaches
--------------

Extreme hand-waving and squinting: there are two ways to work with these really
strong types in Haskell.

1.  Ensure things are "correct" by _construction_
2.  Provide a _proof_ that things are "correct"

. . .

You want do do **as much as possible** using the first method.

What is a proof?
----------------

This builds on the singletons approach introduced by Richard Eisenberg and
Stephanie Weirich:

<https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf>

It's all about equality
-----------------------

> data Proxy a = Proxy

~~~~~{.haskell}
cast00 :: Proxy a -> Proxy b
cast00 x = x  -- Or: `cast00 = id`
~~~~~

. . .

__GHCi time!__

It's all about equality
-----------------------

> data EqualityProof (a :: k) (b :: k) where
>     QED :: EqualityProof a a
>
> type a :~: b = EqualityProof a b

. . .

> cast00 :: a :~: b -> Proxy a -> Proxy b
> cast00 proof x =
>   -- No equality here...
>   case proof of
>     QED -> x  -- Inside here, we have equality

It's all about equality
-----------------------

~~~~~{.haskell}
data EqualityProof (a :: k) (b :: k) where
    QED :: EqualityProof a a

type a :~: b = EqualityProof a b
~~~~~

_All you need to do_™® is construct an `EqualityProof`.

Addition and a proof
--------------------

~~~~~{.haskell}
data Nat = Zero | Succ Nat deriving (Show)
~~~~~

> type family NAdd (x :: Nat) (y :: Nat) :: Nat where
>     NAdd 'Zero     y = y
>     NAdd ('Succ x) y = 'Succ (NAdd x y)
>
> cast01 :: Proxy (NAdd 'Zero n) -> Proxy n
> cast01 = id

Addition and a proof
--------------------

Easy peasy lemon squeezy:

~~~~~{.haskell}
cast01 :: Proxy (NAdd 'Zero n) -> Proxy n
cast01 = id
~~~~~

GHC does not accept:

~~~~~{.haskell}
cast02 :: Proxy (NAdd n 'Zero) -> Proxy n
cast02 = id
~~~~~

Addition and a proof
--------------------

Goal: construct a proof

~~~~~{.haskell}
lemma1 :: ??? -> Proxy (NAdd n 'Zero) :~: n
~~~~~

. . .

What is `???`?  Is it just `n`?

Addition and a proof
--------------------

~~~~~{.haskell}
data Nat = Zero | Succ Nat deriving (Show)
~~~~~

A singleton:

> data SNat (n :: Nat) where
>     SZero :: SNat 'Zero
>     SSucc :: SNat n -> SNat ('Succ n)

Addition and a proof
--------------------

~~~~~{.haskell}
lemma1 :: SNat n -> NAdd n 'Zero :~: n
~~~~~

. . .

~~~~~{.haskell}
lemma1 SZero = QED
~~~~~

Addition and a proof
--------------------

> lemma1 :: SNat n -> NAdd n 'Zero :~: n

> lemma1 SZero = QED
> lemma1 (SSucc n) = case lemma1 n of QED -> QED

Addition and a proof
--------------------

~~~~~{.haskell}
lemma1 :: SNat n -> NAdd n 'Zero :~: n
~~~~~

> cast02 :: SNat n -> Proxy (NAdd n 'Zero) -> Proxy n
> cast02 snat = case lemma1 snat of QED -> id

Binomial trees
==============


Definition of binomial trees
----------------------------

Recursive definition:

- A binomial tree of order 0 is a **single root node**

- A binomial tree of order k has a **single root node** and **children** which
  are the binomial trees of orders **k−1, k−2, ..., 2, 1, 0** (in this order).

Definition of binomial trees
----------------------------

Recursive definition:

> data Tree (k :: Nat) a where
>     Tree :: a -> Children k a -> Tree k a

> data Children (k :: Nat) a where
>     CZero :: Children 'Zero a
>     CCons :: Tree k a -> Children k a -> Children ('Succ k) a

Definition of binomial trees
----------------------------

![](images/illustration-01.png)

Creating binomial trees
-----------------------

> singletonTree :: a -> Tree 'Zero a
> singletonTree x = Tree x CZero

> mergeTree :: Ord a => Tree k a -> Tree k a -> Tree ('Succ k) a
> mergeTree l@(Tree lroot lchildren) r@(Tree rroot rchildren)
>     | lroot <= rroot = Tree lroot (CCons r lchildren)
>     | otherwise      = Tree rroot (CCons l rchildren)

Creating binomial trees
-----------------------

![](images/illustration-02.png)

Creating binomial trees
-----------------------

~~~~~{.haskell}
singletonTree :: a -> Tree 'Zero a
singletonTree x = Tree x CZero

mergeTree :: Ord a => Tree k a -> Tree k a -> Tree ('Succ k) a
mergeTree l@(Tree lroot lchildren) r@(Tree rroot rchildren)
    | lroot <= rroot = Tree lroot (CCons r lchildren)
    | otherwise      = Tree rroot (CCons l rchildren)
~~~~~

. . .

__GHCi time!__

Introducing binary numbers
==========================

Combining trees into heaps
--------------------------

- These trees can only hold collections of exactly 2ᵏ elements

- This is not super useful for general purpose containers

- Binomial heaps solve this by simply having a collection of trees, with at most
  one tree for every order

. . .

For example: how can we create a binomial heap with 5 elements?

Defining binary numbers
-----------------------

> data Binary
>     = B0 Binary
>     | B1 Binary
>     | BEnd
>     deriving (Show)

. . .

Right-to-left:

> type BSix = 'B0 ('B1 ('B1 'BEnd))

. . .

Why?  Well...

Incrementing binary numbers
---------------------------

> type family BInc (binary :: Binary) :: Binary where
>     BInc 'BEnd        = 'B1 'BEnd
>     BInc ('B0 binary) = 'B1 binary
>     BInc ('B1 binary) = 'B0 (BInc binary)

. . .

> type BZero  = 'B0 'BEnd
> type BOne   = BInc BZero
> type BTwo   = BInc BOne
> type BThree = BInc BTwo

. . .

__GHCi time!__

Adding binary numbers
---------------------

> type family BAdd (x :: Binary) (y :: Binary) :: Binary where
>     BAdd 'BEnd   y       = y
>     BAdd x       'BEnd   = x
>     BAdd ('B0 x) ('B0 y) = 'B0 (BAdd x y)
>     BAdd ('B1 x) ('B0 y) = 'B1 (BAdd x y)
>     BAdd ('B0 x) ('B1 y) = 'B1 (BAdd x y)
>     BAdd ('B1 x) ('B1 y) = 'B0 (BInc (BAdd x y))

. . .

> type BFour = BAdd BTwo BTwo
> type BFive = BAdd BFour BOne

__GHCi time!__

Binary singleton
----------------

Again, we have to define a singleton if we want to write proofs later:

~~~~~{.haskell}
data Binary
    = B0 Binary
    | B1 Binary
    | BEnd
    deriving (Show)
~~~~~

> data SBin (b :: Binary) where
>     SB0   :: SBin b -> SBin ('B0 b)
>     SB1   :: SBin b -> SBin ('B1 b)
>     SBEnd :: SBin 'BEnd

Back to heaps
=============

The forest
----------

As we said earlier, a binomial heap is a collection binomial trees, and we can
have at most one of any order

> data Forest (k :: Nat) (b :: Binary) a where
>     FEnd :: Forest k 'BEnd a
>     F0   ::             Forest ('Succ k) b a -> Forest k ('B0 b) a
>     F1   :: Tree k a -> Forest ('Succ k) b a -> Forest k ('B1 b) a

. . .

We are representing the "shape" in the datatype:

E.g. _Forest 3 101_ refers to binomial trees of order 3 and 5 (and no tree of
order 4).

Building a forest
-----------------

> emptyForest :: Forest k 'BEnd a
> emptyForest = FEnd

. . .

Inserting a tree: we can only insert a tree of order `k`:

> insertTree
>     :: Ord a
>     => Tree k a -> Forest k b a
>     -> Forest k (BInc b) a
> insertTree s FEnd     = F1 s FEnd
> insertTree s (F0 f)   = F1 s f
> insertTree s (F1 t f) = F0 (insertTree (mergeTree s t) f)

. . .

__GHCi time!__

Merging two forests
-------------------

Merging to forests is to binary addition what inserting a new tree is to binary
increment

Merging two forests
-------------------

![](images/illustration-03.png)

Merging two forests
-------------------

![](images/illustration-04.png)

Merging two forests
-------------------

> mergeForests
>     :: Ord a
>     => Forest k lb a -> Forest k rb a
>     -> Forest k (BAdd lb rb) a
> mergeForests FEnd      rf   = rf
> mergeForests lf        FEnd = lf
> mergeForests (F0 lf)   (F0 rf)   = F0 (mergeForests lf rf)
> mergeForests (F1 l lf) (F0 rf)   = F1 l (mergeForests lf rf)
> mergeForests (F0 lf)   (F1 r rf) = F1 r (mergeForests lf rf)
> mergeForests (F1 l lf) (F1 r rf) =
>     F0 (insertTree (mergeTree l r) (mergeForests lf rf))

Merging two forests
-------------------

> exampleForest1 = insertTree (singletonTree 'f') emptyForest

> exampleForest5 =
>     insertTree (singletonTree 'a') $
>     insertTree (singletonTree 'b') $
>     insertTree (singletonTree 'c') $
>     insertTree (singletonTree 'd') $
>     insertTree (singletonTree 'e') $
>     emptyForest

. . .

__GHCi time!__

Merging two forests
-------------------

![](images/illustration-04.png)

Merging forests
---------------

Why is this so "easy?"

. . .

We have constructed our value, types and type families such that there almost a
one-to-one mapping in between binary addition and merging forests!

Gif
---

![](images/overlay-01.gif)

Gif
---

![](images/overlay-02.gif)

Gif
---

![](images/overlay-03.gif)

Gif
---

![](images/overlay-04.gif)

Gif
---

![](images/overlay-01.gif)

The binomial heap
=================

The binomial heap
-----------------

> newtype Heap (b :: Binary) a = Heap {unHeap :: Forest 'Zero b a}
>
> emptyHeap :: Heap 'BEnd a
> emptyHeap = Heap emptyForest
>
> pushHeap :: Ord a => a -> Heap b a -> Heap (BInc b) a
> pushHeap x (Heap forest) = Heap (insertTree (singletonTree x) forest)
>
> mergeHeap :: Ord a => Heap lb a -> Heap rb a -> Heap (BAdd lb rb) a
> mergeHeap (Heap lf) (Heap rf) = Heap (mergeForests lf rf)

. . .

__GHCi time!__

Binomial heaps: let's break it down
===================================

This is where things get more complicated
-----------------------------------------

- Things get _significantly_ more complicated when we try to implement popping
  the smallest element from the queue.

- For reference, I implemented the current heap in a couple of hours, whereas I
  worked on the rest of the code on and off for about a week.

- We need about 4 more lemmas and another auxiliary GADT that has a bunch of
  constraints; unfortunately we won't have time to go through to everything

How popping works
-----------------

1.  We first select the tree with the smallest root and remove it from the heap

2.  We break up the tree we selected into its root (which will be the element
    that is "popped") and its children, which we turn into a new heap.

3.  We merge the remainder heap from step 1 together with the new heap we
    made out of the children of the removed tree.

How popping works
-----------------

![](images/illustration-06.png)

How popping works
-----------------

![](images/illustration-07.png)

How popping works
-----------------

![](images/illustration-08.png)

Taking apart a single tree
==========================

Taking a part a single tree
---------------------------

We will focus on step 2: it is easy enough to understand but it still a good
example of how to use these proofs.

Step 2: We break up the tree we selected into its root (which will be the
element that is "popped") and its children, which we turn into a new heap.

Taking apart a tree of order k
------------------------------

Let's talk through the invariants first:

-   A tree of `k` has `2ᵏ` elements
-   If we remove the root, we have `k` child trees with `2ᵏ - 1` elements total.
-   Every child becomes a tree in the new heap.
-   This means that the heap contains `k` full trees.
-   Its shape will be written as `k` "1"s.

The numbers work out: if you write `k` "1"s, you get the binary notation of
`2ᵏ - 1`.

Taking apart a tree of order k
------------------------------

![](images/illustration-07.png)

k "1"s
------

We introduce a type family for computing `k` "1"s:

> type family Ones (n :: Nat) :: Binary where
>     Ones 'Zero     = 'BEnd
>     Ones ('Succ n) = 'B1 (Ones n)

. . .

__GHCi time!__

Taking apart a tree
-------------------

> splitTree
>     :: Tree n a -> (a, Heap (Ones n) a)
> splitTree (Tree x c) =
>     (x, Heap (childrenToForest c))

. . .

> childrenToForest
>     :: Children n a
>     -> Forest 'Zero (Ones n) a
> childrenToForest children =
>     childrenToForest_go SZero (childrenSingleton children) FEnd children

Sidenode: obtaining singletons
------------------------------

> childrenSingleton :: Children n a -> SNat n
> childrenSingleton CZero       = SZero
> childrenSingleton (CCons _ c) = SSucc (childrenSingleton c)

. . .

There are also other ways to create a singleton.

Reversing
---------

The tricky bit is that the `Children` are in descending order, and the `Forest`
has the trees in ascending order.

. . .

We can reverse a list easily using an accumulator in Haskell.

. . .

> reverse :: [a] -> [a]
> reverse = go []
>   where
>      go acc list = case list of
>        []       -> acc
>        (x : xs) -> go (x : acc) xs

Invariants
----------

The length of the list that we get out is the length of the list we put in

~~~~~{.haskell}
reverse :: [a] -> [a]
reverse = go []
  where
    -- Invariant: `length acc ++ length list == n`
    go acc list = case list of
      []       -> acc
      (x : xs) -> go (x : acc) xs
~~~~~

Accumulator invariants
----------------------

> childrenToForest_go
>     :: m ~ NAdd x n
>     => SNat x
>     -> SNat n
>     -> Forest n (Ones x) a
>     -> Children n a
>     -> Forest 'Zero (Ones m) a

Base case
---------

~~~~~{.haskell}
childrenToForest_go xnat _snat@SZero acc CZero =
  acc
~~~~~

Can we just return our accumulator?

. . .

Unfortunately not:

- Accumulator type: `Forest n (Ones x)`
- Expected type: `Forest n (Ones m)`

. . .

But, we know that:

~~~~~~
  n ~ 'Zero, m ~ NAdd x n
⊢ m ~ NAdd x 'Zero
~~~~~~

Base case
---------

Simplified:

- Accumulator type: `Forest n (Ones x)`
- Expected type: `Forest n (Ones (NAdd x 'Zero))`

. . .

~~~~~{.haskell}
lemma1 xnat :: NAdd x 'Zero :~: n
~~~~~

. . .

Using the lemma:

> childrenToForest_go xnat _snat@SZero acc CZero =
>     case lemma1 xnat of QED -> acc

The inductive case
------------------

The inductive case is a bit harder and requires us to prove that:

~~~~~~
  m ~ NAdd x n, m ~ NAdd x n, n ~ 'Succ k
⊢ Ones m ~ 'B1 (Ones (NAdd x k))
~~~~~~

. . .

GHC does a great job and ends up with something like:

~~~~~~
Ones (NAdd x (Succ k)) ~ 'B1 (Ones (NAdd x k))
~~~~~~

. . .

This means we need prove commutativity for `NAdd` (`lemma2`).

The inductive case
------------------

Proving that `xnat + nnat == nnat + xnat`:

> childrenToForest_go xnat (SSucc nnat) acc (CCons tree children) =
>     case lemma2 xnat nnat of
>         QED -> childrenToForest_go
>             (SSucc xnat)
>             nnat
>             (F1 tree acc)
>             children

Proving commutativity
---------------------

The lemma to prove commutativity:

> lemma2 :: SNat n -> SNat m -> NAdd n ('Succ m) :~: 'Succ (NAdd n m)
> lemma2 SZero     _ = QED
> lemma2 (SSucc n) m = case lemma2 n m of QED -> QED

Taking apart a single tree
--------------------------

~~~~~{.haskell}
splitTree
    :: Tree n a -> (a, Heap (Ones n) a)
splitTree (Tree x c) =
    (x, Heap (childrenToForest c))
~~~~~

. . .

__GHCi time!__

Popping an element from the heap
================================

Popping an element from the heap
--------------------------------

1.  We first select the tree with the smallest root and remove it from the heap

2.  We break up the tree we selected into its root (which will be the element
    that is "popped") and its children, which we turn into a new heap.

3.  We merge the remainder heap from step 1 together with the new heap we
    made out of the children of the removed tree.

Overview
========

Fast-forward
------------

![](images/overview.jpg)

Fast-forward
------------

![](images/illustration-05.png)

Fast-forward
------------

Vector utilities

. . .

> instance Functor (Vec n) where
>     fmap _ VNull       = VNull
>     fmap f (VCons x v) = VCons (f x) (fmap f v)

> vecToNonEmpty :: NNonZero n ~ 'True => Vec n a -> NonEmpty a
> vecToNonEmpty (VCons x v) = x :| vecToList v

Fast-forward
------------

More type families

. . .

> type family NNonZero (n :: Nat) :: Bool where
>     NNonZero 'Zero     = 'False
>     NNonZero ('Succ _) = 'True

> type family BNonZero (b :: Binary) :: Bool where
>     BNonZero 'BEnd   = 'False
>     BNonZero ('B1 b) = 'True
>     BNonZero ('B0 b) = BNonZero b

Fast-forward
------------

Even more type families

. . .

> type family Popcount (b :: Binary) :: Nat where
>     Popcount 'BEnd   = 'Zero
>     Popcount ('B1 b) = 'Succ (Popcount b)
>     Popcount ('B0 b) = Popcount b

. . .

...and lemmas:

. . .

> lemma3
>     :: BNonZero b ~ 'True
>     => SBin b
>     -> NNonZero (Popcount b) :~: 'True
> lemma3 (SB1 _) = QED
> lemma3 (SB0 b) = case lemma3 b of QED -> QED

Fast-forward
------------

And even more type families:

. . .

> type family Width (binary :: Binary) :: Nat where
>     Width 'BEnd        = 'Zero
>     Width ('B0 binary) = 'Succ (Width binary)
>     Width ('B1 binary) = 'Succ (Width binary)

Fast-forward
------------

And a constrained datatype:

. . .

> data CutTree (k :: Nat) (b :: Binary) a where
>     CutTree
>         :: Width (BAdd b (Ones x)) ~ Width (BInc (BAdd b (Ones x)))
>         => SNat x
>         -> SNat k
>         -> Tree (NAdd k x) a
>         -> Forest k b a
>         -> CutTree k (BInc (BAdd b (Ones x))) a

Fast-forward
------------

Auxiliary functions:

. . .

> lumberjack_go
>     :: forall k b a.
>        SNat k
>     -> Forest k b a
>     -> Vec (Popcount b) (CutTree k b a)

> lumberjack_go _ FEnd = VNull

Fast-forward
------------

Long auxiliary functions:

. . .

> lumberjack_go nnat0 (F0 forest0) = fmap
>     (\cutTree -> case cutTree of
>         CutTree xnat (SSucc nnat) t1 forest1 -> CutTree
>             (SSucc xnat)
>             nnat
>             (case lemma2 nnat xnat of QED -> t1)
>             (F0 forest1))
>     (lumberjack_go (SSucc nnat0) forest0)

Fast-forward
------------

More cases:

. . .

> lumberjack_go nnat0 (F1 tree0 forest0) = VCons
>     (CutTree
>         SZero
>         nnat0
>         (case lemma1 nnat0 of QED -> tree0)
>         (F0 forest0))
>     (fmap
>         (\cutTree -> case cutTree of
>             CutTree xnat (SSucc nnat) t1 forest1 -> CutTree
>                 (SSucc xnat)
>                 nnat
>                 (case lemma2 nnat xnat of QED -> t1)
>                 (F1 tree0 forest1))
>         (lumberjack_go (SSucc nnat0) forest0))

Fast-forward
------------

Wrapping up:

. . .

> lumberjack
>     :: forall b a. BNonZero b ~ 'True
>     => Forest 'Zero b a
>     -> NonEmpty.NonEmpty (CutTree 'Zero b a)

> lumberjack trees =
>     let cutTrees :: Vec (Popcount b) (CutTree 'Zero b a)
>         cutTrees = lumberjack_go SZero trees in

>     case lemma3 (forestSingleton trees :: SBin b) of
>          QED -> vecToNonEmpty cutTrees

Fast-forward
------------

Constructing singletons:

. . .

> forestSingleton :: Forest k b a -> SBin b
> forestSingleton FEnd     = SBEnd
> forestSingleton (F0 t)   = SB0 (forestSingleton t)
> forestSingleton (F1 _ t) = SB1 (forestSingleton t)

Fast-forward
------------

Wrapping up (again):

. . .

> popForest
>     :: forall a b. Ord a
>     => CutTree 'Zero b a
>     -> (a, Forest 'Zero (BDec b) a)

Fast-forward
------------

Wrapping up (again):

. . .

> popForest (CutTree
>             _xnat _nnat
>             (Tree x (children :: Children r a))
>             (forest :: Forest 'Zero l a)) =
>     let cforest = childrenToForest children
>         merged :: Forest 'Zero (BAdd l (Ones r)) a
>         merged = mergeForests forest cforest
>         evidence :: SBin (BAdd l (Ones r))
>         evidence = forestSingleton merged in
>     (x, case lemma4 evidence of QED -> merged)

Fast-forward
------------

Wait, we need another type family:

. . .

> type family BDec (binary :: Binary) :: Binary where
>     BDec ('B1 b) = 'B0 b
>     BDec ('B0 b) = 'B1 (BDec b)

And a lemma:

. . .

> lemma4
>     :: (Width x ~ Width (BInc x))
>     => SBin x
>     -> BDec (BInc x) :~: x
> lemma4 (SB0 _) = QED
> lemma4 (SB1 b) = case lemma4 b of QED -> QED

popHeap
=======

popHeap
-------

> popHeap
>     :: (BNonZero b ~ 'True, Ord a)
>     => Heap b a -> (a, Heap (BDec b) a)
> popHeap (Heap forest0) =
>     let cutTrees = lumberjack forest0
>         selected = minimumBy (comparing cutTreeRoot) cutTrees in
>     case popForest selected of
>         (x, forest1) -> (x, Heap forest1)
>   where
>     cutTreeRoot :: CutTree k b a -> a
>     cutTreeRoot (CutTree _ _ (Tree x _) _) = x

. . .

__GHCi time!__

Questions?
==========

Are there multiple representations of zero?
-------------------------------------------

Yes.

. . .

Leftists heaps by Lars Brünjes: <https://github.com/brunjlar/heap>

~~~~~{.haskell}
data Binary   = Zero | StrictlyPositive Positive
data Positive = B1End | B0 Positive | B1 Positive
~~~~~

Does this have a runtime cost?
------------------------------

Yes

Why are binary numbers right-to-left?
-------------------------------------

Well...

. . .

> type family BIncLTR (b :: Binary) :: Binary where
>     BIncLTR b = FromRight 'B1 (Carry b)

> type family Carry (b :: Binary) :: Either Binary Binary where
>     Carry ('B1 'BEnd) = 'Left ('B0 'BEnd)
>     Carry ('B0 'BEnd) = 'Right ('B1 'BEnd)
>     Carry ('B0 b)     = 'Right (UnEither 'B1 'B0 (Carry b))
>     Carry ('B1 b)     = MapEither 'B0 'B1 (Carry b)

Why are binary numbers right-to-left?
-------------------------------------

> type family MapEither
>         (f :: a -> c) (g :: b -> d) (e :: Either a b) :: Either c d where
>     MapEither f _ ('Left x)  = 'Left (f x)
>     MapEither _ g ('Right y) = 'Right (g y)

> type family UnEither
>         (f :: a -> c) (g :: b -> c) (e :: Either a b) :: c where
>     UnEither f _ ('Left x)  = f x
>     UnEither _ g ('Right y) = g y

> type family FromRight (f :: a -> b) (e :: Either a b) :: b where
>     FromRight f ('Left x) = f x
>     FromRight _ ('Right y) = y

Show instances
==============

"pretty"-printing of heaps
--------------------------

> instance Show a => Show (Heap b a) where
>     show = show . unHeap
>
> instance Show a => Show (Forest n b a) where
>     show = intercalate "\n" . showForest 0
>
> instance Show a => Show (Tree k a) where
>     show = intercalate "\n" . showTree ""
>
> showForest :: Show a => Int -> Forest m c a -> [String]
> showForest _ FEnd = []
> showForest order (F0 trees) =
>     ("(no tree of order " ++ show order ++ ")") :
>     showForest (order + 1) trees
> showForest order (F1 tree trees) =
>     ("(tree of order " ++ show order ++ ")") :
>     showTree " " tree ++
>     showForest (order + 1) trees
>
> showTree :: Show a => String -> Tree m a -> [String]
> showTree indentation (Tree x children) =
>     (indentation ++ show x) :
>     showChildren (' ' : indentation) children
>
> showChildren :: Show a => String -> Children m a -> [String]
> showChildren _           CZero        = []
> showChildren indentation (CCons x xs) =
>     showTree indentation x ++ showChildren indentation xs
