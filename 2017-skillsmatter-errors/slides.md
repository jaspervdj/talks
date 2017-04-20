---
title: An informal guide to better compiler errors
author: Jasper Van der Jeugt
date: 2016-04-03
patat:
  incrementalLists: true
  wrap: true
...

# Introduction

## Update: recording

A recording of this talk is available here:

<https://skillsmatter.com/skillscasts/9879-an-informal-guide-to-better-compiler-errors-jasper-van-der-jeugt>

## About me

- Got started with Haskell a long time ago
- Active in the Haskell community
- Uses Haskell at work

## About Fugue

- Expressive functional/declarative programming language (Ludwig) to manage
  infrastructure
- A Ludwig program describes what you want your infrastructure to look like
- Fugue makes it so
- For ease of adoption, error message quality is very important

## About this talk

We're going to talk about the quality of error messages and how we can improve
that.  For most of the talk, we'll focus on _non-embedded_ DSLs because these
allow you to tweak more things.

Most of the talk applies to "interpreted DSLs" as well as "actually compiled"
DSLs.

# What is a good error message?

## What is a good error message?

There is no objective way to measure "quality" of an error message.

There is clearly an overlap with User Experience Design / Human Computer
Interaction.

Let's ask the other question first: what's a bad error message?

## What is a bad error message?

Too broad

. . .

    $ python test.py
      File "test.py", line 1
        class Foo
                ^
    SyntaxError: invalid syntax

## What is a bad error message?

Leaking internal names or using weird language

. . .

    $ php test.php
    Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM


## What is a bad error message?

Not enough context

. . .

    $ psc test.ps
    Error in declaration test
    Cannot unify b680 with a682.

## What is a bad error message?

Too much context

. . .

    Could not unify

    {                        |   {
      firstName : String,    |     firstName : String,
      lastName : String,     |     lastName : String,
                             |
      hairColor : String,    |     hairColor : String,
      eyeColor : String,     |     eyeColor : String,
                             |
      address : String,      |     address : String,
      phoenNumber : String,  |     phoneNumber : String,
      email : String,        |     email : String,
                             |
      pets : Int             |     pets : Int
    }                        |   }

## What is a good error message?

In an ideal world, I think an error message would tell you _exactly what to do_.

. . .

In reality, this is often not possible:

    fun add(x): x + x

    foo: add(1, 2)

The compiler cannot possibly know if we wanted `add` to have two parameters or
just one.

# More is more

## Why do we want more error messages?

A compiler should _almost always_ try to produce more rather than less error
messages.  Stopping at the first error is not a good thing.

. . .

Sure, computers are fast today, but:

- I want to run continuous integration on different platforms
- My compiler could provide feedback to an editor to show red squiggly lines
- It might not be clear if the error is the _definition_ or _use_ site

## How can we provide more error messages?

The easiest way to throw errors in a pure way is using the `Either` monad:

```haskell
phase :: Expr1 -> Either Error Expr2
```

. . .

Unfortunately, the `Either` monad is short-circuiting.

## Short-circuiting Either

What does this mean in practice?

Imagine we have some compiler phase which transforms the AST:

```haskell
phase :: Expr1 -> Either Error Expr2
phase (IntE x)
    | x > fromIntegral (maxBound :: Int16) =
        Left "Sorry this is an old-school DSL"
    | otherwise =
        Int16E (fromIntegral x)
```

## Short-circuiting Either

If the LHS of `AndE1` has some error, we won't check RHS:

```haskell
phase (AndE1 x1 y1) = do
    x2 <- phase x1
    y2 <- phase y1
    return (AndE2 x2 y2)
```

## Short-circuiting Either

Can we collect more errors if something goes wrong?

```haskell
phase :: Expr -> Either [Error] Expr2
phase (AndE1 x1 y1) = do
    x2 <- phase x1 `catchError`
        (\e -> case phase y1 of
            Left err -> Left [e, err]
            Right _  -> Left [e])
    y2 <- phase y1
    return (AndE2 x2 y2)
```

This is not a very clean approach.

## Short-circuiting Either

Let's look at this piece of code again:

```haskell
phase (AndE1 x1 y1) = do
    x2 <- phase x1
    y2 <- phase y1
    return (AndE2 x2 y2)
```

. . .

**Key insight**: there is no _data dependency_ between `phase x1` and
`phase y1`.

## Short-circuiting Either

This is like parallelism!

    x1    y1

    |     |
    v     v

    x2    y2

    |
    v

    x3

- We can do `x1 -> x2` in parallel to `y1 -> y2`;
- But we can't do `x1 -> x2` in parallel to `x2 -> x3`.

## Short-circuiting Either

Much has already been written about the differences between `Applicative` and
`Monad` but this keeps popping up.

_"No data dependency"_ screams _"I want to be Applicative!"_.

## Short-circuiting Either

```haskell
data Validation e a
  = Failure e
  | Success a
```

. . .

```haskell
instance Monoid e => Applicative e where
  pure x = Success x

  Success f  <*> Success x  = Success (f x)
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Success e2 = Failure e2
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
```

## Short-circuiting Either

```haskell
phase (AndE1 x1 y1) = do
    x2 <- phase x1
    y2 <- phase y1
    return (AndE2 x2 y2)
```

## Short-circuiting Either

```haskell
{-# LANGUAGE ApplicativeDo #-}

phase (AndE1 x1 y1) = do
    x2 <- phase x1
    y2 <- phase y1
    pure (AndE2 x2 y2)
```

If there's a data dependency, GHC will tell you.

## An alternative solution

- `Validation` is great but it's not a catch-all
- What if there is _some implicit_ data dependency?


## Sometimes it's hard to use Validation

    STRING x = 666;
    INT y = "Hello world";
    STRING u = "I dont think this will pars
    STRING z = x;

. . .

But:

```haskell
compile expr1 = do
    expr2 <- parse expr1
    expr3 <- rename expr2
    expr4 <- typeCheck expr3
    ...
```

## Sometimes it's hard to use Validation

    STRING x = 666;
    INT y = "Hello world";
    STRING u = "I dont think this will pars
    STRING z = x;

- `Validation` works well for _"static"_ parallelism (we know the data
  dependencies ahead of time)
- But it is very hard to use for _"dynamic"_ parallelism (the data
  dependencies depend on user input)

## Laugh in the face of adversity

```haskell
type M a = Writer [Error] a
```

. . .

```haskell
phase :: Expr1 -> M Expr2
phase (IntE x)
    | x > fromIntegral (maxBound :: Int16) = do
        tell ["Sorry this is an old-school DSL"]
        doWhatNow
    | otherwise =
        Int16E (fromIntegral x)
```

## Laugh in the face of adversity

```haskell
data Expr2
  = AndE2 Expr Expr
  | Int16E Int16
  ...
  | ErrorE Error
```

. . .

```haskell
phase :: Expr1 -> M Expr2
phase (IntE x)
    | x > fromIntegral (maxBound :: Int16) = do
        let err = "Sorry this is an old-school DSL"
        tell err
        return (ErrorE err)
    | otherwise = return $ Int16E (fromIntegral x)
```

## Laugh in the face of adversity

Why do we keep the errors in a list _in addition_ to storing them in nodes?

We want to display all errors only once.

Further phases could:

- Move code around;
- Duplicate code;
- Remove dead code;
- ...

This could lead to errors being either duplicated or ignored.

## Laugh in the face of adversity

We could consider having:

```haskell
data Expr2
  = AndE2 Expr Expr
  | Int16E Int16
  ...
  | ErrorE
```

But keeping the `Error` in there lets us do case analysis for suggestions.

## Laugh in the face of adversity

Disadvantage: we need to take errors into account in every compiler phase now.

```haskell
rename (ErrorE err) = return (ErrorE err)

infer (ErrorE err) =
    freshType  -- ∀a. a
```

## Conclusions

Should I use `Validation`, `ErrorE`, a combination of both or something else
entirely?

. . .

It depends!

. . .

Personal opinion: isolate your phases sufficiently using an appropriate module
hierarchy.  Use whatever fits most naturally with the code for the phase.

# Representing errors

## Which is the best representation?

. . .

```haskell
type Error = String
```

. . .

```haskell
type Error = PP.Doc
```

. . .

```haskell
data Error
    = ParseError Line Column String
    | UnificationError Type Type
    | ...
```

. . .

```haskell
data Error = Error
    { errHeader  :: PP.Doc
    , errKind    :: String
    , errSummary :: PP.Doc
    , errHints   :: [PP.Doc]
    }
```

## Which is the best representation?

Within a subsystem (e.g. typechecker):

```haskell
data Error
    = UnificationError Type Type
    | TypeArityMismatch Type Int
    | ...
```

This allows us to `case`-analyse and give hints appropriate to the context

## Which is the best representation?

At subsystem boundary, these are converted to:

```haskell
data Error = Error
    { errHeader  :: PP.Doc
    , errKind    :: String
    , errSummary :: PP.Doc
    , errHints   :: [PP.Doc]
    }
```

Other subsystems shouldn't care about the contents of the error (loose coupling)

This allows easily adding something like `--error-format=json`

# Improving parse errors

## Parse errors

In my experience, parse errors are the hardest to get right

. . .

Providing good errors usually requires you to look at the _context_ in which the
error appeared.  When you are parsing, there is very little _context_ available.

## On parser generators

Parser generators generally generate low-quality error messages

. . .

This is annoying because parser generators help you prevent other issues
such as ambiguous grammars

. . .

It also doesn't seem _impossible_ to create a parser generator that generates
great error message but it just hasn't been done yet (at least for Haskell)

## On parser generators

My personal recommendation would be to have _both_ a parser generated from a
grammar _and_ a hand-written parser

. . .

One can be used to indicate problems in the grammar, and the other can be used
for error messages

. . .

Of course, people usually have finite resources so you'd need to pick one of the
two depending on priorities

## On parser generators

This also (mostly) applies to megaparsec and trifecta.

We'll assume that you are already using `<?>` in places where it really matters.

A lot has been written about error messages and Parsec.

. . .

Example:

> `try a <|> b` considered harmful  
> <http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/>

## On backtracking

Key difference between Parsec and something like Attoparsec:

```
a <|> b
```

. . .

- With Attoparsec, we try to parse using `b` if `a` fails
- With Parsec, we try to parse using `b` if `a` fails _without_ consuming any
  input

. . .

Formulated differently: by consuming input in Parsec, we _commit to the branch_

## On backtracking

Using `try` in the right places (and _only_ the right places) will get you 80%
of the way.

. . .

Bad:

```haskell
try (keyword "type" *> parseType ...) <|>
...
```

. . .

Good:

```haskell
(try (keyword "type") *> parseType) <|>
...
```

## On lexing

By default, Parsec operates directly on a `Char` stream

. . .

But you can define a custom `Token` datatype, e.g.:

```haskell
data Token
  = TType
  | TEq
  | TVar String
  | ...
```

## On lexing

```haskell
data Token
  = TType
  | TEq
  | TVar String
  | ...
```

. . .

This is a good idea.  In all cases where I've tried this I saw:

- A speedup
- Better error messages by default:  
  `unexpected 't'` vs. `unexpected keyword "type"`

. . .

You can generate this "lexer" if you want to, or handroll it!

## On recovery

We've already talked a bit about being able to produce multiple error messages
in the general sense.  For parsers, this is especially important.

. . .

```haskell
type Cat: {
    !#?^@
}

type Doc: {
    age: int
}
```

A simple parser fails at `Cat` and gives up, but that is very sad for the poor
dog who's trying to be a good dog.

## On recovery

```haskell
data TopLevelDecl
    = TypeDecl TypeInfo
    | ...
    | ErrorDecl
```

. . .

```haskell
program = many1 $ recover
    -- How to construct an error
    (\err -> tellError err >> return ErrorDecl)
    -- Skip until next "thing"
    (manyTill anyToken (token TRBrace))
    -- Parser to enable recovery on
    statement
```

## On recovery

`recover` is available in Megaparsec (and can be implemented using Parsec).

. . .

However `recover` is only allowed to do anything when _some input is consumed_.
Otherwise `recover f g a <|> b` would _never_ reach `b`.

. . .

This does not cover some very easy cases!  How about when we _know_ what the
next token is?

```haskell
funStatement = do
    token TFun
    name <- variable
    token TLParen  -- This MUST occur!
    ...
```

## On recovery

```
expect :: Token -> Parser ()
expect t =
    token t <|> tellError ("expected " ++ show t)
```

. . .

```haskell
funStatement = do
    token TFun
    name <- variable
    expect TLParen  -- This MUST occur!
    ...
```

## On Applicative and Monad

Applicative-style parsers have a certain elegance because they are often
formulated in a point-free style

```haskell
statement = do
    x <- lhs
    expect TEq
    y <- expr
    return (AssignE x y)
```

. . .

```haskell
statement =
    AssignE <$> lhs <* expect TEq <*> expr
```

## On Applicative and Monad

One particular way to view this is that Applicative is in a sense context-free
and Monad is context-sensitive:

```haskell
statement = do
    x <- lhs
    expect TEq
    -- Do stuff based on x!
    case x of
        ... -> ...
```

. . .

This is usually a **BAD** idea.  Context-free languages are nice to work with.

. . .

But, it can be used to give specific error hints depending on `lhs`!  In most
cases this should not be necessary.

## Conclusions

- Parser generators have many advantages but good error messages unfortunately
  isn't one of them
- Build a good understanding of how `try` works
- Tokenize first
- Try to recover after a parse fails

# Improving renaming errors

## Improving renaming errors

There is generally only one kind of renaming error: `name "foo" not found`

. . .

```haskell
rename env (VarE v) = case M.lookup v env of
    Just x  -> return (VarE v)
    Nothing -> throwError $ "Name " ++ show v ++ " not found"
```

. . .

```haskell
rename env (VarE v) = case M.lookup v env of
    Just x  -> return (VarE v)
    Nothing -> do
        tellError $ "Name " ++ show v ++ " not found"
        return ErrorE
```

## Improving renaming errors

```haskell
import Text.EditDistance

rename env (VarE v) = case M.lookup v env of
    ...
    Nothing -> do
        let guesses = nclosest v env
        tellError $
            "Name " ++ show v ++ " not found\n" ++
            "perhaps you meant one of: " ++ show closest
        return ErrorE
```

## Improving renaming errors

```haskell
closest :: String -> M.Map String a -> [String]
closest v env =
    map fst $
    take 10 $
    takeWhile ((<= cutoff) . snd) $
    sortOn snd $
    [ (v', levensteihnDistance costs v v')
    | (v', _) <- M.toList env
    ]
  where
    cutoff = length v * factor

    costs = -- depends on language
```

## Improving renaming errors

Costs can depend on the characters:

- It's cheap to turn a character from lower to upper
- It's expensive to turn a character into an operator
- ...

. . .

This is a "fun" game of heuristics.

## Improving renaming errors

Further improvements can be made by interacting with typechecking

We can't really compute the types of things at this point without turning our
compiler into a total mess

. . .

But we can save the information for the next phase:

```haskell
data Expr
    = VarE Var
    | AddE Expr Expr
    ...
    | UnknownVarError String [Var]
```

This keeps the coupling (somewhat) loose

# Improving type errors

## Improving type errors

We'll assume some Hindley-Milner based type system

. . .

Improving type errors is tricky mainly because type system implementations can
be complicated

. . .

The main design goal is to make sure the error-generating code can _never_
change the results of the core algorithm

## Improving type errors

Example:

    {                        |   {
      firstName : String,    |     firstName : String,
      lastName : String,     |     lastName : String,
                             |
      hairColor : String,    |     hairColor : String,
      eyeColor : String,     |     eyeColor : String,
                             |
      address : String,      |     address : String,
      phoenNumber : String,  |     phoneNumber : String,
      email : String,        |     email : String,
                             |
      pets : Int             |     pets : Int
    }                        |   }

## Improving type errors

Example:

    {                        |   {
      ...                    |     ...
      phoenNumber : String,  |     phoneNumber : String,
      ...                    |     ...
    }                        |   }

. . .

We calculate the differences in the record types when unifying _but_ that code
is already complex

. . .

Better to decouple it and compute the differences again when we generate the
actual error message

## Improving type errors

Decoupling is key

. . .

The first step is to decouple constraint generation and constraint solving, and
this is already fairly common in Hindley-Milner implementations

. . .

```haskell
let x = 1 in f x
```

. . .

```
typeOf(x)   = a
a           = Int
typeOf(f x) = b
typeOf(f)   = c -> b
typeOf(x)   = c
```

## Improving type errors

Instead of generating just type constraints, let's generate some more info

```haskell
data TypeOrigin
    = InferredFromLiteral Lit
    | InferredFromVariable Var
    | ExpectedAsFunctionArgument Var Int
    ...
    | OutOfThinAir
```

```haskell
data TypeInfo = TypeInfo Type TypeOrigin
```

## Improving type errors

```haskell
data TypeInfo = TypeInfo Type TypeOrigin
```

The algorithm operates on `Type` and just tracks `TypeInfo`

. . .

What about unification?

```haskell
unify (TypeInfo t1 info1) (TypeInfo t2 info2) = do
  let t = mappend info1 info2
  ...
```

## Improving type errors

```haskell
-- Like the Max Monoid on natural numbers
instance Monoid TypeOrigin where
    mempty  = OutOfThinAir
    mappend = mostInfo
```

Alternatively, you could keep a `[TypeOrigin]` and decide which one to pick when
generating the error message.  Or you might decide to pick multiple.

## Improving type errors

This gives us roughly something like this:

    Could not unify X with Y

    X because TypeOrigin

    Y beacsue TypeOrigin

. . .

Unfortunately, it's not always obvious _why_ the compiler decides to unify
things

## Improving type errors

We can apply the same trick to the _why_ of the equations as to the _why_ of the
types

```haskell
data Constraint
    = EqConstraint TypeInfo TypeInfo EqOrigin
```

. . .

```haskell
data EqOrigin
    = ExprOrigin               Expr
    | HomogeneousArrayEqOrigin Expr
    | HeaderEqOrigin           Signature

```

## Improving type errors

    Could not unify X with Y because

    e.g. we expect the types in a list to be the same

    X because TypeOrigin

    Y because TypeOrigin

# Questions?

# Improving evaluation errors

## Improving evaluation errors

This is very, very DSL-dependent

. . .

Laziness is important:

```
foo:
    let x: error("x is a problem")
    let y: error("y is a problem")
    x + y
```

. . .

The thing that improves evaluation errors most is _stack traces_

## Improving evaluation errors

For stack traces, I usually don't care how or when an erroneous value is
_evaluated_

. . .

I care about how the erroneous value _was constructed_

. . .

This is fairly simple (but a bit slow) to add: we simply keep a reference to the
stack wherever it's relevant

. . .

```haskell
data Thunk = Thunk
  { thunkEnv   :: Env
  , thunkStack :: Stack
  , thunkExpr  :: Expr
  }
```
