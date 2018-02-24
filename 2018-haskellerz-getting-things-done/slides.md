---
title: 'Getting Things Done in Haskell'
author: 'Jasper Van der Jeugt'
date: 2018-02-22
patat:
  incrementalLists: true
  wrap: true
...

# Introduction

## About me

Hi, I'm Jasper

- Got started with Haskell a while ago
- I use Haskell at work (Fugue)
- I use Haskell in my free time (open source)
- I try to be involved in the community (SoH, Zurihac)

## About this talk

The path of the Haskell noob:

1. Write a sort algorithm
2. Write "99 bottles of beer on the wall"
2. Write some unix tools like `rev`
3. ???
4. Write a web application

. . .

Getting things done™

## Haskell pyramid

        /\
       /  \
      /    \
     /      \
    /        \
    ----------   - Simple list comprehensions

## Haskell pyramid

        /\
       /  \
      /    \
     /      \
    /        \   - map, filter
    ----------   - Simple list comprehensions

## Haskell pyramid

        /\
       /  \
      /    \
     /      \    - IO, files, network
    /        \   - map, filter
    ----------   - Simple list comprehensions

## Haskell pyramid

        /\       - Zygohistomorphic prepromorphisms
       /  \
      /    \
     /      \    - IO, files, network
    /        \   - map, filter
    ----------   - Simple list comprehensions

## Haskell pyramid

What you see discussed in papers, social networks, ...:

        /\       - Zygohistomorphic prepromorphisms
       /  \
      /    \

. . .

What you need to write applications:

     /      \    - IO, files, network
    /        \   - map, filter
    ----------   - Simple list comprehensions

<https://patrickmn.com/software/the-haskell-pyramid/>

## Sample application

<http://fugacious.jaspervdj.be/>

Provides a simple 10-minute email application.

Code at <https://github.com/jaspervdj/fugacious>

. . .

Balancing exercise in:

- Doing things right™
- Keeping things simple™

## Sample application

Deployment: not trivial

1. Build a docker container with our app as entry point
2. Push this to AWS ECR
3. Create a cluster, load balancer...
4. Create a queue and topic to receive mail
5. ...

. . .

Conclusion: not really harder (or easier) than other languages, but you need to
get some things right (configuration, logging, ...).

# Haskell's module system

## Haskell's module system

Haskell's module system is one of the simplest there is:

- Modules are hierarchical
- Modules should not be cyclic (but can be)
- You can re-export module
- You can import modules as a given name

And that's about all there is to it...

## The classic organisation

If you're building a command line tool:

- Phase 1: `Main.hs`
- Phase 2: `Main.hs`, `Types.hs`
- Phase 3: `Main.hs`, `Types.hs`, `Util.hs`

. . .

Advantage: it's very clear where to put things (e.g. a type...)

. . .

Disadvantage: it doesn't scale very well...

## A better organisation

Rule of thumb: organise by _problem domain_ / _semantics_, not by kind.
Recurse.

- `Rev.Main`
- `Rev.Reverse`
- `Rev.Options`

## Smell: Types modules

I think having a `.Types` module is a Haskell antipattern.

## Smell: Types modules

Problem 1: Instances

. . .

Haskell asks you to put instances next to the corresponding types.  What if
your instances use auxiliary functions?

    Instances --(depend on)--> Functions --(depend on)--> Types

. . .

Problem!  This forces you to grow a 1000-line Types module which contains lots
of functions (or use orphans).  Neither option is great.

## Smell: Types modules

Problem 2: Ad-hoc types

. . .

Use ad-hoc datatypes liberally!

<https://jaspervdj.be/posts/2016-05-11-ad-hoc-datatypes.html>

## Smell: Types modules

Which is better?

```haskell
getMail :: T.Text -> Maybe (Either String (Either Html String))
```

. . .

```haskell
data GetMailResult
    = MailNotFound
    | MailError String
    | MailHtml  Html
    | MailPlain Text

getMail :: Text -> GetMailResult
```

## Smell: Types modules

Which is better?

```haskell
import Data.Bifunctor (first)  -- Or Control.Arrow...

example = fmap (fmap (first H.body)) (getMail "jasper")
```

. . .

```haskell
example = case getMail "jasper" of
    MailHtml html -> H.body html
    other         -> other
```

## Smell: Types modules

Ad-hoc types can simplify business logic considerably and allow you to "split
up" the problem into more parts.

. . .

Conclusion: Types belong with their problem domain

## Smell: Types modules

Problem: what about the following organisation?

    Fugacious.Mail        -- Provides type, some simple functions
    Fugacious.Mail.Parse  -- Provides parser

But I want to be able to re-export `parseMail` from `Fugacious.Mail`...

. . .

    Fugacious.Mail.Internal  -- Provides type, some simple functions
    Fugacious.Mail.Parse     -- Provides parser
    Fugacious.Mail           -- Re-exports both

## Smell: Types modules

- `Internal` modules are not a code smell
- `Internal` modules should _probably_ be exposed (but usage discouraged)
- `Internal` modules can be nested, e.g.:

        Fugacious.Mail.Internal        -- Provides type, some simple functions
        Fugacious.Mail.Parse.Internal  -- Provides parser primitives
        Fugacious.Mail.Parse           -- Provides parser
        Fugacious.Mail                 -- Re-exports both

## Smell: Utils module

Every C++ codebase contains about as many string split functions as there are
programmers in the team

. . .

The same is probably true for Haskell...

. . .

Presumably every function in `Foo.Utils` should really be in `Foo`.

## Smell: Utils module

Problems with `Utils`:

1. I don't want to import both `Foo` and `Foo.Utils`
2. I want to be able to find stuff
3. I want related functions to be close to each other

## Smell: Utils module

It's usually obvious where we want to put domain-specific functions -- we put
them right where they belong.

But how about:

- Orphan instances, e.g. `instance Binary (HashMap k v)`?
- Non-domain-specific functions, e.g. `ByteString.replace`?

## Smell: Utils module

Solution:

```haskell
module Data.ByteString.Extended
    ( module Data.ByteString  -- Important!
    , replace
    ) where

import Data.ByteString

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace = ...
```

## Smell: Utils module

Solution:

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.HashMap.Strict.Extended
    ( module Data.HashMap.Strict
    ) where

import Data.HashMap.Strict
import Data.Binary

instance (Binary k, Binary v) => Binary (HashMap k v) where
    ...
```

## Smell: Utils module

Conclusion:

- Lots of little extended modules
- Excellent source for pull requests
- Clear where everything should go: only one string split!

<https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html>

## Smell: Utils module

Bonus: `Prelude.Extended`, useful if you use certain functions all the time

. . .

```haskell
module Prelude.Extended
    ( module Prelude
    , unsafePerformIO
    ) where

import Prelude
import System.IO.Unsafe (unsafePerformIO)
```

## Naming and qualified imports

Always design for qualified imports.

    Fugacious.Mail.Parse  -- Exports 'parse', not 'parseMail'
    Fugacious.Mail        -- Re-exports 'parse', not 'parseMail'

## Naming and qualified imports

Go style guide:

> the further from its declaration that a name is used, the more descriptive the
> name must be

. . .

In `Fugacious.Mail`:

```haskell
import Fugacious.Mail.Parse
import qualified Data.Text as T

fromByteString = parse . T.decodeUtf8
```

## Naming and qualified imports

Go style guide:

> the further from its declaration that a name is used, the more descriptive the
> name must be

. . .

In `Fugacious.Web`:

```haskell
import qualified Fugacious.Mail as Mail

push body = do
    mail <- Mail.parse body
    ...
```

# Practical patterns: Handle

## Handle: organizing impure code

Sometimes our code can be nicely captured in beautiful mathematically sound
patterns (Monoid, Monad...)

. . .

But at other times, we can't do that:

- We have deadlines
- We are dealing with inherently mutable state
- We need to use some external code which doesn't behave nicely

## Handle: organizing impure code

The pattern we will talk about feels like the essence of Object Oriented
Programming:

- Encapsulating and hiding state inside objects
- Providing methods to manipulate this state rather than touching it directly
- Coupling these objects together with methods that modify their state

. . .

**NOT**: complicated object hierarchies, subtyping, abstract factories...

## Handle: disclaimers

1. This is usually appropriate for "large" services (e.g. a database) rather
   than "small" datatypes (e.g. a user).
2. Whenever you can write _pure_ code, you should write pure code.  This pattern
   is only useful for when you can't.
3. The main alternative to this approach is building complex monad transformer
   stacks with `IO` at the bottom.  This has some advantages and some
   disadvantages...

## Handle: module layout

```haskell
module MyApp.Database where

data Handle = Handle
    { hPool   :: Pool Postgres.Connection
    , hCache  :: IORef (PSQueue Int Text User)
    , hLogger :: Logger.Handle
    , ...
    }
```

## Handle: module layout

```haskell
module MyApp.Database where

data Handle = ...

createUser :: Handle -> Text -> IO User
createUser = ...

getUserMail :: Handle -> User -> IO [Mail]
getUserMail = ...
```

## Handle: design for qualified import

When designing Haskell modules, always assume that the users of this module will
import your module using `qualified`.

## Handle: design for qualified import

```haskell
import qualified MyApp.Database as Database

main = do
    ...
    user <- Database.createUser db "jasper"
    ...
```

. . .

```haskell
import MyApp.Database

main = do
    ...
    user <- databaseCreateUser db "jasper"
    ...
```

## Handle: creation

As I mentioned before, an important reason to use these best practices is that
developers become "familiar" with it.  Initializing a service typically requires
a bunch of parameters.

. . .

```haskell
data Config = Config
    { cPath :: FilePath
    , ...
    }
```

## Handle: creation

Our module will typically also provide a way to create a `Config`.  This depends
on your application:

```haskell
-- Parse a .conf file
parseConfig :: Configurator.File -> Config
parseConfig = ...
```

. . .

```haskell
-- Load from JSON/YAML
instance FromJSON Config where
    parseJSON = ...
```

## Handle: creation

What if we can load a Config from multiple places?

1. `/etc/myapp/main.conf`
2. `$HOME/.myapp.conf`
3. `$PWD/.myapp.conf`
4. Command line arguments

## Handle: creation

```haskell
data Config = Config
    { cPath :: Maybe FilePath
    }
```

## Handle: creation

```haskell
instance Monoid Config where
    mempty = Config
        { cPath = Nothing
        }
```

. . .

```haskell
    mappend c1 c2 = Config
        { cPath = case cPath c2 of
            Nothing -> c1
            Just p  -> Just p
        }
```

## Handle: creation

```haskell
data Config = Config
    { cPath :: Last FilePath
    }

instance Monoid Config where
    mempty        = Config {cPath = mempty}
    mappend c1 c2 = Config {cPath = cPath c1 <> cPath c2}
```

. . .

```haskell
let config = mconcat [etcConfig, homeConfig, pwdConfig, optsConfig]
```

## Handle: creation

```haskell
new :: Config          -- 1. Config
    -> Logger.Handle   -- 2. Dependencies
    -> IO Handle       -- 3. Result
```

## Handle: summary

```haskell
module MyApp.Database
    ( Config (..)  -- Why?
    , parseConfig  -- And/or JSON, ...

    , Handle  -- Why?
    , new

    , createUser  -- Actual operations
    , ...
    ) where
```

# Practical patterns: Handle polymorphism

## Handle: polymorphism

Being able to split interface and implementation is important

. . .

There are many ways to accomplish this in Haskell:

- Higher order functions
- Type classes / families
- Dictionary passing
- Backpack module system
- ...

## Handle: polymorphism

In Haskell, it's very easy to abstract over things -- you can abstract over
almost anything

. . .

This makes _premature_ abstraction a real concern:

1. You need to implement a bunch of things that look similar
2. You write down a typeclass to create an interface
3. You start writing the actual implementations
4. One of them doesn't quite match the interface so you need to change it two
   weeks in
5. This causes problems for the other interface
6. Go back to (4)

## Handle: polymorphism

General advice:

1. Write the actual implementations
2. _After_ you have something working, see what they have in common

## Handle: polymorphism

```haskell
module MyApp.Database where

data Handle = Handle
    { createUser :: Text -> IO User
    , ...
    }
```

What is the type of `createUser`?

. . .

```haskell
createUser :: Handle -> Text -> IO User
```

Exactly the same as before!

## Handle: polymorphism

```haskell
module MyApp.Database.Postgres where
import MyApp.Database

data Config = ...
```

. . .

```haskell
new :: Config -> Logger.Handle -> IO Handle
new = ...
```

## Handle: polymorphism

```haskell
new :: Config -> Logger.Handle -> IO Handle
new config logger = do
    connection <- ...
    return Handle
        { createUser = \text -> do
            ...
        , ...
        }
```

## Handle: polymorphism

```haskell
new :: Config -> Logger.Handle -> IO Handle
new config logger = do
    connection <- ...
    return Handle
        { createUser = createUserWith connection
        , ...
        }

createUserWith :: Postgres.Connection -> Text -> IO Handle
createUserWith = ...
```

# Practical patterns: withHandle

## Handle: closing

Is `new` enough or do we also need a `close`?

Haskell is garbage collected, but...

. . .

Often we care about closing things timely:

- Databases
- File handles
- Sockets
- Threads

## Handle: closing

Adding a close is usually fairly simple:

```haskell
close :: Handle -> IO ()
close = ...
```

## Handle: closing

```haskell
main :: IO ()
main = do
    database <- Database.new config
    doSomething database
    Database.close config
```

## Handle: closing

```haskell
main :: IO ()
main = do
    database <- Database.new config
    doSomething database  -- What happens if there's an exception?
    Database.close config
```

## Handle: closing in java

```java
try {
    Database database = new Database(config);
    doSomething(database);
} finally {
    database.close();
}
```

## Handle: closing in python

```python
with Database(config) as database:
    doSomething(database)
```

## Handle: closing in haskell

```haskell
main = do
    database <- Database.new config
    doSomething database `finally` Database.close database
```

. . .

There's good news and bad news...

- Bad news: the above code is still not quite correct
- Good news: the above code already exists as a pattern

## Handle: closing in haskell

```haskell
bracket
    :: IO a         -- Create resource
    -> (a -> IO b)  -- Close resource
    -> (a -> IO c)  -- Do something with resource
    -> IO c         -- Result
```

. . .

In our example:

. . .

```haskell
main = bracket (Database.new config) Database.close $ \database -> do
    doSomething database
```

## Handle: avoiding mistakes

```haskell
module MyApp.Database

withHandle
    :: Config            -- Configuration
    -> Logger.Handle     -- Dependencies
    -> (Handle -> IO a)  -- Code
    -> IO a              -- Result
```

. . .

```haskell
withHandle config logger = bracket (new config logger) close
```

## Handle: avoiding mistakes

```haskell
module MyApp.Database
    ( Config (..)
    , Handle
    , withHandle  -- Use .Internal for new/close
    , ...         -- Actual operations
    ) where
```

## Handle: avoiding mistakes

Functions such as `bracket` and `withHandle` are particularly interesting: in
Java and Python, these features are hardcoded into the language to a large
extent.

. . .

In Haskell, however, it's just functions that you can write yourself!

. . .

When talking about higher-order functions, we typically resort to things such as
`map` and `filter` -- but it's very important to note we can do similar things
with `IO` code!

Example: `resource-pool` package.

## Handle: resource-pool

```haskell
createPool
    :: IO a          -- ^ Action that creates a new resource.
    -> (a -> IO ())  -- ^ Action that destroys an existing resource.
    -> ...           -- ^ Some size/timeout parameters
    -> IO (Pool a)
```

. . .

Maybe Haskell really is the best imperative language!

## Handle: other approaches

- Monad transformers
- Effect handlers
- Free monads
- ...

It's simple and it works everywhere™

## Handle: other approaches

```haskell
-- | Create JSON-RPC session around conduits from transport layer.
-- When context exits session disappears.
runJsonRpcT
    :: (MonadLoggerIO m, MonadBaseControl IO m)
    => Ver                  -- ^ JSON-RPC version
    -> Bool                 -- ^ Ignore incoming requests/notifs
    -> Sink ByteString m () -- ^ Sink to send messages
    -> Source m ByteString  -- ^ Source to receive messages from
    -> JsonRpcT m a         -- ^ JSON-RPC action
    -> m a                  -- ^ Output of action
```

# Questions?
