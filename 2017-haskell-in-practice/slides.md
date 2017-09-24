---
title: 'Haskell in practice'
author: 'Jasper Van der Jeugt'
date: 2017-09-26
patat:
  incrementalLists: true
  wrap: true
...

# Introduction

## About me

Started using Haskell at university for pretty much everything

Have been working almost exclusively in Haskell for my entire career

- Tsuru Capital (Algorithmic trading)
- Erudify/Better (E-learning)
- Bdellium (Retirement planning)
- Fugue (Cloud orchestration)

## About Haskell

I subjectively think Haskell is _the best_ general programming language
currently in existence...

. . .

... but let's try not to generalize things too much:

> I suppose it is tempting, if the only tool you have is a hammer, to treat
> everything as if it were a nail.
>
> -- Abraham Maslow

## Why Haskell

- Haskell is _fast_
- Haskell is _safe_

# Practical patterns: Monoid

## What is a Monoid?

```haskell
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m  -- Also `<>`
```

## Monoid laws

TODO: Laws

## Simple examples

- Sums

    ```haskell
    mempty  = 0
    mappend = (+)
    ```

- Products

    ```haskell
    mempty  = 1
    mappend = (*)
    ```

## Simple examples

- Strings/text/...

    ```haskell
    mempty  = ""
    mappend = (++)
    ```

- Lists

    ```haskell
    mempty  = []
    mappend = (++)
    ```

## Further examples

- Configurations

    The user can configure our application using command line flags,
    `$HOME/.app.conf`, `/etc/app/main.conf`...

- Builders

    This is a specialized version of general strings/list Monoids.

We will talk about both of these Monoids later in this talk.

## Why are monoids useful?

```haskell
x = "Hello, " ++ "world!"
```

vs.

```haskell
x = "Hello, " <> "world!"
```

## Why are monoids useful?

- Writing polymorphic code:

    ```
    foldMap :: Monoid m => (a -> m) -> [a] -> m
    ```

    . . .

    Also knows as MapReduce!

- Reasoning about code: from the laws, you can deduce other properties, which
  can be used for optimizations

- You become "familiar" with Monoids

. . .

This actually applies to most typeclasses

# TODO: detour about IO monad?

# Practical patterns: Builder Monoid

## Appending strings

```haskell
unlines :: [String] -> String
unlines []       = "\n"
unlines (x : xs) = x ++ "\n" ++ unlines xs
```

## Appending strings

`String` is just a simple linked list.

`Text` is a buffer-based string representation.

```haskell
unlines :: [Text] -> Text
unlines []       = "\n"
unlines (x : xs) = x <> "\n" <> unlines xs
```

## Appending strings

This is not a Haskell-specific problem:

```c
int xlen = strlen(x);
int ylen = strlen(y);
char *z = malloc(xlen + ylen);
memcpy(z, x, xlen);
memcpy(z + xlen, y, ylen);
```

## Appending strings: StringBuilder

Solutions in other languages often use mutable structures:

```java
StringBuilder b = new StringBuilder();
b.append(x);
b.append(y);
String z = b.toString();
```

But that's not really possible if we want a nice, clean Monoid interface...

## Appending strings: Haskell Builder?

```haskell
data Builder = Builder
    -- Maximum number of bytes we're going to write
    Int
    -- A function that takes a byte pointer, starts writing at that pointer, and
    -- returns a pointer to the /next/ unwritten byte pointer.
    --
    -- In other words, it takes the "start" pointer and returns the "end"
    -- pointer.
    (Ptr Word8 -> IO (Ptr Word8))
```

## Appending strings: Haskell Builder?

```haskell
newline :: Builder
newline = Builder 1 $ \ptr -> do
    poke ptr 10  -- '\n' is 10 in ASCII (and UTF-8)
    return (ptr `plusPtr` 1)
```

## Appending strings: UTF-8

```haskell
-- UTF-8 encoding
char :: Char -> Builder
char c
    -- Simple ASCII range, character is encoded as one byte
    | x <= 0x7F = Builder 1 $ \ptr -> do
        poke ptr (fromIntegral x)
        return (ptr `plusPtr` 1)

    | ...
  where
    x = ord c :: Int
```

## Appending strings: UTF-8

```haskell
char :: Char -> Builder
char c
    | ...

    -- Character is encoded as two bytes
    | x <= 0x07FF = Builder 2 $ \ptr -> do
        let b0 = fromIntegral $ (x `shiftR` 6) + 0xC0 :: Word8
            b1 = fromIntegral $ (x .&. 0x3F)   + 0x80 :: Word8

        poke ptr               b0
        poke (ptr `plusPtr` 1) b1
        return (ptr `plusPtr` 2)

    | ...
  where
    x = ord c :: Int
```

## Appending strings: Builder Monoid?

```haskell
instance Monoid Builder where
    mempty  = ...
    mappend = ...
```

## Appending strings: Builder Monoid?

```haskell
data Builder = Builder
    -- Maximum number of bytes we're going to write
    Int
    -- A function that takes a byte pointer, starts writing at that pointer, and
    -- returns a pointer to the /next/ unwritten byte pointer.
    --
    -- In other words, it takes the "start" pointer and returns the "end"
    -- pointer.
    (Ptr Word8 -> IO (Ptr Word8))
```

## Appending strings: Builder Monoid?

```haskell
instance Monoid Builder where
```

. . .

```haskell
    mempty = Builder 0 (\ptr -> return ptr)
```

. . .

```haskell
    mappend (Builder x f) (Builder y g) = Builder
        (x + y)
        (\ptr0 -> do
            ptr1 <- f ptr0
            g ptr1)
```

## Appending strings: Builder Monoid?

```haskell
instance Monoid Builder where
    ...

string :: String -> Builder
string = mconcat . map char
```

## Appending strings: Conclusion

- Performance: on par with e.g. Java
- Implementation:
    * Haskell: low-level byte poking
    * Java: low-level byte poking
- Interface:
    * Haskell: beautifully immutable monoid interface
    * Java: crying yourself to sleep after spending an entire day fixing another
      issue where you accidentally mutated the builder in a different module

