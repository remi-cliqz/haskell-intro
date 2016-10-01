% Haskell
% Dhananjay and Rémi
% October 5, 2016

# Haskell

# What is Haskell

- Haskell is **lazy**
- Haskell is **pure**
- Haskell is **functional**
- Haskell has a (very expressive) **strong, static type system**
- Haskell is *fast* (enough)
- Haskell is *multi-purpose*


# Haskell and other languages


# Haskell ecosystem

- Glorious/Glasgow Haskell Compiler (*GHC*)
- Stack: cli tool to:
    - Install GHC for you
    - Install packages
    - Build Haskell projects
    - Test Haskell projects
    - etc.
- Stackage/Hackage: central package archive for Haskell.
- Hoogle: Haskell code search engine

# Let's try it out

## Free fall option

```sh
curl -sSL https://get.haskellstack.org/ | sh
```

## On Ubuntu

```sh
apt-get install haskell-stack
```

## On Mac OS

```sh
brew install haskell-stack
```

## Then...

-------------------

```sh
$ stack ghci
```
```haskell
Prelude> print "Hello World!"
"Hello World!"
```


---------------

## Pure and functional?

- Data is immutable
- Everything is basically a function

### Consequences

----------------

## Static strong typing

- All types are known at compile-type
- Compiler is able to *infer* types

```haskell
> :t [1, 2, 3, 4]
[1, 2, 3, 4] :: Num t => [t]
> :t 42
42 :: Num a => a
> :t "Hello World!"
"Hello World!" :: [Char]
```

... But you can specify them, mostly to check your understanding of the program!

### Consequences

- Less bugs
- Documentation can be wrong, types cannot (but write both!)
- Data convertions are explicit


----------------------

## Lazy?

- Nothing is evaluated unless it's needed

### Consequences

- More modularity
- Easier code reuse
- ?


# Why it can make sens to learn Haskell

- Improve thinking and programming skills
- Completely different paradigm
- Less type debugging/testing


### Let's talk about types

And let's find lot of examples maybe?
Less boring, and more visual

# Resources to learn Haskell
# The working group
