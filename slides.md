% Haskell
% Dhananjay and Rémi
% October 19, 2016


# Haskell - Where, when, who?

- Named after *Haskell Curry* (logician)
- 1987: *>= 12 non-strict*, purely functional programming languages existed
- 1987 bis: Committee should be formed to define an open standard
- Purpose: Consolidate the existing FPL into a common one, basis for future research
- 1990: First version of Haskell

-------------------------------------------------------------------------------

# What is Haskell

- Haskell is **lazy**
- Haskell is **pure**
- Haskell is **functional**
- Haskell has a (very expressive) **strong, static type system**
- Haskell is *fast* (enough)
- Haskell is *multi-purpose*

-------------------------------------------------------------------------------

# So, why Haskell?

- Strong type safety, less errors
- Purity, less errors...
- Refactoring is a no-brainer
- Easy parallelization
- Mature language, mature ecosystem
- Type system actually helps you reason about your program (TDD)

-------------------------------------------------------------------------------

# Haskell and other languages

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

## Pure and functional?

- Data is immutable
- Everything is basically a function

### Consequences

- Side effects??
- No loop
- No mutable data structures

-------------------------------------------------------------------------------

Sure, we can have side effects...

```haskell
main = putStrLn "Hello World"
```

-------------------------------------------------------------------------------

```haskell
main :: IO ()
main = putStrLn "Hello World"
```

Could be read as:

```c
void main() {
    printf("Hello World\n");
}
```

But much better!

-------------------------------------------------------------------------------

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

...But you can specify them, mostly to check your understanding of the program!

-------------------------------------------------------------------------------

```haskell
```

### Consequences

- Less bugs
- Documentation can be wrong, types cannot (but write both!)
- Data convertions are explicit

-------------------------------------------------------------------------------

## Lazy?

- Nothing is evaluated unless it's needed

### Consequences

- More modularity
- Easier code reuse
- ?

-------------------------------------------------------------------------------

### Let's talk about types

And let's find lot of examples maybe?
Less boring, and more visual

-------------------------------------------------------------------------------

## Example 1: Diagrams

- Declarative domain-specific language (*DSL*)
- *Creating vector graphics*

![sierpinski](./demos/image.png){ width=150px }

-------------------------------------------------------------------------------

```haskell
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

sierpinski :: Integer -> Diagram B
sierpinski 1 = triangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
    where s = sierpinski (n - 1)

main = mainWith (sierpinski 10)
```

- *Composition*
- *Custom operators*

-------------------------------------------------------------------------------

## Example 2: Fibonacci

- $F_0 = 0$
- $F_1 = 1$
- $F_n = F_{n-1} + F_{n-2}$

0, 1, 1, 2, 3, 5, 8, 13

```haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame0.png){ width=150px }

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame1.png){ width=150px }

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame2.png){ width=150px }

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame3.png){ width=150px }

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame4.png){ width=150px }

-------------------------------------------------------------------------------

![Fibonacci](./demos/frame5.png){ width=150px }

-------------------------------------------------------------------------------

- Lazyness
- Purity

-------------------------------------------------------------------------------

# Resources to learn Haskell

-------------------------------------------------------------------------------

# The working group
