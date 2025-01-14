# Haskell JSON Parser

> A JSON parser written in Haskell.

## Learned

- `data`: Declares a new data type.
- `newtype`: Wraps an existing type (commonly used with a `run____` function).
- `<$>` / `fmap`: "Penetrates" a function into a functor. A.k.a. the **endofunctor** in a monad.
- `<*>` / Applicative: "Double-penetrates" a function into a functor.
    - `<*` / `*>`: Chooses a side, but runs both sides.
- `<|>` / Alternative: Chooses the first non-empty type from two alternatives.
- `traverse`: Maps a function over a structure and collects the results.
- `span`: Takes a predicate and returns the longest prefix of the input that satisfies the predicate.
- `many`: Applies a function until failure, then collects the results.
- `pure` / `return`: Lifts a value into an Applicative functor.

## Works Cited

"JSON Parser 100% From Scratch in Haskell." *YouTube*, uploaded by Tsoding, 21 Nov. 2019, www.youtube.com/watch?v=N9RUqGYuGfw.
JSON Generator, json-generator.com/#. Accessed 13 Jan. 2025.
>! By coincidence, generated my name on line 234 :)
