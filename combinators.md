Name         | #     | [Haskell][]    | [Ramda][]  | [Sanctuary][]     | Signature
------------:|-------|----------------|------------|-------------------|----------
identity     | **I** | `id`           | `identity` | `I`               | `a → a`
constant     | **K** | `const`        | `always`   | `K`               | `a → b → a`
apply        | **A** | `($)`          | `call`     | `A`               | `(a → b) → a → b`
thrush       | **T** | `(&)`          | `applyTo`  | `T`               | `a → (a → b) → b`
duplication  | **W** | `join`²        | `unnest`²  | `join`²           | `(a → a → b) → a → b`
flip         | **C** | `flip`         | `flip`     | `flip`            | `(a → b → c) → b → a → c`
compose      | **B** | `(.)`, `fmap`² | `map`²     | `compose`, `map`² | `(b → c) → (a → b) → a → c`
substitution | **S** | `ap`²          | `ap`²      | `ap`²             | `(a → b → c) → (a → b) → a → c`
psi          | **P** | `on`           |            | `on`              | `(b → b → c) → (a → b) → a → a → c`
fix-point¹   | **Y** | `fix`          |            |                   | `(a → a) → a`

-----

¹) In JavaScript and other non-lazy languages, it is impossible to implement the
  Y-combinator. Instead a variant known as the *applicative* or *strict*
  fix-point combinator is implemented. This variant is sometimes rererred to as
  the Z-combinator.

²) Algebras like `ap` have different implementations for different types.
  They work like Function combinators only for Function inputs.

[Haskell]: https://www.haskell.org/
[Ramda]: http://ramdajs.com/
[Sanctuary]: http://sanctuary.js.org/#combinator