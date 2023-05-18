# fortran-vars upgrade guide
For changes localized to fortran-src, see `upgrade-guide.md` in [fortran-src's
repository](https://github.com/camfort/fortran-src).

## Release 0.4.0
### Using fortran-src 0.15.0
***Necessitates changes.***

The primary change is to how array dimensions in `SemanticType` (the Fortran
type representation in fortran-vars) are represented. See the relevant subheader
for details.

### Extended dimensions representation
fortran-vars previously represented array dimensions with a type synonym:

```haskell
type Dims = [(Int, Int)]
```

fortran-src now provides TODO

```
data Dims t a
  -- | Explicit-shape array. All dimensions are known.
  = DimsExplicitShape
      (t (Dim a)) -- ^ list of all dimensions

  -- | Assumed-size array. The final dimension has no upper bound (it is
  --   obtained from its effective argument). Earlier dimensions may be defined
  --   like explicit-shape arrays.
  | DimsAssumedSize
      (Maybe (t (Dim a))) -- ^ list of all dimensions except last
      a                   -- ^ lower bound of last dimension

  -- | Assumed-shape array. Shape is taken from effective argument. We store the
  --   lower bound for each dimension, and thus also the rank (via list length).
  | DimsAssumedShape
      (t a) -- ^ list of lower bounds
```

## Release 0.3.0
### Using fortran-src 0.8.0
***Necessitates changes.***

Handful of breaking changes here. See fortran-src's upgrade guide above.

### BOZs utils built into fortran-src
*Likely will not necessitate any changes.*

`Vars.BozConstant` no longer defines an internal `BozDecomposed` data type. It
now uses `Boz` data type in `AST.Boz`, which represents a "parsed" BOZ constant
in a similar way to `BozDecomposed`, but without explicitly parsing into fields.
Resolving to an integer is now done by calling an AST function `bozAsNatural`,
rather than explicitly processing the converted binary string. (The function
reads the BOZ string directly into a number depending on the base.)

Since `BozDecomposed` wasn't exposed, we don't expect any issues upgrading.
Notably, though, the (internal) bitstring code is removed. If it's needed, you
should be able to convert to a numeric type and use `showIntAtBase` or a `Bits`
instance.
