# fortran-vars upgrade guide
For changes localized to fortran-src, see `upgrade-guide.md` in [fortran-src's
repository](https://github.com/camfort/fortran-src).

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
