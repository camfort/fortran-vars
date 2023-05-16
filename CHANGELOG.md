## 0.4.0 (Unreleased)
  * Replace expression evaluator with fortran-src one
    * Interface changes are mostly non-breaking. Some behaviour may be slightly
      different due to INTEGER etc. constants using different types.
    * Old evaluator remains available at
      `Language.Fortran.Vars.Eval.Deprecated` (and still gets used by constant
      propagation code).
    * Short-circuiting evaluator uses the old evaluator. (This is feasible for
      the fortran-src evaluator, and is pending work.)
  * Use more precise array dimensions encoding
    * Breaking change.
  * Update to fortran-src 0.15

## 0.3.1 (22 Aug 2022)
  * Update to fortran-src 0.10.2
  * Do some type checking for logical operators used with non LOGICAL arguments
    #6
  * Add type checking for `imag`, `lshift` intrinsics
  * MemoryLocation: change `getLocation` to return a `Maybe` type
  * Memory: expose `getTypeSize` function

## 0.3.0 (10 Jan 2022)
  * Update to fortran-src 0.8.0
  * Replace BozDecomposed with new Boz type in fortran-src
    * Due to how the BozConstant module was used, this should have minimal
      impact. Code that reads and writes BOZs into `ExpVal`s directly may need
      updating.

## 0.2.0 (24 Nov 2021)
  * Update to fortran-src 0.6.0
  * Gather type info from COMMON blocks better (as they now support dimension
    declarators) #1
  * Fix some disabled tests #1

## 0.1.0 (6 Sep 2021)
Initial release.

### Major changes from original package
  * Using public Stack resolver.
  * Some of the underlying types are migrated to fortran-src 0.5.0.
