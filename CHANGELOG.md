## 0.3.0 (10 Jan 2021)
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
