# Ford Documentation

These are control files for the [Ford documentation generator](https://github.com/Fortran-FOSS-Programmers/ford). MAPL currently has
two:

1. `docs-with-remote-esmf.md` - This is the main documentation file.  It
   includes the documentation for MAPL and ESMF.  It is used to generate the
   [MAPL documentation](https://geos-esm.github.io/MAPL-docs/).
2. `docs-with-remote-esmf.public_private_protected.md` - This generates the "developer" version of the MAPL documentation.  It includes
   the documentation for MAPL and ESMF, but also includes the private and
   protected members of the MAPL classes.  It is used to generate the
   [developer version of the MAPL documentation](https://geos-esm.github.io/MAPL-docs/dev-doc/).

## Issue with `pcpp`

Note that currently MAPL does not work with `pcpp` which is the default
preprocessor for Ford 7.  Instead, we must use `cpp` with the `-traditional-cpp`
flag.  This is done by setting the `preprocessor:` in the Ford markdown files.

```markdown
preprocessor: cpp -traditional-cpp -E
```

### cpp on macOS

Note that on macOS, `cpp` is by default clang's preprocessor.  To use the GNU
preprocessor, you must install it with Homebrew and then use the full path to
the executable, e.g.,

```markdown
preprocessor: /opt/homebrew/bin/cpp-13 -traditional-cpp -E
```

