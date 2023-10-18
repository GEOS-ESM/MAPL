`OverwriteConfig.F90` is an application that tests for a bug related to **ESMF**.

When one attribute value is set with `ESMF_ConfigSetAttribute`,
the next attribute is not found by a call to `ESMF_ConfigFindLabel`.

`OverwriteConfig.rc` is a test file to run the application.
`OverwriteConfig_FullTest.F90` is a more comprehensive version of `OverwriteConfig.F90`.
