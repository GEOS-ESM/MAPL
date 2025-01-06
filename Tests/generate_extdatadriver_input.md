# Introduction
This is a simple utility to generate inputs for ExtDataDriver.x so that ExtData can simulate a real GEOS run. It requires 3 things that are passed in
- A list of items ExtData needs to fill. This can be found by looking at the GEOS log
- The import spec of the GCM component from using the printspec option to GEOS
- all the needed yaml files in a directory, name of directory is passed

This will generate the import and optionally the export (as well as History) defition for ExtDataDriver.x to spare the human the tedious work.

# Example Inputs
To get the list of ExtDate items, just grab all the lines that look like this to a file:
```
        EXTDATA: INFO: ---- 00001: BC_AIRCRAFT
        EXTDATA: INFO: ---- 00002: BC_ANTEBC1
        EXTDATA: INFO: ---- 00003: BC_ANTEBC2
        EXTDATA: INFO: ---- 00004: BC_AVIATION_CDS
        EXTDATA: INFO: ---- 00005: BC_AVIATION_CRS
        EXTDATA: INFO: ---- 00006: BC_AVIATION_LTO
        EXTDATA: INFO: ---- 00007: BC_BIOFUEL
        EXTDATA: INFO: ---- 00008: BC_BIOMASS
        EXTDATA: INFO: ---- 00009: BC_SHIP
        EXTDATA: INFO: ---- 00010: BRC_AIRCRAFT
        EXTDATA: INFO: ---- 00011: BRC_ANTEBRC1
        EXTDATA: INFO: ---- 00012: BRC_ANTEBRC2
        EXTDATA: INFO: ---- 00013: BRC_AVIATION_CDS
        EXTDATA: INFO: ---- 00014: BRC_AVIATION_CRS
        EXTDATA: INFO: ---- 00015: BRC_AVIATION_LTO
        EXTDATA: INFO: ---- 00016: BRC_BIOFUEL
        EXTDATA: INFO: ---- 00017: BRC_BIOMASS
        EXTDATA: INFO: ---- 00018: BRC_SHIP
        EXTDATA: INFO: ---- 00019: BRC_TERPENE
```

To get the GCM component spec, run with `PRINTSPEC: 1` in the `CAP.rc` and copy lines out that look like this:
```
 #IMPORT spec for GCM
 #COMPONENT, SHORT_NAME, LONG_NAME, UNIT, DIMS, CONTAINER_TYPE
        GENERIC: INFO: GCM, WSUB_CLIM, stdev in vertical velocity, m s-1,   3, esmf_field
        GENERIC: INFO: GCM, MEGAN_ORVC, MEGAN_ORVC, kgC/m2/s,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_CROP, CLM4_PFT_CROP, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_C4_GRSS, CLM4_PFT_C4_GRSS, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_C3_NARC_GRSS, CLM4_PFT_C3_NARC_GRSS, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_C3_ARCT_GRSS, CLM4_PFT_C3_ARCT_GRSS, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_DECD_BORL_SHRB, CLM4_PFT_BDLF_DECD_BORL_SHRB, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_DECD_TMPT_SHRB, CLM4_PFT_BDLF_DECD_TMPT_SHRB, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_EVGN_SHRB, CLM4_PFT_BDLF_EVGN_SHRB, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_DECD_BORL_TREE, CLM4_PFT_BDLF_DECD_BORL_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_DECD_TMPT_TREE, CLM4_PFT_BDLF_DECD_TMPT_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_DECD_TROP_TREE, CLM4_PFT_BDLF_DECD_TROP_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_EVGN_TMPT_TREE, CLM4_PFT_BDLF_EVGN_TMPT_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_BDLF_EVGN_TROP_TREE, CLM4_PFT_BDLF_EVGN_TROP_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_NDLF_DECD_BORL_TREE, CLM4_PFT_NDLF_DECD_BORL_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_NDLF_EVGN_BORL_TREE, CLM4_PFT_NDLF_EVGN_BORL_TREE, 1,   2, esmf_field
        GENERIC: INFO: GCM, CLM4_PFT_NDLF_EVGN_TMPT_TREE, CLM4_PFT_NDLF_EVGN_TMPT_TREE, 1,   2, esmf_field
```

Finally just grab the right yaml files for ExtData.

To run you will of course need to do some further editing of the produced files and link in the acutal data using the same convention `gcm_run.j` does.
