# Data Tests

This document outlines the testing structure for the Choctawhatchee Basin Alliance's Water Quality Dashboard. The tests are located in the `/tests/testthat/` directory of the repository.

## Overview

The wq-dashboard is an R Shiny application that follows standard R package testing conventions using the `testthat` framework. Tests are organized in the `tests/testthat/` directory with test files that follow the naming pattern `test-*.R`.

Tests are currently setup for data used by the dashboard and are meant to ensure any updates to the data include relevant and correct information.  Any test failures will notify the maintainer by email for immediate attention.  
Tests are run any time updates are made to the main GitHub repository using GitHub Actions.  Test runs can be viewed by clicking the badge on the README or accessing <https://github.com/choctawhatchee-basin-alliance/wq-dashboard/actions>.

## Test Directory Structure

```
tests/
├── testthat.R           # Main test runner file
└── testthat/            # Directory containing individual test files
    ├── test-*.R         # Individual test files
    └── helper-*.R       # Helper file
```

## Test Files

Each file evaluates conditions and characteristics of the following files in the `data` folder:

* `alldat.RData` - Contains all water quality data for both CBA and LakeWatch
* `cbawbid.RData` - Contains waterbody identifiers (WBIDs) for CBA sampling locations
* `cntdat.RData` - Contains continuous water quality data
* `meta.RData` - Metadata file for `alldat`, no testing is needed
* `nncdat.RData` - Contains numeric nutrient criteria (NNC) for relevant water quality parameters and WBIDs
* `stas.RData` - Contains station information
* `raindat.RData` - Contains rainfall data at three locations
* `rainstas.RData` - Contains rainfall station locations

Test files in `tests/testthat` evaluate the following:

1. `test-checkcbastations.R` - Tests all waterbody, station combinations for CBA in `alldat` are found in `stas`
1. `test-checkcntdatstations.R` - Tests  all waterbody, station combinations in `cntdat` are found in `stas`
1. `test-checkcntdattimestep.R` - All dates in the `timestamp` column are ascending for each waterbody, station combination
1. `test-checkcounties.R` - All counties in `alldat` are identifiable
1. `test-checkdates.R` - Dates in `alldat` are not prior to 1992-11-08 or after the current date
1. `test-checklkwstations.R` - Tests all waterbody, station combinations for LakeWatch in `alldat` are found in `stas`
1. `test-checklocations.R` - Verifies all lat/lon entries in `stas` are within an appropriate bounding box polygon for the region
1. `test-checknncdat.R` - Verifies stations and waterbodies in `alldat` are in `nncdat` and all WBIDs in `nncdat` are in `cbawbid`
1. `test-checkparameters.R` - Parameters in `alldat` are included in `meta`
1. `test-checkraindata.R` - Checks all rainfall data less than or greater than zero inches, dates greater than or equal to 1990-01-01 and less than or equal to system date.
1. `test-checkrainwbid.R` - All WBIDs in `rainstas` are identifiable, WBIDs in `raindat` are present in `rainwbid`
1. `test-checkrainstations.R` - Verifies station names are shared between `raindat` and `rainstas`
1. `test-checksamplelocations.R` - Verifies sample location in `alldat` is provided as `surf` or `bott`, no missing entries
1. `test-checkwaterbodies.R` - All waterbodies in `alldat` are identifiable
1. `test-checkwbid.R` - All WBIDs in `stas` are identifiable, WBIDs in `alldat` are present in `cbawbid`

Please consult the file `tests/testthat/helper-data.R` for entries that are used for verifying correct values for the data. 

## Running Tests

Tests can be run manually using:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-specific-file.R")
```