# testing_dirs

These programs are a first-pass at a set of testing functions for evaluating whether source and target data are equal.  The basic idea is that a developer would have a set of source .sas7bdat datasets that were created as part of the project prep.  These source datasets can be intermediate inputs or final outputs.  Ideally, the developer will write an R program that creates identical outputs to the original SAS program.  These functions will inventory the source directory of SAS data and check to see the R data match.

# Getting Started

The actual testing functions are storied in the `test_functions.R` program.  A series of `test_that()` scripts are available in the `tests.R` program.  

## Simulating data

There is a function called `create_data()` that will simulate data and store them in **/SAS DATA** and **/R DATA** directories.  At the moment, these data are both stored as **.RDS** files for testing.  The `create_data()` function will change filenames, variable names, data frame dimensions, variable classes, and actual values, and each testing function will evaluate these in series.  Each of the `test_that()` calls will create the data required for its evaluation.

## same_files()

The `same_files()` function will test that the file names in the **/SAS Data** directory match the file names in the **/R Data** directory.  If there are files added/missing in one of these directories, it will result in an error.  It is not necessary for the file names to be case-sensitive. The function works by stripping the suffix from each file name and making a straight comparison.

## same_colnames()

Once the directory contents are evaluated as identical, the `same_colnames()` function will load each individual data file and evaluate that the names and order of the columns are the same.  Column names in source and target datasets must be identical and are case-sensitive.

## same_dims()

The next test is to check the dimensions of each dataset.  The `same_dims()` function will load each dataset and run a `dim()` function on the dataframes and compare.  Source and target datasets must be identical in order to pass the test.

## same_class()

SAS data can only be coded as numeric or character types, so this makes comparing variable classes fairly straightforward.  Object types are evaluated using the `class()` function which should flag mismatched variable types.  This may cause problems with some more specialized variable types like `datetime` or other SAS formats.  This function may need revisiting once we have some real .sas7bdat data to work with.  

## same_values()

The `same_values()` function evaluates data equality using the `base::all.equal()` function with a tolerance of 0.001.  If run by itself, it should flag most of the problems identified with the above functions as well.  If there are any issues with the data, it will produce a dataframe listing the mismatches between source and target datasets.




R data that are coded as `integer` or `double` will both be returned as class = "numeric".  