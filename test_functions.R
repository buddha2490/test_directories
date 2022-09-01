


# The following tests are sourced, in this order
#  0.  create_data() - simulates some data for testing
#  1.  same_files() - check to make sure the same files are in both directories
#  2.  same_colnames() - checks to make sure the column names of the datasets in both directories match
#  3.  same_dims()  - checks the [nrow, ncol] of each dataset
#  4.  same_class() - checks the variable class of each table column, only numeric/character
#  5.  same_values() - checks the actual values of the data in each table


# Create some test data based on different failures
create_data <- function(N = 5,
                        rows = 10000,
                        change_filename = FALSE,
                        change_colname = FALSE,
                        change_dims = FALSE,
                        change_class = FALSE,
                        change_values = FALSE) {

  lst <- list()
  for (i in 1:N) {
    lst[[i]] <- data.frame(
      Letters = sample(letters, rows, replace = T),
      Double = runif(rows, 0, 100),
      Int = sample.int(rows, replace = T),
      Dollars = scales::label_dollar(big.mark = ",", decimal.mark = ".")(runif(rows, 0, 100))
    )
  }
  labs <- paste0("Test Data ", 1:N, ".RDS")
  names(lst) <- labs
  R <- lst
  SAS <- lst


  changeDF <- sample(1:N, 1)
  changeCol <- sample(1:4, 1)

  # change filenames)
  if (change_filename == T) names(SAS)[changeDF] <- "Changed filename.RDS"
  if (change_colname == T)  {
    names(R[[changeDF]])[changeCol] <- "ChangedColNameR"
    names(SAS[[changeDF]])[changeCol] <- "ChangedColNameSAS"
  }
  if (change_dims == T) {
    R[[changeDF]] <- R[[changeDF]][-sample(1:1000, 1),]
  }
  if (change_class == T) {
    R[[changeDF]][[changeCol]] <- factor(R[[changeDF]][[changeCol]])
  }
  if (change_values == T) {
    R[[changeDF]]$Double <- R[[changeDF]]$Double  * sample(runif(100, -2, 2), 1)
    SAS[[changeDF]]$Int <- as.integer(SAS[[changeDF]]$Int  * sample(c(-5, -4, -3, -2, 2, 3, 4, 5), 1))
  }

  # Write to the output folders
  lapply(names(R), function(df) {
    saveRDS(R[[df]], file = file.path("R Data", df))
  })

  lapply(names(SAS), function(df) {
    saveRDS(SAS[[df]], file = file.path("SAS Data", df))
  })

  return(invisible(list(R = R, SAS = SAS)))

}


# Function to check if the SAS Data directory has the same files as the R data directory
# Each of these files can be checked
same_files <- function(dir1, dir2) {

  require(dplyr)
  require(testthat)

  act1 <- quasi_label(rlang::enquo(dir1), deparse(substitute(dir1)))
  act2 <- quasi_label(rlang::enquo(dir2), deparse(substitute(dir2)))

  # get filenames and drop the extension
  act1$filenames <- list.files(act1$val, pattern = ".RDS", ignore.case = T)
  act1$short <- sub(".RDS*", "", act1$filenames, ignore.case = T)
  act2$filenames <- list.files(act2$val, pattern = ".RDS", ignore.case = T)
  act2$short <- sub(".RDS*", "", act2$filenames, ignore.case = T)

  # Combine them into a dataframe that I can reference later
  act1$df <- data.frame(filenames = act1$filenames, short = act1$short)
  act2$df <- data.frame(filenames = act2$filenames, short = act2$short)

  test1 <- act1$short[!act1$short %in% act2$short]  # these are the files in SAS Data that are NOT in R Data
  test2 <- act2$short[!act2$short %in% act1$short]  # these are the files in R Data that are not in SAS data

  # subset the df so I know what errors to print
  act1$print <- filter(act1$df, short %in% test1)
  act2$print <- filter(act2$df, short %in% test2)

  # Run my tests
  if (length(test1) + length(test2) == 0) {
    cat("same_files() \n Filenames match in both directories \n")
    succeed()
    return(invisible(dir1))
  }
  if ((length(test1) != 0) | (length(test2) != 0)) {

    message1 <- ""
    message2 <- ""

    if (length(test1) != 0) {
      message1 <- paste0("same_files() \n The following files are in ", act1$val," but were not found in ", act2$val, ":\n")
      for (i in 1:length(act1$print$filenames)) {
        message1 <- paste0(message1, "     ", act1$print$filenames[i], "\n")
      }
    }
    if (length(test2) != 0) {
      message2 <- paste0("The following files are in ", act2$val," but were not found in ", act1$val, ":\n")
      for (i in 1:length(act2$print$filenames)) {
        message2 <- paste0(message2, "     ", act2$print$filenames[i], "\n")
      }
    }

    message <- paste(message1, message2, sep = "\n")
    cat(message)
    fail("same_files() test failed")
  }

}

# checks the column names in all files
same_colnames <- function(dir1, dir2) {

  require(dplyr)
  require(testthat)

  act1 <- quasi_label(rlang::enquo(dir1), deparse(substitute(dir1)))
  act2 <- quasi_label(rlang::enquo(dir2), deparse(substitute(dir2)))

  # Get filenames of data files
  act1$filenames <- list.files(act1$val, pattern = ".RDS", ignore.case = T)
  act2$filenames <- list.files(act2$val, pattern = ".RDS", ignore.case = T)


  # Quick check to make sure all the files are in the directories correctly
  allFiles <- c(act1$filenames, act2$filenames)
  allFiles <- sub(".RDS", "", allFiles, ignore.case = T)
  allFiles <- sub(".sas7bdat", "", allFiles, ignore.case = T)
  dups <- allFiles[duplicated(allFiles)]
  files_without_dups <- allFiles[!allFiles %in% dups]
  if (length(files_without_dups) != 0) {
    message <- paste("There are files in one/both directories that are not in the other.  Please go back and match them. \n",
                     "Please run the same_files() expectation on these directories for more information. \n")
    fail(message)
  }

  # Get the column from each output file
  act1$colnames <- lapply(act1$filenames, function(file) {
    df <- readRDS(file.path(act1$val, file))
    colnames(df)
  })
  short1 <- sub(".RDS", "", act1$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act1$colnames) <- short1

  act2$colnames <- lapply(act2$filenames, function(file) {
    df <- readRDS(file.path(act2$val, file))
    colnames(df)
  })
  short2 <- sub(".RDS", "", act2$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act2$colnames) <- short2


  testDf <- lapply(names(act2$colnames), function(obj) {
    data.frame(File = obj, Identical = identical(act1$colnames[obj], act2$colnames[obj]))
  }) %>%
    do.call("rbind", .)
  test <- identical(rep(TRUE, length(testDf$Identical)),  testDf$Identical)

  if (test == TRUE) {
    cat("same_colnames() \n","Column names match in all datasets \n")
    succeed()
    return(invisible(act1$val))
  }

  if (test == FALSE) {
    problems <- filter(testDf, Identical == FALSE)
    message <- paste("same_colnames() \n", "The column names do not match for the following datasets: \n")
    for (i in nrow(problems)) {
      message <- paste(message, problems$File[i], "\n")
    }
    cat(message)
    fail(message)
  }
}

# Checks the dimensions of each file in the directories
same_dims <- function(dir1, dir2) {

  require(dplyr)
  require(testthat)

  act1 <- quasi_label(rlang::enquo(dir1), deparse(substitute(dir1)))
  act2 <- quasi_label(rlang::enquo(dir2), deparse(substitute(dir2)))

  # Get filenames of data files
  act1$filenames <- list.files(act1$val, pattern = ".RDS", ignore.case = T)
  act2$filenames <- list.files(act2$val, pattern = ".RDS", ignore.case = T)

  # Quick check to make sure all the files are in the directories correctly
  allFiles <- c(act1$filenames, act2$filenames)
  allFiles <- sub(".RDS", "", allFiles, ignore.case = T)
  allFiles <- sub(".sas7bdat", "", allFiles, ignore.case = T)
  dups <- allFiles[duplicated(allFiles)]
  files_without_dups <- allFiles[!allFiles %in% dups]
  if (length(files_without_dups) != 0) {
    message <- paste("There are files in one/both directories that are not in the other.  Please go back and match them. \n",
                     "Please run the same_files() expectation on these directories for more information. \n")
    fail(message)
  }

  act1$dims <- lapply(act1$filenames, function(file) {
    readRDS(file.path(act1$val, file)) %>% dim()
  })
  short1 <- sub(".RDS", "", act1$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act1$dims) <- short1


  act2$dims <- lapply(act2$filenames, function(file) {
    readRDS(file.path(act2$val, file)) %>% dim()
  })
  short2 <- sub(".RDS", "", act2$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act2$dims) <- short2

  testDf <- lapply(names(act2$dims), function(obj) {
    data.frame(File = obj, Identical = identical(act1$dims[obj], act2$dims[obj]))
  }) %>%
    do.call("rbind", .)

  test <- identical(rep(TRUE, length(testDf$Identical)),  testDf$Identical)

  if (test == TRUE) {
    cat("same_dims() \n","dimensions match in all datasets \n")
    succeed()
    return(invisible(act1$val))
  }
  if (test == FALSE) {
    problems <- filter(testDf, Identical == FALSE)
    message <- paste("same_dims() \n", "The dimension names in", act1$val, "do not match for the following datasetsL: \n")
    for (i in nrow(problems)) {
      message <- paste(message, problems$File[i], "\n")
    }
    cat(message)
    fail(message)
  }
}

# Check classes (numeric or character) for each variable in the datasets
same_class <- function(dir1, dir2) {

  require(dplyr)
  require(testthat)

  act1 <- quasi_label(rlang::enquo(dir1), deparse(substitute(dir1)))
  act2 <- quasi_label(rlang::enquo(dir2), deparse(substitute(dir2)))

  # Get filenames of data files
  act1$filenames <- list.files(act1$val, pattern = ".RDS", ignore.case = T)
  act2$filenames <- list.files(act2$val, pattern = ".RDS", ignore.case = T)

  # Quick check to make sure all the files are in the directories correctly
  allFiles <- c(act1$filenames, act2$filenames)
  allFiles <- sub(".RDS", "", allFiles, ignore.case = T)
  allFiles <- sub(".sas7bdat", "", allFiles, ignore.case = T)
  dups <- allFiles[duplicated(allFiles)]
  files_without_dups <- allFiles[!allFiles %in% dups]
  if (length(files_without_dups) != 0) {
    message <- paste("There are files in one/both directories that are not in the other.  Please go back and match them. \n",
                     "Please run the same_files() expectation on these directories for more information. \n")
    fail(message)
  }

  act1$classes <- lapply(act1$filenames, function(file) {
    df <- readRDS(file.path(act1$val, file))
    lapply(colnames(df), function(col) {
      data.frame(Variable = col, Class = class(df[[col]]))
    }) %>%
      do.call("rbind", .)
  })
  short1 <- sub(".RDS", "", act1$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act1$classes) <- short1

  act2$classes <- lapply(act2$filenames, function(file) {
    df <- readRDS(file.path(act2$val, file))
    lapply(colnames(df), function(col) {
      data.frame(Variable = col, Class = class(df[[col]]))
    }) %>%
      do.call("rbind", .)
  })
  short2 <- sub(".RDS", "", act2$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act2$classes) <- short2

  testDf <- lapply(names(act2$classes), function(dat) {
    dplyr::full_join(
      dplyr::rename(act1$classes[[dat]],  dir1 = Class),
      dplyr::rename(act2$classes[[dat]],  dir2 = Class),
      by = "Variable") %>%
      mutate(File = dat)
  }) %>%
    do.call("rbind", .) %>%
    mutate(Test = ifelse(dir1 == dir2, "Pass", "Fail")) %>%
    filter(Test == "Fail")
  names(testDf) <- c("Variable", act1$val, act2$val, "File", "Test")
  testDf <- select(testDf, File, Variable, act1$val, act2$val, Test)

  test <- identical(rep("Pass", length(testDf$Test)),  testDf$Test)

  if (test == TRUE) {
    cat("check_class() \n","Variable classes match for each dataset \n")
    succeed()
    return(invisible(act1$val))
  }

  if (test == FALSE) {
    message <- "check_class()  has discovered errors in the following datasets: \n"
    cat(message)
    print(testDf)
    fail(message)
  }
}

# Checks the values in each file in the directories
same_values <- function(dir1, dir2) {

  require(dplyr)
  require(testthat)

  act1 <- quasi_label(rlang::enquo(dir1), deparse(substitute(dir1)))
  act2 <- quasi_label(rlang::enquo(dir2), deparse(substitute(dir2)))

  # Get filenames of data files
  act1$filenames <- list.files(act1$val, pattern = ".RDS", ignore.case = T)
  act2$filenames <- list.files(act2$val, pattern = ".RDS", ignore.case = T)

  # Quick check to make sure all the files are in the directories correctly
  allFiles <- c(act1$filenames, act2$filenames)
  allFiles <- sub(".RDS", "", allFiles, ignore.case = T)
  allFiles <- sub(".sas7bdat", "", allFiles, ignore.case = T)
  dups <- allFiles[duplicated(allFiles)]
  files_without_dups <- allFiles[!allFiles %in% dups]
  if (length(files_without_dups) != 0) {
    message <- paste("There are files in one/both directories that are not in the other.  Please go back and match them. \n",
                     "Please run the same_files() expectation on these directories for more information. \n")
    fail(message)
  }

  act1$data <- lapply(act1$filenames, function(file) {
    df <- readRDS(file.path(act1$val, file))
  })
  short1 <- sub(".RDS", "", act1$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act1$data) <- short1

  act2$data <- lapply(act2$filenames, function(file) {
    df <- readRDS(file.path(act2$val, file))
  })
  short2 <- sub(".RDS", "", act2$filenames, ignore.case = T) %>%
    sub(".sas7bdat", "", ., ignore.case = T)
  names(act2$data) <- short2

  mytests <- lapply(names(act1$data), function(dat) {
    test <- all.equal(act1$data[[dat]],
                      act2$data[[dat]],
                      tolerance = 0.001,
                      countEQ = T)
    if  (TRUE %in% test) {
      data.frame(File = file.path(act1$val, dat),
                 Test = "Passed")
    } else {
      data.frame(File = file.path(act1$val, dat),
                 Test = test)
    }
  }) %>%
    do.call("rbind", .)

  # test results
  test <- identical(rep("Passed", length(mytests$Test)),  mytests$Test)

  if (test == TRUE) {
    cat("same_values() \n   Results of equality tests \n")
    print(mytests)
    succeed()
    return(invisible(act1$val))

  } else {
    cat("same_values() found mismatches in your input datasets: \n")
    print(mytests)
    fail("Failed test")
  }
}

