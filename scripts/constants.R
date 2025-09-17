# Read in R packages and define project constants

# Start from a clean environment
# TODO this would be dropped if written as a function like gcamdata
#remove(list = ls())


# 0. Load packages -------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(here)
library(tidyr)
library(zoo)
#remotes::install_github("jgcri/hector@dev")
# we want to use the local version!
library(hector)
library(readxl)


# During development let's not set these versions in stone
if (FALSE) {
    # TODO probably use a package manager but for now this is probably good enough
    stopifnot(packageVersion("dplyr") == "1.1.4")
    stopifnot(packageVersion("tidyr") == "1.3.1")
    stopifnot(packageVersion("here") == "1.0.1")
    stopifnot(packageVersion("zoo") == "1.8.12")
    stopifnot(packageVersion("hector") == "3.4.95")
}

# packages that are probably not going to be required but could be helpful during
# the developmental stage.
library(ggplot2)

# 1. Set Up Directories --------------------------------------------------------
BASE <-  here::here()
DIRS <- list(
    DATA = file.path(BASE, "data"),
    RAW_DATA = file.path(BASE, "data", "raw-data"),
    CALIBRATION_DATA = file.path(BASE, "data", "calibration"),
    MAPPING = file.path(BASE, "data", "mapping"),
    INTERMED = file.path(BASE, "data", "intermed"),
    INPUTS = file.path(BASE, "inputs")
)

sapply(DIRS,
       dir.create,
       showWarnings = FALSE,
       recursive = TRUE)


# 2. Define Helper Functions ---------------------------------------------------


# Write a hector input table Save the hector csv files into the proper hector format
# Args
#   x: data table containing Hector input values
#   required: str vector of the required variables that must be included in the table
#   write_to: str directory to write the hector csv output to
#   save_as: string of the csv file name to save
# Return: str file name
write_hector_csv <- function(x,
                             required=NULL,
                             write_to,
                             save_as){

    # Format and save the emissions and concentration constraints in the csv files
    # in the proper Hector table input file.
    assert_that(dir.exists(write_to))
    assert_that(has_name(x, c("year", "variable", "units", "value")))
    x <- as.data.table(x)

    # Create the file name
    fname <- file.path(write_to, save_as)

    if(!is.null(required)){
        missing <- !required %in% unique(x[["variable"]])
        assert_that(all(!missing), msg = paste("Missing required variable(s):", paste0(required[missing], collapse = ", ")))
    }

    # Transform the data frame into the wide format that Hector expects.
    input_data <- dcast(as.data.table(x)[, list(Date = year, variable, value)], Date ~ variable)

    # Throw an error if any NAs are included in the data frame.
    #stopifnot(any(is.na(input_data)))

    # Add the header information to the csv table.
    # TODO look into a more efficient way to do this, one that does not
    # require intermediate products to be written out to the disk.
    readr::write_csv(input_data, fname, append = FALSE, col_names = TRUE)
    lines <- readLines(fname)

    # Format a list of units that will be used in the header.
    vars <- names(input_data)
    var_units <- getunits(vars[-1])
    units_list <- paste(c('; UNITS:', var_units), collapse = ', ')

    git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
    create_info <-  c(paste0('; created by history4hector on ', date(),
                             " commit ", git_tag))
    final_lines <- append(c(paste0('; TODO add documenation'),
                            paste0("; commit ", git_tag),
                            paste0("; date ", date()),
                            units_list),
                          lines)
    writeLines(final_lines, fname)
    return(fname)

}


# Format the units string
# Args
#   str: unit string
# Return: units string that should be compatible with the mapping units
format_units_fxn <- function(str) {
    tolower(gsub(
        pattern = " ",
        replacement = "",
        x = str
    ))
}


# Check to make sure the data frame has the required columns
# Args
#   df: data.frame
#   req_cols: str vector of the required column names
# Return: throws an error if missing a required column and returns a
# data frame with only the required columns of data.
check_req_names <- function(df, req_cols) {
    missing <- setdiff(req_cols, names(df))

    if (!length(missing) == 0) {
        x <-
            paste0("df missing required columns: ", missing, collapse = ", ")
        stop(x)

    }


    df %>%
        select(all_of(req_cols)) ->
        df

    return(df)
}


# 3. Constants -----------------------------------------------------------------

FINAL_HIST_YEAR <- 2015
