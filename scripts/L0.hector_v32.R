# Generate comparison data from the latest release aka hector V32,
# this should only be run once and requires a specific version of Hector
# to be installed.

# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))
source(here::here("scripts", "fxns_calibration.R"))
source(here::here("scripts", "fxns_benchmarking.R"))

# Make sure the correct version of hector is being used here
remotes::install_github("jgcri/hector@main")
library(hector)
expected_version <- "3.2.0"
stopifnot(packageVersion("hector") == expected_version)



# 1. Set Up --------------------------------------------------------------------

# Generate the dev branch benchmarking results....
source <- "v32"
params_to_use <- c("alpha" = 1) # this is the default value

# the metric table comes from the v32 documentation manuscript


# the ssp values
system.file(package = "hector", "input") %>%
    list.files(pattern = "ssp", full.names = TRUE) ->
    ALL_SSPS
out <- batch_ssp_run(inis = ALL_SSPS, params = params_to_use, source = source)
write.csv(out, "data/hector_v32_ssps.csv", row.names = FALSE)


# for the observation comparison
# Run Hector with the parameter values
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- my_setvar_fxn(newcore(ini), params_to_use)
run(hc)
fetchvars_4comparison(hc, comparison_data) %>%
    rename(value = hector) ->
    hector_rslts
fetchvars(hc, unique(hector_rslts$year),
          vars = c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())) ->
    extra

hector_rslts %>%
    bind_rows(extra) %>%
    mutate(scenario = source) ->
    hector_rslts_full

write.csv(hector_rslts_full,
          file = file.path("data", "hector_v32.csv"),
          row.names = FALSE)
