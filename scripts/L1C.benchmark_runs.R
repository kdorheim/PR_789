# Using the new parameter values and natural CH4 emissions generate the metrics
# and results to be used in the L2 benchmarking.


# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")
source("scripts/fxns_benchmarking.R")

# 1. Parameter Values  ---------------------------------------------------------
params_to_use <- read.csv("inputs/params.csv")


# 2. Benchmark & Hector Results  -----------------------------------------------
name <- "V3.5.0"

# Generate the benchmark metrics (the metrics/values reported in the AR6 table).
get_table_metrics(params_to_use) %>%
    mutate(source = name) ->
    out

write.csv(out, file = file.path("data", "results", "V350-benchmarks.csv"),
          row.names = FALSE)


# Run historical hector to compare with the observations used in the
# calibration process.
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- my_setvar_fxn(newcore(ini), params_to_use)
run(hc)


# Use the fetchvars 4 comparison function to make sure the data
# is normalized properly and in the correct units.
fetchvars_4comparison(hc, comparison_data) %>%
    rename(value = hector) ->
    hector_rslts

# Get the extra variables to be used in the comparison process.
fetchvars(hc, unique(hector_rslts$year),
          vars = c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())) ->
    extra

# Save the historical hector results in a single data frame
# and write to file.
hector_rslts %>%
    bind_rows(extra) %>%
    mutate(scenario = name) ->
    hector_rslts_full

write.csv(hector_rslts_full,
          file = file.path("data", "results", "V350_hector.csv"),
          row.names = FALSE)


# Run all of the SSP scenarios.
out <- batch_ssp_run(inis = ALL_SSPS, params = params_to_use, source = name)
write.csv(out, "data/results/hector_V350_ssps.csv", row.names = FALSE)



