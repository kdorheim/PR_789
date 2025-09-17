# Exploring Hector Calibration Protocol
# This is the same prtocol that is used for calibrating hector for GCAM.


# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")
source("scripts/fxns_benchmarking.R")

# Flag to indicate if should run the calibration or load previous
# fit parameter values.
RUN <- FALSE

# 1. Calibration ---------------------------------------------------------------

if(RUN){
    # All three variables free at once.
    inital_guess <- c("diff" = 2.5, "beta" = 0.36, "q10_rh" = 2.1)

    # Set up the hector core
    ini <- system.file(package = "hector", "input/hector_ssp245.ini")
    core <- newcore_CH4_N2O(ini, name = "contrs")


    # Select the comparison data.
    comparison_data %>%
        filter(variable %in% c(CONCENTRATIONS_CO2(), GMST(), "OHC")) ->
        comp_data

    fxn  <- internal_fn(p = inital_guess,
                        err_fn = obj_MSE,
                        obs = comp_data,
                        core = core)

    fit1 <- optim(par = inital_guess,
                  fn = fxn,
                  lower = c(0.1, 0.001, 0.001),
                  upper = c(10, 2, 6), method = "L-BFGS-B")

    # Run Hector
    params_to_use <- round(fit1$par, digits = 3)


    write.csv(data.frame(t(params_to_use)), file = file.path("data", "experiments", "exp1_gcam-params.csv"), row.names = FALSE)

} else {

    # Load the previous parameter values.
    file <- file.path("data", "experiments", "exp1_gcam-params.csv")
    params_to_use <- read.csv(file)
}

# 2. Benchmark & Hector Results  ------------------------------------------------

# Generate the benchmark metrics.
out1 <- get_table_metrics(params_to_use)
out1$source <- "exp1_gcam"

write.csv(out1,
          file = file.path("data", "experiments", "exp1_gcam-benchmarks.csv"),
          row.names = FALSE)


# Run Hector with the parameter values
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- my_setvar_fxn(newcore(ini), params_to_use)
run(hc)
fetchvars_4comparison(hc, comparison_data) %>%
    rename(value = hector) ->
    hector_rslts
fetchvars(hc, unique(hector_rslts$year), vars = c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())) ->
    extra

hector_rslts %>%
    bind_rows(extra) %>%
    mutate(scenario = "exp1") ->
    hector_rslts_full

write.csv(hector_rslts_full,
          file = file.path("data", "experiments", "exp1_hector.csv"),
          row.names = FALSE)



# Run all of the SSP scenarios to see what happens in the future!


system.file(package = "hector", "input") %>%
    list.files(pattern = "ssp")







# ggplot() +
#     geom_line(data = hector_rslts_full, aes(year, value, color = "new")) +
#     geom_line(data = comparison_data, aes(year, value, color = "comp")) +
#     facet_wrap("variable", scales = "free", ncol = 1)
#








