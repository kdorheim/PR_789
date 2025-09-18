# Exploring Hector Calibration Protocol
# This is the same protocol that is used for calibrating hector for GCAM.


# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")
source("scripts/fxns_benchmarking.R")

# Flag to indicate if should run the calibration or load previous
# fit parameter values.
RUN <- FALSE

# 1. Calibration ---------------------------------------------------------------

if(RUN){
    # All three variables free at once.
    inital_guess <- c("diff" = 2.5)

    # Set up the hector core make with the major GHGs constrained,
    # right now only calibrated the hector's thermal component.
    ini  <- system.file(package = "hector", "input/hector_ssp245.ini")
    core <- newcore_CO2_CH4_N2O(ini, name = "constrainted")

    # Select the comparison data.
    comparison_data %>%
        filter(variable %in% c(GMST(), "OHC")) ->
        comp_data

    fxn  <- internal_fn(p = inital_guess,
                        err_fn = obj_MSE,
                        obs = comp_data,
                        core = core)

    fit1 <- optim(par = inital_guess,
                  fn = fxn,
                  lower = c(1e-2),
                  upper = c(10), method = "L-BFGS-B")

    # Run Hector
    params_to_use <- round(fit1$par, digits = 3)

    # Now calibrate the CO2 parameters...
    # Make sure to fix the parameter we found in part 1
    ini  <- system.file(package = "hector", "input/hector_ssp245.ini")
    core <- newcore_CH4_N2O(ini, name = "emiss. co2 driven")
    core <- my_setvar_fxn(core, params_to_use)


    # Select the comparison data, while these parameters only
    # have indirectly effects on temperature let's make sure
    # that we are still minimzing temp error.
    comparison_data %>%
        filter(variable %in% c("CO2_concentration")) ->
        comp_data

    inital_guess <- c("beta" = 10, "q10_rh" = 1)
    fxn  <- internal_fn(p = inital_guess,
                        err_fn = obj_MSE,
                        obs = comp_data,
                        core = core)

    fit2 <- optim(par = inital_guess,
                  fn = fxn,
                  method = "L-BFGS-B",
                  lower = c(1e-1, 1e-1),
                  upper = c(10, 10))


    params_to_use <- round(c(fit1$par, fit2$par),  digits = 3)


    write.csv(data.frame(t(params_to_use)), file = file.path("data", "experiments", "exp2-params.csv"), row.names = FALSE)

} else {

    # Load the previous parameter values.
    file <- file.path("data", "experiments", "exp2-params.csv")
    params_to_use <- read.csv(file)
}

# 2. Benchmark & Hector Results  ------------------------------------------------

# Generate the benchmark metrics.
out1 <- get_table_metrics(params_to_use)
out1$source <- "exp2_2step"

write.csv(out1,
          file = file.path("data", "experiments", "exp2-benchmarks.csv"),
          row.names = FALSE)


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
    mutate(scenario = "exp2") ->
    hector_rslts_full

write.csv(hector_rslts_full,
          file = file.path("data", "experiments", "exp2_hector.csv"),
          row.names = FALSE)


source <- "exp2"

# the ssp values
system.file(package = "hector", "input") %>%
    list.files(pattern = "ssp", full.names = TRUE) ->
    ALL_SSPS
out <- batch_ssp_run(inis = ALL_SSPS, params = params_to_use, source = source)
write.csv(out, "data/experiments/hector_exp2_ssps.csv", row.names = FALSE)

