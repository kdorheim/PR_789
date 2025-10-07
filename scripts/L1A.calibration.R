# Calibrate free Hector parameter values to observations in preparation
# of the V3.5.0 release.


# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")
source("scripts/fxns_benchmarking.R")

# Flag to indicate if should run the calibration or load previous
# fit parameter values.
RUN <- TRUE

# 1. Calibration ---------------------------------------------------------------

if(RUN){
    # All three variables free at once.
    inital_guess <- c("diff" = 1.16, "beta" = 0.55, "q10_rh" = 2.2)

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

    fit <- optim(par = inital_guess,
                 fn = fxn,
                 # Use the boundaries from Matilda
                 lower = c(1.042, 0.45, 1.2),
                 upper = c(1.278, 0.65, 3.2),
                 method = "L-BFGS-B")


    # Save the parameter values.
    params_to_use <- round(fit$par, digits = 3)
    write.csv(data.frame(t(params_to_use)),
              file = file.path("inputs", "params.csv"),
              row.names = FALSE)

} else {

    # Load the previous parameter values.
    file <- file.path("inputs", "params.csv")
    params_to_use <- read.csv(file)
}





