# Exploring Hector Calibration Protocol....


# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")


# 1. Diff, Beta, Q10, MSE  -----------------------------------------------------
if (CALIBRATION){
    # This is pretty bad I think that it is not favoring the CO2 concentrations
    # enough ah!

    # All three variables free at once.
    inital_guess <- c("diff" = 2.5, "beta" = 0.36, "q10_rh" = 2.1)

    # Set up the hector core
    ini <- "inputs/hector-gcam.ini"
    core <- newcore_CH4_N2O(ini, name = "contrs")


    # Select the comparison data.
    comparison_data %>%
        filter(variable %in% c(CONCENTRATIONS_CO2(), GMST(), "OHC")) ->
        comp_data

    fxn  <- internal_fn(p = inital_guess, err_fn = obj_MSE,
                        obs = comp_data, core = core)
    fit1 <- optim(par = inital_guess, fn = fxn, lower = c(0.1, 0.001, 0.001),
                  upper = c(10, 2, 6), method = "L-BFGS-B")

    # Run Hector
    params_to_use <- round(fit1$par, digits = 3)
    write.csv(data.frame(t(params_to_use)), file = file.path(DIRS$INTERMED, "hector_params.csv"), row.names = FALSE)


}





