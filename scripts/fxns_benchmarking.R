# Functions used to generate the benchmark results

# working of of
# https://github.com/ptrscll/Scully_2024_SULI_Hector_tech_note/blob/a296f885ea056a20d7dab5e7da5f7a3336eff497/scripts/major_functions.R#L27


# 0. Set Up --------------------------------------------------------------------

# Load the project constants and basic functions
source(here::here("scripts", "constants.R"))
source(here::here("scripts", "fxns_calibration.R"))

# 1. fxns ----------------------------------------------------------------------
#
# params <- c("S" = 5, "diff" = 2.3, "aero_scalar" = 0.5)
# ini <- system.file(package = "hector", "input/hector_ssp245.ini")
# hc <- newcore(ini)
# run(hc)



## 1A. helper fxns -------------------------------------------------------------

# Helper function to normalize hector output
# Args
#   d: data frame of hector results
#   yrs: vector of reference period
# Returns: data frame of normalized results
normalize_fxn <- function(d, yrs){

    # check inputs
    req_cols <- c("variable", "scenario", "value")
    invisible(check_req_names(d, req_cols))
    stopifnot({
        d %>%
            select(variable, scenario) %>% distinct %>%
            nrow == 1})
    stopifnot(all(yrs %in% d$year))


    d %>%
        filter(year %in% yrs) %>%
        pull(value) %>%
        mean ->
        ref_value

    d %>%
        mutate(value = value - ref_value) ->
        out

    return(out)
}



## 1B. TCRE & TCR fxns ----------------------------------------------------------

# Helper function used to calculate the TCRE
# Args
#   params: vector of hector parameters to use
# Returns: TCRE
get_trce_fxn <- function(params){

    # The scenario to use to calculate the TCRE
    ini_tcre <- system.file("input/hector_ssp585.ini", package = "hector")
    hc <- newcore(ini_tcre)

    hc <- my_setvar_fxn(hc, pars = params)

    run(hc)
    out <- fetchvars(core = hc,
                     dates = 1750:2300,
                     vars =  c(GLOBAL_TAS(),
                               FFI_EMISSIONS(), LUC_EMISSIONS(),
                               LUC_UPTAKE(), DACCS_UPTAKE()))


    # Get the total emissions
    out %>%
        subset(variable %in% c(FFI_EMISSIONS(), LUC_EMISSIONS())) %>%
        summarise(value = sum(value), .by = year) %>%
        pull(value) %>%
        cumsum ->
        total_emiss

    # Get the total uptake variables
    out %>%
        subset(variable %in% c(LUC_UPTAKE(), DACCS_UPTAKE())) %>%
        summarise(value = sum(value), .by = year) %>%
        pull(value) %>%
        cumsum ->
        total_uptake

    net_emiss <- total_emiss - total_uptake

    out %>%
        filter(variable == GLOBAL_TAS()) %>%
        pull(value) ->
        temp


    # Calculating TCRE via linear regression
    tcre_reg <- lm(temp ~ net_emiss)
    tcre <- tcre_reg$coefficients[2] * 1000

    return(tcre[[1]])

}


# Helper function used to calculate the TCR
# Args
#   params: vector of hector parameters to use
# Returns: TCR
get_trc_fxn <- function(params){

    ini <- "inputs/hector_1pctCO2.ini"
    stopifnot(file.exists(ini))
    hc <- my_setvar_fxn(newcore(ini), pars = params)

    run(hc)

    dates <- 1800:2000
    temp  <- fetchvars(hc, dates, GLOBAL_TAS())
    co2   <- fetchvars(hc, dates, CONCENTRATIONS_CO2())
    pi_co2 <- fetchvars(hc, NA, PREINDUSTRIAL_CO2())[["value"]]

    # Finding TCR
    tcr_reg <- lm(temp$value ~ co2$value)
    coefficients(tcr_reg)[["(Intercept)"]]
    tcr <- coefficients(tcr_reg)[["(Intercept)"]] + coefficients(tcr_reg)[["co2$value"]] * pi_co2 * 2

    return(tcr)
}



## 1C. historical fxns ----------------------------------------------------------

# Helper function convert ocean heat flux to ocean heat content
# Args
#   d: data frame of hector ocean heat flux
# Returns: data frame of ocean heat content
fetch_ohc <- function(hc, yrs){

    # Ocean heat content constants
    OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
    W_TO_ZJ <- 3.155693e-14              # Watts to ZJ

    # get annual ocean heat content from hector's heat flux variable
    fetchvars(hc, yrs, vars = HEAT_FLUX()) %>%
        mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>%
        select(-units) %>%
        mutate(variable = "OHC", units = "ZJ") ->
        out

    return(out)

}

# Helper function that calculates the total aerosol radiative forcing from a
# active hector core.
# Args
#   d: data frame of hector ocean heat flux
# Returns: data frame of ocean heat content
fetch_total_aerosol <- function(hc, yrs){

    # The aerosol variables
    aerosol_vars <- c(RF_BC(), RF_OC(), RF_NH3(),
                      RF_SO2(), RF_ACI())

    # Calculate the annual total rf
    fetchvars(hc, yrs, vars = aerosol_vars) %>%
        summarise(value = sum(value), .by = c("scenario", "year", "units")) %>%
        mutate(variable = "tot_aer_ERF") ->
        out

    return(out)

}

# Helper function that calculates the non ghg radiative forcing from a
# active hector core.
# Args
#   hc: active hector core
# Returns: data frame of the non ghg rf
fetch_nonghg_rf <- function(hc, yrs){

    # non ghg RF
    aero_rf <- c(RF_BC(), RF_OC(), RF_NH3(), RF_SO2(), RF_ACI())
    non_ghg_rf <- c(aero_rf, RF_VOL(), RF_ALBEDO(), RF_MISC())

    # Calculate the annual total rf
    fetchvars(hc, yrs, vars = non_ghg_rf) %>%
        summarise(value = sum(value), .by = c("scenario", "year", "units")) %>%
        mutate(variable = "non_ghg_ERF") ->
        out

    return(out)

}

# Helper function to get the well mixed ghg from an active hector core.
# Args
#   hc: active hector core
# Returns: data frame of the wmghg erf
fetch_wmghg_rf <- function(hc, yrs){

    nonghg <- fetch_nonghg_rf(hc, yrs)

    fetchvars(hc, yrs, RF_TOTAL()) %>%
        rename(tot = value) %>%
        left_join(nonghg, by = c("year", "scenario")) %>%
        mutate(value = tot - value) %>%
        select(scenario, year, value) %>%
        mutate(variable = "wmghg_ERF", units = getunits("SV")) ->
        out

    return(out)

}


# Helper function that gets the historical values
# Args
#   hc: active hector core
# Returns: data frame of the historical metrics
get_hist_metrics <- function(params){

    # Set up the hector core with the parameter values of interest.
    ini <- system.file(package = "hector", "input/hector_ssp245.ini")
    hc  <- my_setvar_fxn(newcore(ini), params)
    run(hc)

    # historical warming
    fetchvars(hc, dates = 1750:2100, GLOBAL_TAS()) %>%
        normalize_fxn(yrs = 1850:1900) %>%
        filter(year %in% 1995:2014) %>%
        summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
        mutate(variable = "hist. warming",
               year = "1995-2014") ->
        gsat_warming

    # historical ocean heat content
    yrs <- 1971:2018
    fetch_ohc(hc, yrs) %>%
        summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
        mutate(value = value * length(yrs)) %>%
        mutate(year = "1971-2018") ->
        hist_ohc

    # historical aerosol rf
    fetch_total_aerosol(hc, 2005:2015) %>%
        summarise(value = mean(value), .by = c(scenario, variable, units)) %>%
        mutate(year = "2005-15") ->
        tot_aer_erf

    bind_rows(fetch_nonghg_rf(hc, 2019),
              fetch_wmghg_rf(hc, 2019),
              fetchvars(hc, 2019, RF_CH4())) %>%
        mutate(year = as.character(year)) %>%
        bind_rows(tot_aer_erf,
                  hist_ohc,
                  gsat_warming) %>%
        mutate(scenario = "historical") ->
        out

    return(out)
}


## 1D. future fxns -------------------------------------------------------------

# For a single ssp scenario
# Args
#   ini: path to the ini file
#   params: vector of parameters
# returns: data.frame of warming over future periods
get_single_future_warming <- function(ini, params){

    scn <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(ini))
    hc <- my_setvar_fxn(newcore(ini, name = scn), params)
    run(hc)

    fetchvars(hc, 1750:2100, vars = GLOBAL_TAS()) %>%
        normalize_data_fxn(yrs = 1995:2014) %>%
        mutate(units = "degC rel. 1995-2014") ->
        temp

    temp %>%
        filter(year %in% 2021:2040) %>%
        mutate(year = "2021-2040") %>%
        summarise(value = mean(value),
                  .by = c(scenario, year, variable, units)) ->
        near_temp

    temp %>%
        filter(year %in% 2041:2060) %>%
        mutate(year = "2041-2060") %>%
        summarise(value = mean(value),
                  .by = c(scenario, year, variable, units)) ->
        mid_temp

    temp %>%
        filter(year %in% 2081:2100) %>%
        mutate(year = "2081-2100") %>%
        summarise(value = mean(value),
                  .by = c(scenario, year, variable, units)) ->
        long_temp

    out <- rbind(near_temp, mid_temp, long_temp)

    return(out)

}


## 2. get table metrics --------------------------------------------------------

# Get metrics and other benchmarks
# Args
#   params: vector of hector parameter values
# Returns: data.table of the metrics/benchmarks
get_table_metrics <- function(params){

    # emergent climate metrics
    tcre <- data.frame(value = get_trce_fxn(params),
                       variable = "TCRE")
    tcr <- data.frame(value = get_trc_fxn(params),
                      variable = "TCR")

    # historical values
    hist <- get_hist_metrics(params)


    # get the future warming levels
    system.file(package = "hector", "input") %>%
        list.files(pattern = "ssp", full.names = TRUE) %>%
        lapply(FUN = get_single_future_warming, params = params) %>%
        bind_rows ->
        fut

    out <- bind_rows(fut, hist, tcr, tcre)

    return(out)

}

# 3. SSP scenarios -------------------------------------------------------------

system.file(package = "hector", "input") %>%
    list.files(pattern = "ssp", full.names = TRUE) ->
    ALL_SSPS

VARS <- c(GMST(), NPP(), VEG_C(), CONCENTRATIONS_CO2(),
          CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O(),
          HEAT_FLUX(), SST())

# Helper function that runs a batch of ssps for some parameter combination
# Args
#   inis: list of input files to run
#   params: vector of hector parameter values
#   vars: vector of the hector variable names to save
#   source: str name of the param combaition
# Returns: data frame of all the spps included in the inis
batch_ssp_run <- function(inis = ALL_SSPS, params, vars = VARS, source){

    inis %>%
        lapply(function(ini){
            scn <- gsub(pattern = "hector_|.ini", replace = "", x = basename(ini))

            hc <- my_setvar_fxn(newcore(ini, name = scn), pars = params)
            run(hc)
            fetchvars(hc, 1750:2100, vars = vars)
        }) %>%
        bind_rows %>%
        mutate(source = source) ->
        out

    return(out)


}





# 3. default values ------------------------------------------------------------
if(FALSE){

    # Generate the dev branch benchmarking resulst....
    source <- "v349"

    # table values
    default <- get_table_metrics(c("aero_scalar" = 1))
    default$source <- source
    write.csv(default, "data/default_v349.csv", row.names = FALSE)

    # the ssp values
    out <- batch_ssp_run(params = c("aero_scalar" = 1), source = source)
    write.csv(out, "data/hector_v349_ssps.csv", row.names = FALSE)


    # for the observation comparison
    # Run Hector with the parameter values
    ini <- system.file(package = "hector", "input/hector_ssp245.ini")
    hc <- my_setvar_fxn(newcore(ini), c("aero_scalar" = 1))
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
              file = file.path("data", "hector_v349.csv"),
              row.names = FALSE)

}




