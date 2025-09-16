# Compute the inital esimate of the natural CH4 emissions, because of the
# temperature effect on permafrost thaw, there might be subtle changes
# to the natural ch4 emissions after the calibration protocol has been finalized....

# 0. Set Up --------------------------------------------------------------------
# There is a helper function we need from the calibration functions script.
source("scripts/constants.R")

# Set up a new Hector core with constrained CH4 and N2O concentrations
# Args
#   ini: path to the hector ini
#   name: "no name" by default but will name the hector results if a
#           different string is provided
# Returns: active hector core ready to run in constraint mode
newcore_CH4_N2O <- function(ini, name = "no name"){

    stopifnot(file.exists(ini))

    # Instantiate the hector core
    hc <- newcore(inifile = ini, name = name)

    # Subset the GHG's that should be used as constraints.
    ch4 <- filter(ghg_constraints, variable == CONCENTRATIONS_CH4())
    n2o <- filter(ghg_constraints, variable == CONCENTRATIONS_N2O())

    # Apply the constraints to the Hector core
    setvar(core = hc, dates = ch4$year, var = CH4_CONSTRAIN(),
           values = ch4$value, unit = getunits(CH4_CONSTRAIN()))
    reset(hc)

    setvar(core = hc, dates = n2o$year, var = N2O_CONSTRAIN(),
           values = n2o$value, unit = getunits(N2O_CONSTRAIN()))
    reset(hc)

    return(hc)

}


# Get a variable from the ini file for those not available to fetch via R
# Args
#   pnames: vector of parameter names to extract from the ini file
#   ini_lines: lines from the hector ini
# Returns: vector of parameter and parameter values
fetch_vars_ini <- function(pnames, ini_lines){

    val <- c()

    for(p in pnames){

        # Identify the ini line where the parameter is set.
        pattern <- paste0("^", p, "=.*;")
        indx    <- which(grepl(pattern = pattern, x = ini_lines))

        # There should have only been one ini line identified.
        stopifnot(length(indx) == 1)

        single_line <- ini_lines[indx]

        single_val <- as.numeric(unlist(strsplit(single_line, split = "=|.;"))[2])
        val <- c(val, single_val)
    }

    names(val) <- pnames
    return(val)
}


# 1. Get Data ------------------------------------------------------------------

# Emissions to be used to calculate the CH4 emissions
emiss_vars <- c(EMISSIONS_CO(), EMISSIONS_NMVOC(),
                EMISSIONS_NOX(), EMISSIONS_CH4())

# Input file
csv_file <- system.file(package = "hector", "input/tables/ssp245_emiss-constraints_rf.csv")
read.csv(csv_file, comment.char = ";") %>%
    pivot_longer(-Date, names_to = "variable") %>%
    rename(year = Date) %>%
    filter(year <= 2015) ->
    input_table

# Initial Emissions
input_table %>%
    filter(year == min(input_table$year)) %>%
    filter(variable %in% emiss_vars) %>%
    select(variable, value) %>%
    tidyr::spread(variable, value) ->
    E0

# Full Emission Input Time Series
input_table %>%
    filter(variable %in% emiss_vars) %>%
    select(year, variable, value) %>%
    tidyr::spread(variable, value) ->
    E_inputs


# CH4 concentrations to fit to
input_table %>%
    filter(variable == CH4_CONSTRAIN()) %>%
    mutate(variable = CONCENTRATIONS_CH4()) ->
    ch4_conc

# Extract constants/parameters/coefficients from a Hector ini file
pnames   <- c("TOH0", "CNOX", "CCO", "CNMVOC", "CCH4", "M0",
              "Tsoil", "Tstrat", "UC_CH4")

ini_lines  <- readLines( system.file(package = "hector", "input/hector_ssp245.ini"))
p_values   <- fetch_vars_ini(pnames, ini_lines)


# 2. Calculate natural CH4 -----------------------------------------------------

## 2A. Get Natural CH4 PI ------------------------------------------------------

# During the preindustrial time period we assume that there is no change
# in emissions or concentrations which simplifies some of the calcualtions.
with(as.list(p_values), {

    delta_CH4 <- 0
    total_emiss   <- (delta_CH4 + M0/TOH0 + M0/Tstrat + M0/Tsoil) * 2.78
    new_nat_emiss <- total_emiss - E0$CH4_emissions

    return(new_nat_emiss)
}) ->
    NCH40

## 2B. Get Natural CH4 until 2022 ----------------------------------------------
with(as.list(p_values), {

    yrs <- E_inputs$year
    natural_emiss_ts <- c(NCH40)
    tau_oh_ts        <- c(TOH0)
    ch4_conc_ts      <- c(M0)


    for (i in 2:nrow(ch4_conc)){

        a <- CCH4 * (log(ch4_conc_ts[i-1]) - log(M0))
        b <- CNOX * (E_inputs$NOX_emissions[i] - E0$NOX_emissions)
        c <- CCO * (E_inputs$CO_emissions[i] - E0$CO_emissions)
        d <- CNMVOC * (E_inputs$NMVOC_emissions[i] - E0$NMVOC_emissions)
        toh_exp <- a + b + c + d

        new_TOH <- TOH0 * exp(-1 * toh_exp)

        delta_CH4 <- ch4_conc$value[i] - ch4_conc_ts[i-1]

        total_emiss   <- (delta_CH4 + ch4_conc_ts[i-1]/new_TOH + ch4_conc_ts[i-1]/Tstrat + ch4_conc_ts[i-1]/Tsoil) * 2.78
        new_nat_emiss <- total_emiss - E_inputs$CH4_emissions[i]
        new_ch4_conc  <- delta_CH4 + ch4_conc_ts[i-1]

        # update results
        natural_emiss_ts <- c(natural_emiss_ts, new_nat_emiss)
        tau_oh_ts <- c(tau_oh_ts, new_TOH)
        ch4_conc_ts <- c(ch4_conc_ts, new_ch4_conc)
    }


    # Format results as a data frame
    rbind( data.frame(year = yrs,
                      value = natural_emiss_ts,
                      variable = NATURAL_CH4()),
           data.frame(year = yrs,
                      value = tau_oh_ts,
                      variable = "TAU_OH"),
           data.frame(year = yrs,
                      value = ch4_conc_ts,
                      variable = CONCENTRATIONS_CH4())) ->
        my_rslts
    return(my_rslts)
}) %>%
    filter(variable == NATURAL_CH4()) ->
    natural_ch4_emissions

## 2C. Future CH4 emissions ----------------------------------------------------

# The time varying emissions should stop at the point concentrations transition
# from historical to projection
# Calculate the decade average
size <- 10

natural_ch4_emissions %>%
    filter(year >= max(natural_ch4_emissions$year) - size) %>%
    pull(value) %>%
    mean ->
    average

data.frame(value = average,
           year = (FINAL_HIST_YEAR +1):2300,
           variable = NATURAL_CH4()) ->
    future_NCH4

# 3. Save natural CH4 emissions -----------------------------------------------------------
# Consolidate the natural emissions into a single data frame.
rbind(natural_ch4_emissions, future_NCH4) %>%
    mutate(units = getunits(NATURAL_CH4())) %>%
    select(Date = year,
           CH4N = value) ->
    out


write.csv(out,
          file = here::here("inputs", "tables", "nat_ch4.csv"),
          row.names = FALSE)





# Z. Testing  ------------------------------------------------------------------

if(FALSE){

    # Let's run hector with these emissions and see how the comparison looks!

    ini <- system.file(package =  "hector", "input/hector_ssp245.ini")
    hc <- newcore(ini)
    setvar(hc, dates = natural_ch4_emissions$year+1, var = NATURAL_CH4(),
           values = natural_ch4_emissions$value, unit = getunits(NATURAL_CH4()))
    reset(hc)
    run(hc)
    fetchvars(hc,  1745:2015, vars = CONCENTRATIONS_CH4()) ->
        out


    ggplot() +
        geom_line(data = ch4_conc, aes(year, value)) +
        geom_line(data = out, aes(year, value))

}


