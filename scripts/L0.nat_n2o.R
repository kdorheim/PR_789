# Calculate the natural n2o emissions based on historical emissions

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(hector)
library(tidyr)

# Using observations of N2O concentrations and the anthropocentric N2O emissions
# back calculate the natural N2O emissions for Hector
# Args
#   n2o_conc: data frame of observations of global N2O concentrations
#   total_emiss: data frame of Hector's N2O_emissions
# Returns: data frame of the N2O natural emissions for Hector
get_natural_N2O <- function(n2o_conc, total_emiss){

    # Confirm that we are only working with the correct variables.
    stopifnot(unique(total_emiss$variable) == EMISSIONS_N2O())
    stopifnot(unique(n2o_conc$variable) == CONCENTRATIONS_N2O())


    # As defined in table S2 of Dorheim et al. 2024
    tau_0 <- 132
    N2O_conc_0 <- 273.87

    # Save information about the number of entries
    n <- nrow(n2o_conc)

    # Determine the change in N2O concentrations
    # per time step.
    delta_n2o <- diff(n2o_conc$value)

    # Calculate N2O lifetime
    tau_n2o <- tau_0 * (n2o_conc$value[1:n-1]/N2O_conc_0)^(-0.05)

    # Calculate the total emissions based on equation (S1)
    my_emiss <- 4.8 * (delta_n2o + n2o_conc$value[1:n-1]/tau_n2o)

    # Calculate the difference between total emissions associated with
    # n2o concentrations and the anthropocentric emissions.
    natural_emiss <- my_emiss-total_emiss$value[2:n]

    data.frame(year = total_emiss$year[1:n-1]+1,
               value = natural_emiss,
               variable = NAT_EMISSIONS_N2O()) %>%
        na.omit() %>%
        mutate(units = getunits(NAT_EMISSIONS_N2O())) ->
        out

    return(out)

}

# 1. Data  ---------------------------------------------------------------------

system.file(package = "hector", "input/tables") %>%
    list.files("ssp245_emiss-constraints_rf.csv", full.names = TRUE) %>%
    read.csv(comment.char = ";") %>%
    select(Date, N2O_emissions, N2O_constrain) %>%
    filter(Date <= 2016) %>%
    # Change the format from wide to long
    pivot_longer(names_to = "variable", cols = c(N2O_emissions, N2O_constrain)) %>%
    rename(year = Date) ->
    hector_inputs


hector_inputs %>%
    filter(variable == EMISSIONS_N2O()) ->
    n2o_emiss


hector_inputs %>%
    filter(variable == N2O_CONSTRAIN()) %>%
    mutate(variable = CONCENTRATIONS_N2O()) ->
    n2o_conc

# the historical N2O emissions
hist_emiss <- get_natural_N2O(n2o_conc = n2o_conc,  total_emiss = n2o_emiss)

# Use the last decade of the historical natural emissions to for the
# future values.
hist_emiss %>%
    filter(year %in% 2005:2015) %>%
    pull(value) %>%
    mean ->
    future_val

data.frame(year = 2016:2100,
           value = future_val,
           variable = NAT_EMISSIONS_N2O(),
           units = getunits(NAT_EMISSIONS_N2O())) ->
    future_emiss



hist_emiss %>%
    bind_rows(future_emiss) %>%
    select(Date = year,
           N2O_natural_emissions = value) ->
    out


write.csv(out,
          file = here::here("inputs", "tables", "nat_n2o.csv"),
          row.names = FALSE)



# 2. Testing  ------------------------------------------------------------------

if(FALSE){

    # Let's run hector with these emissions and see how the comparison looks!

    ini <- system.file(package =  "hector", "input/hector_ssp245.ini")
    hc <- newcore(ini)
    setvar(hc, dates = hist_emiss$year+1, var = NAT_EMISSIONS_N2O(),
           values = hist_emiss$value, unit = getunits(NAT_EMISSIONS_N2O()))
    reset(hc)
    run(hc)
    fetchvars(hc,  1745:2015, vars = CONCENTRATIONS_N2O()) ->
        out


    ggplot() +
        geom_line(data = n2o_conc, aes(year, value)) +
        geom_line(data = out, aes(year, value))

}


