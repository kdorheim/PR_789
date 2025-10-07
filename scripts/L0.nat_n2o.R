# Calculate the natural n2o emissions based on historical emissions

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(hector)
library(tidyr)

# Using the CMIP6 N2O concentrations and the antro. N2O emissions
# back calculate the natural N2O emissions for Hector
# Args
#   n2o_conc: data frame of observations of global N2O concentrations
#   total_emiss: data frame of Hector's N2O_emissions
# Returns: data frame of the N2O natural emissions for Hector
get_natural_N2O <- function(n2o_conc, n2o_emiss){

    # Confirm that we are only working with the correct variables.
    stopifnot(unique(n2o_emiss$variable) == EMISSIONS_N2O())
    stopifnot(unique(n2o_conc$variable) == CONCENTRATIONS_N2O())

    # As defined in table S2 of Dorheim et al. 2024
    tau_0 <- 132
    N2O_conc_0 <- 273.87

    # Save the values for 1745 as the starting point.
    my_n2o_conc <- c(N2O_conc_0)
    my_tau      <- c(tau_0)

    total_E <- (N2O_conc_0/tau_0) * 4.8
    new_nat_n2o <- total_E - n2o_emiss$value[1]

    my_nat_n2o  <- c(new_nat_n2o)

    for(t in n2o_emiss$year[-1]){

        # Extract the information that we need in our calculation
        # for the present time step.
        antro_emiss <- n2o_emiss$value[n2o_emiss$year == t]
        lag_n2o <- my_n2o_conc[t-1745]
        current_n2o <- n2o_conc$value[n2o_conc$year == t]

        # Calculate the elements of the N2O concentration equation
        delta_n2o <- current_n2o - lag_n2o
        tau <- tau_0 * (lag_n2o/N2O_conc_0)^(-0.05)
        total_emiss <- 4.8 * (delta_n2o + lag_n2o/tau)
        new_nat_emiss <- total_emiss - antro_emiss

        # Make sure that the natural emissions strictly positive
        # if not then assume 0 natural emissions and update the
        # concentrations accordingly.
        if(new_nat_emiss < 0){
            update_delta_n2o <- antro_emiss/4.8 - lag_n2o/tau
            my_n2o_conc <- c(my_n2o_conc, lag_n2o + update_delta_n2o)
            my_nat_n2o <- c(my_nat_n2o, 0)
        } else {
            my_nat_n2o <- c(my_nat_n2o, new_nat_emiss)
            my_n2o_conc <- c(my_n2o_conc, lag_n2o + delta_n2o)
        }

    }


    data.frame(year = n2o_emiss$year,
               value = my_nat_n2o,
               variable = NAT_EMISSIONS_N2O(),
               units = getunits(NAT_EMISSIONS_N2O())) ->
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
hist_emiss <- get_natural_N2O(n2o_conc = n2o_conc,  n2o_emiss = n2o_emiss)

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
CHECK <- TRUE
if(CHECK){

    # Let's run hector with these emissions and see how the comparison looks!

    ini <- system.file(package =  "hector", "input/hector_ssp245.ini")
    hc <- newcore(ini)
    setvar(hc, dates = hist_emiss$year, var = NAT_EMISSIONS_N2O(),
           values = hist_emiss$value, unit = getunits(NAT_EMISSIONS_N2O()))
    reset(hc)
    run(hc)
    fetchvars(hc,  1745:2015, vars = CONCENTRATIONS_N2O()) ->
        out

    n2o_conc %>%
        rename(obs = value) %>%
        left_join(out, by = c("year")) %>%
        mutate(diff = abs(obs - value)) %>%
        na.omit %>%
        summarise(mean = mean(diff),
                  min = min(diff),
                  max = max(diff),
                  sd = sd(diff))

    ggplot() +
        geom_line(data = n2o_conc, aes(year, value)) +
        geom_line(data = out, aes(year, value, color = "red"), linetype = 2)

}


