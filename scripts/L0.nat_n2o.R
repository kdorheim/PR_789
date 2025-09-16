# Calculate the natural n2o emissions based on historical emissions

# 0. Set Up --------------------------------------------------------------------
library(dplyr)

# 1. Natural N2O Emissions -----------------------------------------------------

# Assume there is no change in N2O concentrations per time step.
dN2O <- 0

# Parameter values from hector ini files...
N0=273.87 		# (ppb) preindustrial nitrous oxide from IPCC AR6 table 7.SM.1
UC_N2O=4.8		# TgN/ppbv unit conversion between emissions and concentrations
TN2O0=132       # initial lifetime of N2O, years

# Load the n2o emissions from the input table.
system.file(package = "hector", "input/tables") %>%
    list.files(pattern = "ssp245", full.names = TRUE) %>%
    read.csv(comment.char = ";") %>%
    select(year = Date, N2O_emissions, N2O_constrain) %>%
    filter(year <= 2015) ->
    n2o_inputs


# dN2O <- diff(n2o_inputs$N2O_constrain)
# E_n2o <- n2o_inputs$N2O_emissions
# n2o_conc <- n2o_inputs$N2O_constrain


# attempt 2 -------

n2o_conc_inputs <- n2o_inputs$N2O_constrain
E_n2o <- n2o_inputs$N2O_emissions

n2o_conc <- 273.7540
nat_emiss <- c()

for(t in 2:nrow(n2o_inputs)){

    tau_n2o_t <- TN2O0 * (n2o_conc[t-1]/N0) ^ (-0.05)
    dn2o_t <- n2o_conc_inputs[t] - n2o_conc[t-1]

    total_emiss_t <- (dn2o_t + n2o_conc[t-1]/tau_n2o_t) * UC_N2O
    nat_emiss_t <- total_emiss_t - E_n2o[t]
    n2o_conc_t <- n2o_conc[t-1] + dn2o_t
    n2o_conc <- c(n2o_conc, n2o_conc_t)

    nat_emiss <- c(nat_emiss, nat_emiss_t)

}

# format the results as a data frame
data.frame(year = n2o_inputs$year[-1],
           value = nat_emiss,
           variable = NAT_EMISSIONS_N2O(),
           units = getunits(NAT_EMISSIONS_N2O())) ->
    hist_nat_df

hist_nat_df %>%
    filter(year %in% 2005:2015) %>%
    pull(value) %>%
    mean ->
    fut_nat_n2o_value


data.frame(year = 2016:2100,
           value = fut_nat_n2o_value,
           variable = NAT_EMISSIONS_N2O(),
           units = getunits(NAT_EMISSIONS_N2O())) ->
    fut_nat_n2o_df

hist_nat_df %>%
    bind_rows(fut_nat_n2o_df) ->
    nat_n2o_df

# Save as a data.frame
nat_n2o_df %>%
    select(Date = year, N2O_natural_emissions = value) ->
    out

write.csv(out,
          file = file.path("inputs", "tables", "nat_n2o.csv"),
          row.names = FALSE)

# 3. Plots ---------------------------------------------------------------------

# default hector
ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc)

fetchvars(hc, 1745:2015, vars = CONCENTRATIONS_N2O()) %>%
    mutate(scenario = "v32") ->
    n2o1

n2o_inputs %>%
    filter(year <= 2015) %>%
    select(year, value = N2O_constrain) %>%
    mutate(variable = CONCENTRATIONS_N2O(),
           scenario = "rmip") ->
    rcmip_n2o

fetchvars(hc, 1745:2015, vars = NAT_EMISSIONS_N2O()) %>%
    mutate(scenario = "v32") ->
    nat1

# Try running hector with the newly calculated natural n2o emissions
setvar(core = hc,
       dates = hist_nat_df$year,
       var = NAT_EMISSIONS_N2O(),
       values = nat_emiss,
       unit = getunits(NAT_EMISSIONS_N2O()))

reset(hc)
run(hc)

fetchvars(hc, 1745:2015, vars = CONCENTRATIONS_N2O()) %>%
    mutate(scenario = "new") ->
    n2o2

# natural n2o emiss
nat1 %>%
    bind_rows(hist_nat_df %>% mutate(scenario = "new")) %>%
    ggplot() +
    geom_line(aes(year, value, color = scenario)) +
    labs(title = NAT_EMISSIONS_N2O(),
         y = NULL, x = NULL) +
    theme(legend.title = element_blank()) ->
    plot; plot

ggsave(plot = plot,
       filename = file.path("figs", "natural_n2o_emiss.png"),
       height = 5, width = 5)

# the n2o concentrations
rcmip_n2o %>%
    bind_rows(n2o1,
              n2o2) %>%
    ggplot() +
    geom_line(aes(year, value, color = scenario, linetype = scenario)) +
    labs(title = CONCENTRATIONS_N2O(),
         y = NULL, x = NULL) +
    theme(legend.title = element_blank()) ->
    plot; plot

ggsave(plot = plot,
       filename = file.path("figs", "n2o_conc.png"),
       height = 5, width = 5)

