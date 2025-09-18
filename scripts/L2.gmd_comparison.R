# Add the V35 results to key figures from the 2024 manuscript.

# 0. Set Up -----------------------------------------------------------------
source("scripts/fxns_calibration.R")

# 1. Data -------------------------------------------------------------------

## Load Hector Results
"data/GMD_2024/" %>%
    list.files(pattern = "hector_3.2.0_ssp.csv", full.names = TRUE) %>%
    lapply(read.csv) %>%
    do.call(what = "rbind") %>%
    filter(year <= 2100) %>%
    mutate(source = "v3.2") ->
    hector_ssp_gmd

"data/results/hector_V350_ssps.csv" %>%
    read.csv %>%
    bind_rows(hector_ssp_gmd) ->
    hector_ssp

## 1D. Colors -------------------------------------------------------------------

# have each scenario or experiment correspond to a unique color code
num <- length(unique(hector_ssp$source))
show_col(hue_pal()(num))
selected_colors <- hue_pal()(num)

COLORS <- selected_colors
names(COLORS) <- unique(hector_ssp$source)
COLORS <- c(COLORS, "obs" = "black", "CMIP6 ESM" = "grey")


# 2. Obs. vs Hector Plots  -----------------------------------------------------

# Comparison with Observations
## Atmospheric CO~2~

# Meinshausen, M., Vogel, E., Nauels, A., Lorbacher, K., Meinshausen, N., Etheridge, D. M.,
# Fraser, P. J., Montzka, S. A., Rayner, P. J., Trudinger, C. M., Krummel, P. B., Beyerle,
# U., Canadell, J. G., Daniel, J. S., Enting, I. G., Law, R. M., Lunder, C. R., O'Doherty,
# S., Prinn, R. G., Reimann, S., Rubino, M., Velders, G. J. M., Vollmer, M. K., Wang,
# R. H. J., and Weiss, R.: Historical greenhouse gas concentrations for climate modelling
# (CMIP6), Geosci. Model Dev., 10, 2057–2116, https://doi.org/10.5194/gmd-10-2057-2017, 2017.
here::here("data/GMD_2024/", "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>%
    read.csv(skip = 21) %>%
    na.omit %>%
    select(year =  "v.YEARS.GAS..", value = "CO2") %>%
    mutate(variable = "co2 obs") ->
    cmip6_co2_obs

hector_ssp %>%
    filter(variable == CONCENTRATIONS_CO2()) %>%
    filter(scenario == "ssp119") %>%
    filter(year <= 2014) ->
    hector_co2

# The cmip6_co2_obs data needs to be split into two categories to indicate the period of time
# that was used in the calibration process.
cmip6_co2_obs %>%
    filter(year <= 1800) ->
    inital_obs

cmip6_co2_obs %>%
    filter(year >= 1800) ->
    cmip6_co2_obs

ggplot() +
    geom_line(data = cmip6_co2_obs, aes(year, value, color = "obs")) +
    geom_line(data = hector_co2, aes(year, value, color = source)) +
    labs(x = "Year", y = expression('CO'[2]~' (ppm)')) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/co2_comparison.png", height = 6, width = 6)


cmip6_co2_obs %>%
    select(obs = value, year) %>%
    left_join(hector_co2, by = join_by(year)) %>%
    mutate(AE = abs(value - obs)) %>%
    filter(!is.na(AE)) %>%
    summarise(MAE = mean(AE), .by = source) ->
    MAE_df

diff <- round(abs(diff(MAE_df$MAE)), 3)

MAE_df %>%
    ggplot() +
    geom_bar(aes(source, MAE, fill = source),  stat = "identity") +
    labs(title = "Mean Abolute Error",
         subtitle = "Historical Hector vs. CO2 Obs") +
    labs(x = NULL, caption = paste0("Difference between MAE: ", diff)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = COLORS) ->
    co2_mae

ggsave(plot, filename = "figs/co2_mae.png", height = 6, width = 6)


## Global Mean Surface Temperature
# Hadcrut5
# Global mean surface temperature anomaly
# https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html
# The temperature anomaly is based off of 1961–1990
# surface temperature with the anomaly!
# Morice, C. P., Kennedy, J. J., Rayner, N. A., Winn, J. P., Hogan, E., Killick, R. E., et al. (2021).
# An updated assessment of near-surface temperature change from 1850: the HadCRUT5 data
# set. Journal of Geophysical Research: Atmospheres, 126, e2019JD032361.
# https://doi.org/10.1029/2019JD032361
here::here("data/GMD_2024", "HadCRUT5.csv") %>%
    read.csv(stringsAsFactors = FALSE) %>%
    na.omit ->
    hadcrut_obs_data
names(hadcrut_obs_data) <- c("year", "value", "lower", "upper")
hadcrut_obs_data$variable <- "gmst"

yrs <- hadcrut_obs_data$year

hector_ssp %>%
    filter(variable == "gmst") %>%
    filter(scenario == "ssp119") %>%
    filter(year %in% yrs) %>%
    split(., .$source) %>%
    lapply(FUN = normalize_data_fxn, yrs = 1961:1990) %>%
    bind_rows %>%
    filter(!is.na(value))->
    hector_gmst

ggplot() +
    geom_ribbon(data = hadcrut_obs_data, aes(year, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = hadcrut_obs_data, aes(year, value, color = "obs")) +
    geom_line(data = hector_gmst, aes(year, value, color = source), linewidth = 0.75) +
    labs(x = "Year", y = expression("Temperature anomaly ("~degree~"C)")) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/gmst_obs_hector.png", height = 6, width = 6)


hadcrut_obs_data %>%
    select(obs = value, year) %>%
    left_join(hector_gmst, by = join_by(year)) %>%
    mutate(AE = abs(value - obs)) %>%
    filter(!is.na(AE)) %>%
    summarise(MAE = mean(AE), .by = source) ->
    MAE_df

diff <- round(abs(diff(MAE_df$MAE)), 3)

MAE_df %>%
    ggplot() +
    geom_bar(aes(source, MAE, fill = source),  stat = "identity") +
    labs(title = "Mean Abolute Error",
         subtitle = "Historical Hector vs. CO2 Obs") +
    labs(x = NULL, caption = paste0("Difference between MAE: ", diff)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/gmst_mae.png", height = 6, width = 6)

# 3. Hector vs. CMIP  -----------------------------------------------------


# CMIP6 Comparison
# Make a plot with the ESMs colored by their ECS labels.

scns <- c("ssp245", "ssp126", "ssp585")

# G. A. Meehl, C. A. Senior, V. Eyring, G. Flato, J.-F. Lamarque, R. J. Stouffer, K. E. Taylor,
# M. Schlund, Context for interpreting equilibrium climate sensitivity and transient climate response
# from the CMIP6 Earth system models. Sci. Adv. 6, eaba1981 (2020).
# Schlund, Manuel, Axel Lauer, Pierre Gentine, Steven C. Sherwood, and Veronika Eyring. 2020.
# “Emergent Constraints on Equilibrium Climate Sensitivity in CMIP5: Do They Hold for CMIP6?” Earth System Dynamics 11 (4): 1233–58.

# Lovato, T., Peano, D., Butenschön, M., Materia, S., Iovino, D., Scoccimarro, E.,
# et al. (2022). CMIP6 simulations with the CMCC Earth System Model (CMCCESM2).
# Journal of Advances in Modeling Earth Systems, 14, e2021MS002814.
# https://doi.org/10.1029/2021MS002814
cmip6_ecs <- as.data.frame(rbind(c("ACCESS-CM2", 4.7),
                                 c("ACCESS-ESM1-5", 3.9),
                                 c("CAMS-CSM1-0", 2.3),
                                 c("CanESM5", 5.6),
                                 c("CESM2", 5.2),
                                 c("CESM2-WACCM", 4.8),
                                 c("CMCC-CM2-SR5", 3.52) ,
                                 c("HadGEM3-GC31-LL", 5.6),
                                 c("MIROC-ES2L", 2.7),
                                 c("MIROC6", 2.6),
                                 c("MRI-ESM2-0", 3.2),
                                 c("NorESM2-MM", 2.5),
                                 c("TaiESM1", 4.31),
                                 c("UKESM1-0-LL", 5.3),
                                 c("CMCC-ESM2", 3.57)))
names(cmip6_ecs) <- c("model", "ecs")


# TS.3.2 Climate Sensitivity and Earth System Feedbacks
# the likely range is 2.5°C to 4°C and the very likely range is 2°C to 5°C.
cmip6_ecs %>%
    mutate(id = "not very likely") %>%
    mutate(id = ifelse(ecs >= 2 & ecs <= 5, "very likely", id)) ->
    ecs_table


VARS <- c(GLOBAL_TAS(), LAND_TAS(), SST())


read.csv(here::here("data/GMD_2024", "cmip6_model_means.csv")) %>%
    dplyr::filter(scenario %in% scns)  %>%
    filter(model %in% cmip6_ecs$model) %>%
    filter(variable %in% VARS) %>%
    full_join(ecs_table, by = "model") ->
    cmip6_rslts

hector_ssp %>%
    filter(year %in% 1850:2100) %>%
    filter(variable %in% VARS) %>%
    filter(scenario %in% cmip6_rslts$scenario) %>%
    split(., interaction(.$variable, .$scenario, .$source), drop = TRUE) %>%
    lapply(FUN = normalize_data_fxn, yrs = 1850:1900) %>%
    do.call(what = "rbind") ->
    hector_temp

cmip6_rslts %>%
    group_by(scenario, year, variable, id) %>%
    summarise(min = min(value),
              max = max(value)) %>%
    filter(variable %in% VARS) ->
    cmip6_temp_summary

# Create the labels
temp_labs <- c("Global Mean Air Temp.", "Mean Land Surface Temp.", "Mean Sea Surface Temp.")
names(temp_labs) <-  c(GLOBAL_TAS(), LAND_TAS(), SST())

ggplot() +
    geom_ribbon(data = cmip6_temp_summary, aes(year, ymin = min, ymax = max, fill = id), alpha = 0.9) +
    geom_line(data = hector_temp, aes(year, value, color = source), size = 1) +
    facet_grid(scenario~variable, labeller = labeller(variable = temp_labs), scales = "free") +
    labs(y = expression("Temperature anomaly relative to 1850-1860 ("~degree~"C)"), x = "Year") +
    scale_fill_manual(values = c("very likely" = "#5A5A5A",
                                 "not very likely" = "#D3D3D3")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/cmip6_ribbon.png", height = 6, width = 6)



ggplot() +
    geom_line(data = cmip6_rslts, aes(year, value, group = interaction(model),
                                      color = "CMIP6 ESM"), alpha = 0.5) +
    geom_line(data = hector_temp, aes(year, value, color = source), size = 1) +
    facet_grid(scenario~variable, labeller = labeller(variable = temp_labs), scales = "free") +
    labs(y = expression("Temperature anomaly ("~degree~"C)"), x = "Years") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/cmip6_spagetti.png", height = 6, width = 6)

## Idealized Runs
max_yr <- 150

hector_idealized <- read.csv(file.path("data/GMD_2024", "hector_3.2.0_idealized.csv")) %>%
    filter(variable == GLOBAL_TAS()) %>%
    mutate(scenario = if_else(scenario ==  "abruptx4CO2", "abrupt-4xCO2", scenario)) %>%
    filter(year <= max_yr)


read.csv("data/results/output-V3.5.0.csv") %>%
    filter(scenario %in% c("1pctCO2", "abruptx4CO2"),
           variable %in% hector_idealized$variable) %>%
    mutate(scenario = if_else(scenario ==  "abruptx4CO2", "abrupt-4xCO2", scenario)) %>%
    pivot_longer(-c(version, scenario, variable, units),
                 names_to = "year") %>%
    mutate(year = as.integer(gsub(pattern = "X", replace = "", year))) %>%
    mutate(year = year - 1799) %>%
    filter(year >= 0 & year <= max_yr) ->
    new_hector_idealized

cmip6_idealized <- read.csv(here::here("data/GMD_2024", "cmip6_idealized.csv")) %>%
    filter(year <= max_yr) %>%
    rename(scenario = experiment)


ggplot() +
    geom_line(data = cmip6_idealized, aes(year, value, group = interaction(model, ensemble),
                                          color = "CMIP6 ESM"), alpha = 0.5) +
    geom_line(data = hector_idealized, aes(year, value, color = "v3.2")) +
    geom_line(data = new_hector_idealized, aes(year, value, color = "V3.5.0")) +
    facet_wrap("scenario", scales = "free") +
    labs(y = expression("Temperature anomaly ("~degree~"C)"), x = "Years") +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/cmip6_idealized.png", height = 6, width = 6)
