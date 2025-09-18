# Compare all of the possible results against one another...

# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(paletteer)

# Plotting aesthetics
theme_set(theme_bw())
JW <- 0.15

# 1. Load Data -----------------------------------------------------------------

## 1A. Benchmark table values ---------------------------------------------------
here::here("data", "ar6.csv") %>%
    read.csv ->
    ar6_data

here::here("data", "ar6_scms.csv") %>%
    read.csv ->
    ar6_scms_data

here::here("data", "default_v32.csv") %>%
    read.csv ->
    hector_v32

here::here("data", "default_v349.csv") %>%
    read.csv ->
    hector_v349

bind_rows(hector_v32,
          hector_v349) %>%
    filter(scenario %in% ar6_data$scenario) ->
    hector_old

bind_rows(read.csv("data/experiments/exp1_gcam-benchmarks.csv"),
          read.csv("data/experiments/exp2-benchmarks.csv")) %>%
    filter(scenario %in% ar6_data$scenario) ->
    new

## 1B. historical hector for obs. comparison  -----------------------------------
# Load the hector results to be use in the comparison with observations
bind_rows(read.csv("data/experiments/exp1_hector.csv"),
          read.csv("data/experiments/exp2_hector.csv"),
          read.csv("data/hector_v32.csv"),
          read.csv("data/hector_v349.csv")) ->
    hector_rslts

## 1C. hector ssps --------------------------------------------------------------

# Load the ssp hector results
bind_rows(read.csv("data/hector_v32_ssps.csv"),
          read.csv("data/hector_v349_ssps.csv"),
          read.csv("data/experiments/hector_exp1_ssps.csv"),
          read.csv("data/experiments/hector_exp2_ssps.csv")) ->
    hector_ssps




## 1D. Colors -------------------------------------------------------------------

# have each scenario or experiment correspond to a unique color code
num <- length(unique(new$scenario))
show_col(hue_pal()(num))
selected_colors <- hue_pal()(num)


COLORS_RSLTS <- selected_colors
names(COLORS_RSLTS) <- unique(hector_rslts$scenario)
COLORS_RSLTS <- c(COLORS_RSLTS, "obs" = "black", "old hector" = "grey")


COLORS_METRICS <- selected_colors
names(COLORS_METRICS) <- unique(new$source)
COLORS_METRICS <- c(COLORS_METRICS, "ar6" = "black", "v32" = "black", "old hector" =  "grey")


# 2. Future Warming  -----------------------------------------------------------

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    ar6_future_warming

ar6_scms_data%>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    ar6_scms_future_warming

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_old %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    old_future_warming

new %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    new_future_warming

ggplot() +
    geom_errorbar(data = ar6_future_warming,
                  aes(year, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_future_warming, aes(year, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_scms_future_warming, aes(year, value, color = "scms"),
               shape = 4, position = position_jitter(height = 0, width = JW)) +
    geom_point(data = old_future_warming, aes(year, value, color = "old hector"),
               position = position_jitter(height = 0, width = JW)) +
    geom_point(data = new_future_warming, aes(year, value, color = source),
               position = position_jitter(height = 0, width = JW)) +
    labs(y = "degC rel. 1995-2014",
         title = "Global Temp.",
         x = NULL) +
    facet_wrap("scenario", scales = "free", ncol = 1) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS_METRICS) ->
plot; plot

# 3. Key Metrics --------------------------------------------------------------
# The "variables" to be considered here...
vars <- c("TCR", "TCRE")

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable %in% vars) ->
    ar6_key_metrics

ar6_scms_data %>%
    filter(variable %in% vars) ->
    ar6_key_scms_metrics

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_old %>%
    filter(variable %in% vars) ->
    old_key_metrics

new %>%
    filter(variable %in% vars) ->
    new_key_metrics


ggplot() +
    geom_errorbar(data = ar6_key_metrics,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_key_metrics, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_key_scms_metrics, aes(variable, value, color = "scms"),
               shape = 4, position = position_jitter(height = 0, width = JW)) +
    geom_point(data = old_key_metrics, aes(variable, value, color = "old hector"),
               position = position_jitter(height = 0, width = JW)) +
    geom_point(data = new_key_metrics, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = JW)) +
    labs(y = NULL,
         title = "Key Metrics",
         x = NULL) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS_METRICS) ->
    plot; plot

# 4. Historical Benchmarks  ----------------------------------------------------
# The "variables" to be considered here...
vars <- c( "wmghg_ERF",  "RF_CH4", "tot_aer_ERF", "OHC", "hist. warming")

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable %in% vars) ->
    ar6_hist

ar6_scms_data %>%
    filter(variable %in% vars) ->
    ar6_scms_hist


# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_old %>%
    filter(variable %in% vars) ->
    old_hist


# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
new %>%
    filter(variable %in% vars) ->
    new_hist



ggplot() +
    geom_errorbar(data = ar6_hist,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_hist, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_scms_hist, aes(variable, value, color = "scms"),
               shape = 4,  position = position_jitter(height = 0, width = JW)) +
    geom_point(data = old_hist, aes(variable, value, color = "old hector"),
               position = position_jitter(height = 0, width = JW)) +
    geom_point(data = new_hist, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = JW)) +
    labs(y = NULL,
         title = "Key Metrics",
         x = NULL) +
    facet_wrap("variable", scales = "free") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS_METRICS) ->
    plot; plot


# 5. Hector vs. Obs ------------------------------------------------------------

hector_v_obs_fxn <- function(hector_data, vars, SAVE = FALSE){

    comparison_data %>%
        filter(variable %in% vars) ->
        comp_to_plot

    hector_data %>%
        filter(variable %in% vars) %>%
        filter(year %in% comp_to_plot$year) ->
        hector_to_plot


    ggplot() +
        geom_line(data = comp_to_plot, aes(year, value, color = "obs"), linewidth = 1, alpha = 0.25) +
        geom_line(data = hector_to_plot, aes(year, value, color = scenario), linewidth = 0.75) +
        facet_wrap("variable", scales = "free") +
        labs(x = NULL, y = NULL) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        scale_color_manual(values = COLORS_RSLTS) ->
        plot; plot

    return(plot)

    if(SAVE){

        pfile <- file.path("figs", paste0("hector_", paste0(vars, collapse = ""), ".png"))
        ggsave(plot = plot,
               filename = file.path("figs", pfile),
               height = 5, width = 5)
    }

}


hector_rslts %>%
    select(scenario, year, variable, hector = value) %>%
    left_join(comparison_data, by = join_by(year, variable)) %>%
    mutate(diff = abs(hector - value)) ->
    wide_obs_v_hector


wide_obs_v_hector %>%
    filter(!is.na(diff)) %>%
    summarise(MAE = mean(diff), .by = c(scenario, variable)) %>%
    ggplot() +
    geom_bar(aes(scenario, MAE),  stat = "identity") +
    facet_wrap("variable", scales = "free") +
    labs(title = "Mean Abolute Error",
         subtitle = "Historical Hector vs. Obs")

wide_obs_v_hector %>%
    filter(!is.na(diff)) %>%
    filter(variable == CONCENTRATIONS_CO2()) %>%
    ggplot(aes(year, diff, color = scenario)) +
    geom_line() +
    labs(title = "Abosolute Error",
         y = "Absolute Error",
         subtitle = CONCENTRATIONS_CO2())

wide_obs_v_hector %>%
    filter(variable == "OHC") %>%
    filter(!is.na(diff)) %>%
    ggplot(aes(year, diff, color = scenario)) +
    geom_line() +
    labs(title = "Abosolute Error",
         y = "Absolute Error",
         subtitle = "ocean heat content")

wide_obs_v_hector %>%
    filter(variable == GMST()) %>%
    filter(!is.na(diff)) %>%
    ggplot(aes(year, diff, color = scenario)) +
    geom_line() +
    labs(title = "Abosolute Error",
         y = "Absolute Error",
         subtitle = GMST())

# 6. SSP scenarios ------------------------------------------------------------

# How do the future scenarios compare to one another??
hector_ssps %>%
    filter(variable == GMST()) %>%
    ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
    geom_line() +
    labs(title = GMST())

hector_ssps %>%
    filter(variable == CONCENTRATIONS_CO2()) %>%
    ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
    geom_line() +
    labs(title = CONCENTRATIONS_CO2())

hector_ssps %>%
    filter(variable == NPP()) %>%
    ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
    geom_line() +
    labs(title = NPP())

hector_ssps %>%
    filter(variable == SST()) %>%
    ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
    geom_line() +
    labs(title = SST())

hector_ssps %>%
    filter(variable == HEAT_FLUX()) %>%
    ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
    geom_line() +
    labs(title = HEAT_FLUX())

