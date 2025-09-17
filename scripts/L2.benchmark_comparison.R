

# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(paletteer)

# Plotting aesthetics
theme_set(theme_bw())
JW <- 0

# 1. Load Data -----------------------------------------------------------------

# The benchmark results
here::here("data", "ar6.csv") %>%
    read.csv ->
    ar6_data

here::here("data", "hector_v32.csv") %>%
    read.csv ->
    hector_v32

# Load the calibration results.

bind_rows(read.csv("data/experiments/exp1_gcam-benchmarks.csv"),
          read.csv("data/experiments/exp2-benchmarks.csv")) %>%
    filter(scenario %in% ar6_data$scenario) ->
    new


# Load the hector results
bind_rows( read.csv("data/experiments/exp1_hector.csv"),
           read.csv("data/experiments/exp2_hector.csv")) ->
    hector_rslts



selected_colors <- paletteer_d("colorBlindness::PairedColor12Steps")[1:length(unique(hector_rslts$scenario))]
COLORS_RSLTS <- c(selected_colors, "black")
names(COLORS_RSLTS) <- c(unique(hector_rslts$scenario), "obs")


COLORS_METRICS <- c(selected_colors, "black", "black")
names(COLORS_METRICS) <- c(unique(new$source), "ar6", "v32")


# 2. Future Warming ------------------------------------------------------------

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    ar6_future_warming

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_v32 %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    v32_future_warming

new %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    new_future_warming

ggplot() +
    geom_errorbar(data = ar6_future_warming,
                  aes(year, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_future_warming, aes(year, value, color = "ar6"), shape = 4) +
    geom_point(data = v32_future_warming, aes(year, value, color = "v32"),
               position = position_jitter(height = 0, width = JW)) +
    geom_point(data = new_future_warming, aes(year, value, color = source),
               position = position_jitter(height = 0, width = JW)) +
    labs(y = "degC rel. 1995-2014",
         title = "Global Temp.",
         x = NULL) +
    facet_wrap("scenario", scales = "free", ncol = 1) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS_METRICS)
    plot; plot

ggsave(plot = plot,
       filename = file.path("figs", "future_warming.png"),
       height = 10, width = 5)


# 3. Key Metrics --------------------------------------------------------------
# The "variables" to be considered here...
vars <- c("TCR", "TCRE")

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable %in% vars) ->
    ar6_key_metrics

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_v32 %>%
    filter(variable %in% vars) ->
    v32_key_metrics


new %>%
    filter(variable %in% vars) ->
    new_key_metrics


ggplot() +
    geom_errorbar(data = ar6_key_metrics,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_key_metrics, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = v32_key_metrics, aes(variable, value, color = "v32"),
               position = position_jitter(height = 0, width = JW)) +
    geom_point(data = new_key_metrics, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = JW)) +
    labs(y = NULL,
         title = "Key Metrics",
         x = NULL) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS_METRICS) ->
    plot; plot

ggsave(plot = plot,
       filename = file.path("figs", "key_metrics.png"),
       height = 5, width = 5)


# 4. Historical Benchmarks  ----------------------------------------------------
# The "variables" to be considered here...
vars <- c( "wmghg_ERF",  "RF_CH4", "tot_aer_ERF", "OHC", "hist. warming")

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable %in% vars) ->
    ar6_hist

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_v32 %>%
    filter(variable %in% vars) ->
    v32_hist


# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
new %>%
    filter(variable %in% vars) ->
    new_hist



ggplot() +
    geom_errorbar(data = ar6_hist,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_hist, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = v32_hist, aes(variable, value, color = "v32"),
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

ggsave(plot = plot,
       filename = file.path("figs", "key_metrics.png"),
       height = 5, width = 5)




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



hector_v_obs_fxn(hector_rslts, vars = CONCENTRATIONS_CO2())






# 5. Hector vs. Obs ------------------------------------------------------------



