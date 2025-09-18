# Compare V32 & V35 results against known benchmarks from AR6 SM7 table 10.

# 0. Set Up --------------------------------------------------------------------
source("scripts/fxns_calibration.R")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(paletteer)
library(ggpmisc)
library(scales)

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
    read.csv %>%
    mutate(source = "v32") ->
    hector_v32

here::here("data", "results", "V350-benchmarks.csv") %>%
    read.csv ->
    hector_v35

hector_v32 %>%
    bind_rows(hector_v35) %>%
    filter(scenario %in% ar6_data$scenario) ->
    hector_benchmarks

## 1B. historical hector for obs. comparison  -----------------------------------
# Load the hector results to be use in the comparison with observations
bind_rows(read.csv("data/hector_v32.csv"),
          read.csv("data/results/V350_hector.csv")) ->
    hector_rslts

## 1C. hector ssps --------------------------------------------------------------

# Load the ssp hector results
bind_rows(read.csv("data/hector_v32_ssps.csv"),
          read.csv("data/results/hector_V350_ssps.csv")) ->
    hector_ssps

## 1D. Colors -------------------------------------------------------------------

# have each scenario or experiment correspond to a unique color code
num <- length(unique(hector_benchmarks$source))
selected_colors <- hue_pal()(num)

COLORS <- selected_colors
names(COLORS) <- unique(hector_benchmarks$source)
COLORS <- c(COLORS, "obs" = "black", "ar6" = "black", "scms" = "grey")



# 2. Future Warming  -----------------------------------------------------------

# Get the AR6 values, these are the ultimate benchmark for comparisons.
ar6_data %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    ar6_future_warming

ar6_scms_data %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    ar6_scms_future_warming

# Get the V3.2 values, that were included in Dorheim et al 2024 paper.
hector_benchmarks %>%
    filter(variable == "global_tas",
           units == "degC rel. 1995-2014") ->
    hector_future_warming

ggplot() +
    geom_errorbar(data = ar6_future_warming,
                  aes(year, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_future_warming,
               aes(year, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_scms_future_warming,
               aes(year, value, color = "scms"),
               shape = 4, position = position_jitter(height = 0, width = JW)) +
    geom_point(data = hector_future_warming,
               aes(year, value, color = source), alpha = 0.7,
               position = position_jitter(height = 0, width = 0)) +
    labs(y = "degC rel. 1995-2014",
         title = "Global Temp.",
         x = NULL) +
    facet_wrap("scenario", scales = "free", ncol = 1) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/benchmark_future_warming.png", height = 8, width = 4)


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
hector_benchmarks %>%
    filter(variable %in% vars) ->
    hector_key_metrics

ggplot() +
    geom_errorbar(data = ar6_key_metrics,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_key_metrics, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_key_scms_metrics, aes(variable, value, color = "scms"),
               shape = 4, position = position_jitter(height = 0, width = JW)) +
    geom_point(data = hector_key_metrics, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = 0)) +
    labs(y = NULL,
         title = "Key Metrics",
         x = NULL) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/benchmark_metrics.png", height = 4, width = 4)

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
hector_benchmarks %>%
    filter(variable %in% vars) ->
    hector_hist



ggplot() +
    geom_errorbar(data = ar6_hist,
                  aes(variable, ymin = min, ymax = max),
                  width=.2, alpha = 0.5) +
    geom_point(data = ar6_hist, aes(variable, value, color = "ar6"), shape = 4) +
    geom_point(data = ar6_scms_hist, aes(variable, value, color = "scms"),
               shape = 4,  position = position_jitter(height = 0, width = JW)) +
    geom_point(data = hector_hist, aes(variable, value, color = source),
               position = position_jitter(height = 0, width = 0), alpha = 0.7) +
    labs(y = NULL,
         title = "Key Metrics",
         x = NULL) +
    facet_wrap("variable", scales = "free") +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/benchmark_hist.png", height = 6, width = 6)

# 5. Hector vs. Obs ------------------------------------------------------------

hector_v_obs_fxn <- function(hector_data, vars, SAVE = FALSE){

    comparison_data %>%
        filter(variable %in% vars) ->
        comp_to_plot

    hector_data %>%
        filter(variable %in% vars) %>%
        filter(year %in% comp_to_plot$year) ->
        hector_to_plot

    comp_to_plot %>%
        select(year, variable, obs = value) %>%
        left_join(hector_to_plot) %>%
        summarise(MAE = mean(abs(obs - value)), .by = c(variable, scenario)) %>%
        mutate(MAE = signif(MAE, digits = 3)) ->
        MAE_table

    tbs <- lapply(split(MAE_table, MAE_table$variable), "[")

    df <- tibble(x = rep(Inf, length(tbs)),
                 y = rep(-Inf, length(tbs)),
                 variable = vars,
                 tbl = tbs)

    ggplot() +
        geom_line(data = comp_to_plot, aes(year, value, color = "obs"), linewidth = 1, alpha = 0.25) +
        geom_line(data = hector_to_plot, aes(year, value, color = scenario), linewidth = 0.75) +
        facet_wrap("variable", scales = "free") +
        labs(x = NULL, y = NULL) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        scale_color_manual(values = COLORS) +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                   hjust = 1, vjust = 0) ->
        plot; plot

    return(plot)

    if(SAVE){

        pfile <- file.path("figs", paste0("hector_", paste0(vars, collapse = ""), ".png"))
        ggsave(plot = plot,
               filename = file.path("figs", pfile),
               height = 5, width = 5)
    }

}


hector_v_obs_fxn(hector_rslts, vars = GMST()) ->
    plot; plot

ggsave(plot, filename = "figs/gmst_obs_v_hector.png", height = 6, width = 6)

hector_v_obs_fxn(hector_rslts, vars = CONCENTRATIONS_CO2()) ->
    plot; plot

ggsave(plot, filename = "figs/co2_obs_v_hector.png", height = 6, width = 6)

hector_v_obs_fxn(hector_rslts, vars = "OHC") ->
    plot; plot

ggsave(plot, filename = "figs/OHC_obs_v_hector.png", height = 6, width = 6)


hector_rslts %>%
    select(scenario, year, variable, hector = value) %>%
    left_join(comparison_data, by = join_by(year, variable)) %>%
    mutate(diff = abs(hector - value)) ->
    wide_obs_v_hector

wide_obs_v_hector %>%
    filter(!is.na(diff)) %>%
    summarise(MAE = mean(diff), .by = c(scenario, variable)) %>%
    filter(variable %in% c("CO2_concentration", "OHC", "gmst")) %>%
    ggplot() +
    geom_bar(aes(scenario, MAE, fill = scenario),  stat = "identity") +
    facet_wrap("variable", scales = "free") +
    labs(title = "Mean Abolute Error",
         subtitle = "Historical Hector vs. Obs", x = NULL) +
    theme(legend.position = "none") +
    scale_fill_manual(values = COLORS) ->
    plot; plot

ggsave(plot, filename = "figs/obs_mae.png", height = 6, width = 6)

# wide_obs_v_hector %>%
#     filter(!is.na(diff)) %>%
#     filter(variable == CONCENTRATIONS_CO2()) %>%
#     ggplot(aes(year, diff, color = scenario)) +
#     geom_line() +
#     labs(title = "Abosolute Error",
#          y = "Absolute Error",
#          subtitle = CONCENTRATIONS_CO2())
#
# wide_obs_v_hector %>%
#     filter(variable == "OHC") %>%
#     filter(!is.na(diff)) %>%
#     ggplot(aes(year, diff, color = scenario)) +
#     geom_line() +
#     labs(title = "Abosolute Error",
#          y = "Absolute Error",
#          subtitle = "ocean heat content")
#
# wide_obs_v_hector %>%
#     filter(variable == GMST()) %>%
#     filter(!is.na(diff)) %>%
#     ggplot(aes(year, diff, color = scenario)) +
#     geom_line() +
#     labs(title = "Abosolute Error",
#          y = "Absolute Error",
#          subtitle = GMST())

# 6. SSP scenarios ------------------------------------------------------------
# Get the MAE between the SSP scenarios to include on the
# plots
hector_ssps %>%
    filter(year > 2016) %>%
    pivot_wider(names_from = source, values_from = value) %>%
    na.omit %>%
    summarise(MAE = mean(abs(v32 - `V3.5.0`)),
              .by = c(scenario, variable)) ->
    MAE_table


# Quickly plot the ssp scenario results against one another
# Args
#   hector_ssps: data frame of the hector ssp results
#   MAE_table: data frame of the mean absolute error of the future results
#   var: variable name
# Returns: ggplot with a MAE table
plot_ssp <- function(hector_data, MAE_table, VAR){

    stopifnot(length(VAR) == 1)

    MAE_table %>%
        filter(variable == VAR) %>%
        mutate(MAE = signif(MAE, digits = 3)) %>%
        select(variable, scenario, MAE) ->
        tb

    tbs <- lapply(split(tb, tb$variable), "[", -1)

    df <- tibble(x = rep(-Inf, length(tbs)),
                 y = rep(Inf, length(tbs)),
                 variable = VAR,
                 tbl = tbs)


    # How do the future scenarios compare to one another??
    hector_data %>%
        dplyr::filter(variable == VAR) ->
        to_plot

    to_plot %>%
        ggplot(aes(year, value, color = source, group = interaction(scenario, source))) +
        geom_line() +
        labs(title = VAR, y = NULL, x = NULL, caption = "hist. excluded from MAE") +
        geom_table(data = df, aes(x = x, y = y, label = tbl),
                   hjust = 0, vjust = 1) +
        theme(legend.position = "bottom", legend.title = element_blank()) ->
        out

    return(out)

}


# Save plots for everything!
unique(hector_ssps$variable) %>%
    lapply(function(v){

        p <- plot_ssp(hector_ssps, MAE_table, v)
        fname <- paste0("ssps_",v,".png")
        ggsave(p, filename = file.path("figs", fname), height = 6, width = 6)

    })



