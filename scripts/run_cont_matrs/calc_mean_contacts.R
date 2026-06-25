#### MEAN CONTACTS BY IMD — PATCH FIGURE ####

library(data.table)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(purrr)
library(MASS)
library(ggplot2)
library(patchwork)
library(viridis)

#### arguments & sources ####

.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs", "base", "participants.rds"),
  "nhs_ages",
  file.path("output", "data", "cont_matrs", "nhs_ages", "mean_contacts", "mean_contacts.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here("scripts", "run_cont_matrs", "cont_matr_fcns.R"))
source(here::here("scripts", "setup", "colors.R"))

sens_analysis <- .args[2]

colors <- if (sens_analysis == "nhs_ages") colors_p_age_group_nhs else colors_p_age_group

## if NHS age groups, change age groups
if(sens_analysis == 'nhs_ages'){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}

#### CI helpers ####

eti95L <- function(x) quantile(x, 0.025)
eti95U <- function(x) quantile(x, 0.975)
eti50L <- function(x) quantile(x, 0.25)
eti50U <- function(x) quantile(x, 0.75)

summarise_boots <- function(dt, group_vars) {
  dt <- data.table(dt)[, bootstrap_index := NULL]
  rbind(
    dt[, lapply(.SD, mean),   by = group_vars][, measure := "mean"],
    dt[, lapply(.SD, eti95L), by = group_vars][, measure := "lower"],
    dt[, lapply(.SD, eti95U), by = group_vars][, measure := "upper"],
    dt[, lapply(.SD, eti50L), by = group_vars][, measure := "lower50"],
    dt[, lapply(.SD, eti50U), by = group_vars][, measure := "upper50"]
  )
}

#### plot constants ####

updown      <- 0.07
black_alpha <- 0.05
shade95     <- 0.4
shade50     <- 0.7

#### core plot function ####
# Draws one panel in the shared style.
# `x_var`   : column name for x-axis (string)
# `x_label` : x-axis label
# `facet_var`: optional column name to facet by (string or NULL)
# `y_limits` : passed to scale_y_continuous limits
# `legend`   : TRUE/FALSE

make_panel <- function(plot_df,
                       x_var,
                       x_label    = x_var,
                       facet_var  = NULL,
                       y_limits   = c(0, NA),
                       legend     = FALSE) {
  
  x_sym <- sym(x_var)
  
  p <- plot_df %>%
    ggplot(aes(
      x     = !!x_sym,
      group = as.factor(imd_quintile),
      col   = as.factor(imd_quintile)
    )) +
    geom_errorbar(aes(ymin = lower,            ymax = upper),
                  width = 0, linewidth = 2, col = 'white',
                  position = position_dodge(0.9), alpha = 1) +
    geom_errorbar(aes(ymin = lower,            ymax = upper),
                  width = 0, linewidth = 2,
                  position = position_dodge(0.9), alpha = shade95) +
    geom_errorbar(aes(ymin = lower50,          ymax = upper50),
                  width = 0, linewidth = 3,
                  position = position_dodge(0.9), alpha = shade50) +
    geom_errorbar(aes(ymin = mean - updown,    ymax = mean + updown),
                  width = 0, linewidth = 4,
                  position = position_dodge(0.9), alpha = 1) +
    geom_errorbar(aes(ymin = mean - updown,    ymax = mean + updown),
                  width = 0, linewidth = 4,
                  position = position_dodge(0.9), alpha = black_alpha, col = "black") +
    scale_color_manual(values = imd_quintile_colors) +
    scale_y_continuous(limits = y_limits, expand = expansion(c(0.00075, 0.025))) +
    theme_bw() +
    theme(
      text          = element_text(size = 16),
      axis.ticks    = element_line(linewidth = 0.25),
      legend.position = if (legend) "top" else "none"
    ) +
    labs(col = "IMD quintile", y = "Mean contacts", x = x_label)
  
  if (!is.null(facet_var)) p <- p + facet_wrap(reformulate(facet_var), scales = "free")
  
  p
}

#### panel configuration ####
# Each entry defines one panel in the patch figure.
# `group_vars`  : variables passed to neg_bin_fcn / summarise_boots
# `group_vars_plot`  : variables after pre_process
# `csv_suffix`  : suffix for the cached CSV (must be unique)
# `x_var`       : x-axis column in the summarised data
# `x_label`     : x-axis label
# `facet_var`   : optional facet column (NULL for none)
# `y_limits`    : y-axis limits
# `legend`      : show legend?
# `pre_process` : optional function(dt) applied before make_panel
# Add or remove list entries here to change the figure panels.

panel_specs <- list(
  
  imd_only = list(
    group_vars   = c("imd_quintile", "p_age_group"),
    group_vars_plot   = c("imd_quintile"),
    csv_suffix   = "imd_quintile",
    x_var        = "imd_quintile",
    x_label      = "IMD quintile",
    y_limits     = c(0, 12),
    legend       = FALSE,
    pre_process  = function(dt) {
      # weighted mean over age groups using census age structure
      imd_age <- read_csv(
        file.path("data", "imd_25",
                  paste0("imd_ages_", ifelse(grepl("nhs", sens_analysis), 2, 1), ".csv")),
        show_col_types = FALSE
      ) %>%
        group_by(imd_quintile, age_grp) %>%
        summarise(pop = sum(pop), .groups = "drop") %>%
        group_by(imd_quintile) %>%
        mutate(prop = pop / sum(pop)) %>%
        rename(p_age_group = age_grp)
      
      dt %>%
        left_join(imd_age, by = c("imd_quintile", "p_age_group")) %>%
        group_by(imd_quintile, bootstrap_index) %>%
        summarise(n = weighted.mean(n, prop), .groups = "drop")
    }
  ),
  
  imd_income = list(
    group_vars   = c("imd_quintile", "p_income", "p_broad_age"),
    group_vars_plot   = c("imd_quintile", "p_income"),
    csv_suffix   = "imd_quintile_p_income_p_broad_age",
    x_var        = "p_income",
    x_label      = "Household income",
    y_limits     = c(0, 25),
    legend       = F,
    pre_process  = function(dt) {
      income_levels <- c("Less than £20,000", "£20,000 - £39,999",
                         "£40,000 - £59,999", "£60,000 - £100,000", "Over £100,000")
      gsub_income_levels <- gsub("- ", "-\n", gsub("than", "than\n", income_levels))
      dt <- dt %>%
        filter(p_broad_age == "Adult",
               !grepl("Applic", p_income)) %>% select(!p_broad_age) %>% 
        mutate(
          p_income = factor(p_income, levels = income_levels),
          p_income = gsub("- ", "-\n", gsub("than", "than\n", as.character(p_income))),
          p_income = factor(p_income, levels = gsub_income_levels)
        )
      
      dt
    }
  ),
  
  imd_age = list(
    group_vars   = c("imd_quintile", "p_age_group"),
    group_vars_plot   = c("imd_quintile", "p_age_group"),
    csv_suffix   = "imd_quintile_p_age_group",
    x_var        = "p_age_group",
    x_label      = "Age group",
    y_limits     = c(0, 25),
    legend       = T,
    pre_process  = function(dt) {
      dt %>% mutate(p_age_group = factor(p_age_group, levels = age_labels))
    }
  ),
  
  imd_age_reg = list(
    group_vars   = c("imd_quintile", "p_age_group", "p_engreg"),
    group_vars_plot   = c("imd_quintile", "p_age_group", "p_engreg"),
    csv_suffix   = "imd_quintile_p_age_group_p_engreg",
    x_var        = "p_age_group",
    x_label      = "Age group",
    y_limits     = c(0, NA),
    facet_var    = "p_engreg",
    legend       = T,
    pre_process  = function(dt) {
      dt %>% mutate(p_age_group = factor(p_age_group, levels = age_labels))
    }
  ),
  
  imd_hiqual = list(
    group_vars   = c("imd_quintile", "p_hiqual"),
    group_vars_plot   = c("imd_quintile", "p_hiqual"),
    csv_suffix   = "imd_quintile_p_hiqual",
    x_var        = "p_hiqual",
    x_label      = "Highest qualification",
    y_limits     = c(0, NA),
    legend       = T,
    pre_process  = function(dt) {
      dt 
    }
  )
  
  # ── add further panels here, e.g.: ──────────────────────────────────────────
  # imd_gender = list(
  #   group_vars  = c("imd_quintile", "p_gender"),
  #   csv_suffix  = "imd_quintile_p_gender",
  #   x_var       = "p_gender",
  #   x_label     = "Gender",
  #   y_limits    = c(0, NA),
  #   legend      = FALSE,
  #   pre_process = NULL
  # ),
)

#### output folders ####

output_folder      <- file.path("output", "data",    "cont_matrs", sens_analysis, "mean_contacts")
output_folder_figs <- file.path("output", "figures", "cont_matrs", sens_analysis, "mean_contacts")
dir.create(output_folder,      showWarnings = FALSE, recursive = TRUE)
dir.create(output_folder_figs, showWarnings = FALSE, recursive = TRUE)

csv_path <- function(suffix) gsub(".csv", paste0("_", suffix, ".csv"), .args[3])

#### load & prepare participant data ####

part <- readRDS(.args[1])

part_reconnect <- readRDS(file.path("data", "reconnect", "reconnect_part.rds"))

part <- part %>%
  left_join(
    part_reconnect %>% select(p_id, p_gender, day_week, p_income, p_engreg, p_broad_age, p_hiqual),
    by = "p_id"
  ) %>%
  mutate(total_contacts = n_contacts + large_n)

if (sens_analysis == "nhs_ages") {
  part <- part %>%
    mutate(p_age_group = cut(p_age,
                             breaks = c(0, age_limits, Inf),
                             labels = age_labels,
                             right  = FALSE))
}

#### fit or read each panel's data ####

fit_panel <- function(spec) {
  group_vars_bs <- c(spec$group_vars, "bootstrap_index")
  part_dt <- data.table(part)[, .SD, .SDcols = c(group_vars_bs, "total_contacts")]
  out <- part_dt[, lapply(.SD, neg_bin_fcn), by = group_vars_bs]
  out[, k := as.numeric(sub("^[^_]*_", "",  total_contacts))]
  out[, n := as.numeric(sub("(.*)_.*", "\\1", total_contacts))]
  out[, total_contacts := NULL]
  out
}

get_panel_data <- function(spec) {
  f <- csv_path(spec$csv_suffix)
  if (file.exists(f)) {
    data.table(read_csv(f, show_col_types = FALSE))
  } else {
    dt <- fit_panel(spec)
    write_csv(as.data.frame(dt), f)
    dt
  }
}

panel_data <- map(panel_specs, get_panel_data)

#### build plot for each panel ####

build_panel_plot <- function(spec, dt) {
  if (!is.null(spec$pre_process)) dt <- spec$pre_process(dt)
  
  if('k' %in% colnames(dt)) dt <- dt %>% select(!k)
  
  plot_df <- summarise_boots(dt, spec$group_vars_plot) %>%
    pivot_wider(names_from = measure, values_from = n)
  
  make_panel(plot_df,
             x_var     = spec$x_var,
             x_label   = spec$x_label,
             facet_var = spec$facet_var %||% NULL,
             y_limits  = spec$y_limits,
             legend    = spec$legend)
}

plots <- map2(panel_specs, panel_data, build_panel_plot)

## also plotting (weighted) proportion of participants in each IMD quintile
# weighting by 5-year age group
{
  age_limits_2 <- seq(5,75,5)
  age_labels_2 <- paste0(c(0,age_limits_2), c(rep('-', length(age_limits_2)),''), c(age_limits_2 - 1, '+'))
  age_structure <- read_csv(file.path('data','imd_25','imd_ages_1.csv'), show_col_types=F)
  demog_age <- age_structure %>% group_by(age_grp) %>% 
    summarise(pop = sum(pop)) %>% ungroup() %>% 
    mutate(prop = pop/sum(pop)) %>% rename(p_age_group = age_grp)
  
  summ_part <- part %>% 
    mutate(p_age_group = cut(p_age,breaks = c(0,age_limits_2,Inf),
                             labels = age_labels_2,right = F)) %>% 
    group_by(p_age_group, bootstrap_index, imd_quintile) %>% 
    count() %>% group_by(p_age_group, bootstrap_index) %>% 
    mutate(p = prop.table(n)) %>% 
    left_join(demog_age, by = 'p_age_group') %>% 
    group_by(imd_quintile, bootstrap_index) %>% 
    summarise(p = weighted.mean(x=p, w=prop)) %>% 
    group_by(imd_quintile) %>% 
    summarise(m = mean(p), l = quantile(p, 0.025), u = quantile(p, 0.975)) %>% 
    mutate(neat = paste0(round(100*m,2), ' (', round(100*l,2), ' - ', round(100*u,2), ')'))
  
  write_csv(summ_part, file.path('output','data','cont_matrs','weighted_imd_proportions.csv'))
  
  imd_props <- summ_part %>% 
    ggplot() + 
    geom_errorbar(aes(x=imd_quintile, ymin=l, ymax=u, col=as.factor(imd_quintile)),
                  width = 0.4) +
    geom_point(aes(x=imd_quintile, y=m, col=as.factor(imd_quintile)),
               size = 2) + 
    scale_y_continuous(limits = c(0.15, 0.25), breaks = c(0.15,0.2,0.25),
                       expand = expansion(c(0.00075, 0.00075))) + 
    theme_bw() + scale_color_manual(values = imd_quintile_colors) +
    labs(x = 'IMD quintile', y = 'Proportion of participants') +
    theme(
      text = element_text(size = 14),
      axis.ticks = element_line(linewidth = 0.25),
      legend.position='none')
}

#### patchwork layout & save ####

layout <- "
AB
CC
DD
"

imd_props + plots[[1]] + plots[[3]] + plots[[2]] +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

ggsave(
  filename = gsub("data", "figures", gsub(".csv", "_patch.png", .args[3])),
  width = 10, height = 12
)

## regional age- and IMD-specific contacts
plots[[4]]
ggsave(
  filename = gsub("data", "figures", gsub(".csv", "_age_region.png", .args[3])),
  width = 18, height = 11
)

#### dummy output for Makefile ####

write_csv(data.table(x = 0), .args[3])
