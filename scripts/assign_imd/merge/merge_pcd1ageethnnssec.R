
## making merged output

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)
library(scoringutils)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_det_pcd1ageethn.rds"),
  file.path("output", "data", "assignment","connect_det_pcd1ethnnssec.rds"),
  'det',
  file.path("output", "data", "assignment","connect_det_pcd1ageethnnssec.rds")
) else commandArgs(trailingOnly = TRUE)

age_ethn <- readRDS(.args[1])
ethn_nssec <- readRDS(.args[2]) 

# merge

merged <- rbind(age_ethn %>% filter(p_age < 18 | p_age >= 65),
                ethn_nssec %>% filter(p_age >= 18 & p_age < 65))

write_rds(merged, .args[4])

