
## BALANCE CONTACT MATRICES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_0-4.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_5-9.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_10-14.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_15-19.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_20-24.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_25-29.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_30-34.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_35-39.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_40-44.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_45-49.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_50-54.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_55-59.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_60-64.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_65-69.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_70-74.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_75+.csv"),
  file.path("output", "data", "cont_matrs","regional","fitted_matrs.csv")
) else commandArgs(trailingOnly = TRUE)

#### READ IN AND SPLIT DATA ####
dummy <- data.table(read_csv(.args[1], show_col_types = F))
regions <- unique(dummy$p_engreg)

merged <- list()
for(k in 1:length(regions)){
  merged[[k]] <- data.table()
}

for(i in 1:(length(.args) - 1)){
  
  age_data <- suppressMessages(data.table(read_csv(.args[i], show_col_types = F)))
  
  for(k in 1:length(regions)){
    
    reg <- regions[k]
    age_reg_dat <- age_data[p_engreg == reg]
    
    merged[[k]] <- rbind(merged[[k]],
                         age_reg_dat)
    
  }
  
}

# dummy saved data for makefile purposes

write_csv(data.table(x=1), .args[length(.args)])

# save data

for(k in 1:length(regions)){
  
  write_csv(merged[[k]], gsub('.csv', paste0('_', regions[k], '.csv'), .args[length(.args)]))
  
}


