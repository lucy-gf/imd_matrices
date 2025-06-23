
#### WHICH VARIABLES FOR MODEL SELECTION ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(stats)

# set arguments 
.args <- if (interactive()) c(
  file.path("data", "connect", "connect_part.rds")
) else commandArgs(trailingOnly = TRUE)

# read in connect data
connect_part <- readRDS(.args[1]) %>% 
  filter(p_country == 'England')

lm <- glm.nb(data = connect_part,
             formula = (n_contacts + large_n) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + hh_tenure_nm + p_hiqual + 
                       p_sec_input + p_urban_rural)

summ <- summary(lm)

out <- data.table(summ$coefficients) %>% 
  mutate(names = rownames(summ$coefficients)) %>% 
  mutate(var = case_when(
    names %like% 'Intercept' ~ 'Intercept',
    names %like% 'age_grp' ~ 'age_grp',
    names %like% 'p_engreg' ~ 'p_engreg',
    names %like% 'p_ethnicity' ~ 'p_ethnicity',
    names %like% 'hh_size_nm' ~ 'hh_size_nm',
    names %like% 'hh_tenure_nm' ~ 'hh_tenure_nm',
    names %like% 'p_hiqual' ~ 'p_hiqual',
    names %like% 'p_sec_input' ~ 'p_sec_input',
    names %like% 'p_urban_rural' ~ 'p_urban_rural'
  ))

ggplot(out) + 
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.3) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 1) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 1) +
  geom_point(aes(x=Estimate, y = `Pr(>|z|)`,
                 col = var), size = 2) + 
  theme_bw() + facet_wrap(var ~ ., scales = 'free') 

lm <- glm.nb(data = connect_part,
             formula = (n_contacts + large_n) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + p_hiqual + 
               p_sec_input + p_urban_rural)

summ <- summary(lm)

out <- data.table(summ$coefficients) %>% 
  mutate(names = rownames(summ$coefficients)) %>% 
  mutate(var = case_when(
    names %like% 'Intercept' ~ 'Intercept',
    names %like% 'age_grp' ~ 'age_grp',
    names %like% 'p_engreg' ~ 'p_engreg',
    names %like% 'p_ethnicity' ~ 'p_ethnicity',
    names %like% 'hh_size_nm' ~ 'hh_size_nm',
    names %like% 'hh_tenure_nm' ~ 'hh_tenure_nm',
    names %like% 'p_hiqual' ~ 'p_hiqual',
    names %like% 'p_sec_input' ~ 'p_sec_input',
    names %like% 'p_urban_rural' ~ 'p_urban_rural'
  ))

out %>% 
  filter(!var %in% c('Intercept','p_engreg')) %>% 
  ggplot() + 
  geom_hline(yintercept = 0.05, lty = 2, alpha = 0.3) + 
  geom_vline(xintercept = 0, lty = 2, alpha = 1) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 1) +
  geom_point(aes(x=Estimate, y = `Pr(>|z|)`,
                 col = var), size = 2) + 
  theme_bw() + facet_wrap(var ~ ., scales = 'free') 



