
## SAMPLE LARGE GROUP CONTACT AGES ##

# load packages
library(data.table)
library(readr)
library(stats)
library(readxl)
library(MASS, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(socialmixr)

# set arguments
.args <- if (interactive()) c(
  file.path("data", "ons","age_ethn_sex.xlsx"),
  file.path("output", "data", "cont_matrs","reconnect_weights.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN ONS AGE STRUCTURE ####

age_ethn_sex_raw <- readxl::read_xlsx(.args[1])

age_ethn_sex <- age_ethn_sex_raw %>% 
  filter(Countries == 'England') %>% 
  select(Age, Ethnicity, Sex, Observation) %>% 
  mutate(Age = parse_number(Age),
         Ethnicity = case_when(
           Ethnicity %like% 'Asian' ~ 'Asian',
           Ethnicity %like% 'Black' ~ 'Black',
           Ethnicity %like% 'Mixed' ~ 'Mixed',
           Ethnicity %like% 'White' ~ 'White',
           T ~ 'Other'
         )) %>% 
  mutate('p_age_group' = cut(Age,
                             breaks = age_breaks,
                             labels = age_labels,
                             right = F)) %>% 
  select(!Age) %>% 
  rename(p_ethnicity = Ethnicity,
         p_gender = Sex) %>% 
  group_by(p_age_group, p_ethnicity, p_gender) %>% summarise(value = sum(Observation)) %>% ungroup() %>% 
  mutate(proportion = value/sum(value)) %>% complete(p_age_group, p_ethnicity, p_gender,
                                                     fill = list(value = 0, proportion = 0))

age_structure_fine <- age_ethn_sex_raw %>% 
  filter(Countries == 'England') %>% 
  select(Age, Observation) %>% 
  mutate(Age = parse_number(Age)) %>% 
  mutate('p_age_group' = cut(Age,
                             breaks = age_breaks,
                             labels = age_labels,
                             right = F)) %>% 
  select(!Age) %>% 
  group_by(p_age_group) %>% summarise(n = sum(Observation)) %>% ungroup() %>% 
  mutate(proportion = n/sum(n)) %>% 
  complete(p_age_group, fill = list(n = 0, proportion = 0))

#### MAKE RECONNECT DATA ####

reconnect_part <- readRDS(file.path("data", "reconnect","reconnect_part.rds"))
reconnect_contact <- readRDS(file.path("data", "reconnect","reconnect_contacts.rds"))

# merge
rc <- reconnect_contact %>% select(p_id, c_id, c_age_group, c_location) %>% 
  left_join(reconnect_part %>% 
              select(p_id, p_age_group, p_gender, p_ethnicity, day_week), by = 'p_id') %>% 
  filter(!is.na(p_age_group)) %>% # remove anyone not in England
  group_by(p_id, p_age_group, p_gender, p_ethnicity, day_week, c_age_group, c_location) %>% 
  summarise(n = n()) %>% # sum
  ungroup() %>% 
  complete(nesting(p_id = reconnect_part$p_id,
                   p_age_group = reconnect_part$p_age_group,
                   p_gender = reconnect_part$p_gender,
                   p_ethnicity = reconnect_part$p_ethnicity,
                   day_week = reconnect_part$day_week),
           c_age_group = unique(reconnect_contact$c_age_group),
           c_location = unique(reconnect_contact$c_location),
           fill = list(n = 0)) # add in those with no c_age_group/c_location-specific contacts

# add 'total' location
rc <- rbind(
  rc,
  rc %>% group_by(p_id, p_age_group, p_gender, p_ethnicity, day_week, c_age_group) %>% 
    summarise(n = sum(n)) %>% mutate(c_location = 'total')
) 

# weighting function 
participant_weights <- weight_participants(
  participant_input = reconnect_part, 
  eth_age_sex_structure = age_ethn_sex,
  weighting = c('p_gender','p_ethnicity','day_week'),
  group_vars = 'p_age_group',
  truncation_percentile = c(0.05,0.95)
)

# add weights
rc <- rc %>% 
  left_join(participant_weights %>% select(p_id, post_strat_weight),
            by = 'p_id') %>% 
  mutate(c_location = tolower(c_location))

rc <- data.table(rc)

# model to fit negative binomial GLM for data in one c_location
neg_bin_model <- function(locn){
  
  glm <- glm.nb(data = rc[c_location == locn, ],
         n ~ p_age_group:c_age_group + 0,
         weights = post_strat_weight)
  
  out <- data.table(
    c_location = locn,
    p_var = gsub('p_age_group','',gsub("\\:.*", "",names(glm$coefficients))),
    c_var = gsub('c_age_group','',gsub(".*:","",names(glm$coefficients))),
    n = exp(glm$coefficients)
  )
  
  out
  
}

# fit neg bin glm for all locations
glm_list <- map(
  .x = c('total','school','work','other'),
  .f = neg_bin_model
)

# merge outputs
reconnect_data <- rbindlist(glm_list) %>% rename(mean_mu = n)

#### MAKE RECONNECT WEIGHTS ####

r_weights <- reconnect_weights_fcn(
  data = reconnect_data
  )

#### SAVE RDS ####

write_rds(r_weights, .args[2])

#### plot difference between Reconnect and Polymod ####

p_weights <- readRDS(file.path("data", "ons","polymod_weights.rds"))
r_weights

dat <- rbind(p_weights %>% mutate(study = 'Polymod'),
             r_weights %>% mutate(study = 'Reconnect'))
dat$p_age_group <- factor(dat$p_age_group, levels = unique(dat$p_age_group))
dat$c_age_group <- factor(dat$c_age_group, levels = unique(dat$c_age_group))

dat %>% 
  ggplot() + 
  geom_line(aes(x = c_age_group, y = prob, col = study, lty = broad_age_group, 
                group = interaction(study, c_location, broad_age_group))) +
  facet_grid(p_age_group ~ c_location, switch = 'y') +
  scale_linetype_manual(values = c(2,1,4)) + 
  theme_bw() +
  labs(col = 'Study', lty = 'Broad age group', x = 'Contact age group', y = 'Participant age group') +
  theme(text = element_text(size = 14),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(file.path('output','figures','cont_matrs','large_n_age','age_distr_diff.png'),
       height = 12, width = 10)



