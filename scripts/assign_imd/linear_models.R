
#### MODEL SELECTION ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(stats)
suppressPackageStartupMessages(library(car))
library(MASS, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))

# set arguments 
.args <- if (interactive()) c(
  file.path("data", "connect", "connect_part.rds"),
  file.path("data", "connect", "connect_contacts.rds")
) else commandArgs(trailingOnly = TRUE)

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))
# source colors
source(file.path("scripts", "setup", "colors.R"))

# read in connect data 
connect_part <- readRDS(.args[1]) %>% 
  filter(p_country == 'England')

connect_cont <- readRDS(.args[2]) %>% 
  filter(p_id %in% unique(connect_part$p_id))

prop_u18 <- connect_cont %>% 
  group_by(p_id) %>% 
  summarise(n_u18 = sum(c_age < 18))

connect_part <- connect_part %>% 
  left_join(prop_u18, by ='p_id') %>% 
  mutate(n_u18 = case_when(is.na(n_u18) ~ 0, T ~ n_u18)) %>% 
  mutate(total_n_u18 = (n_u18 + add_u18_school + add_u18_work + add_u18_other)) %>% 
  mutate(prop_u18 = (n_u18 + add_u18_school + add_u18_work + add_u18_other)/(n_contacts + large_n)) 

# run models

# number of contacts

lm_n <- glm.nb(data = connect_part,
               formula = (n_contacts + large_n) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + hh_tenure_nm + p_hiqual + 
                          p_sec_input + p_urban_rural)

# proportion of contacts u18

lm_u <- glm(data = connect_part,
            formula = cbind(total_n_u18, (n_contacts + large_n - total_n_u18)) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + hh_tenure_nm + p_hiqual + 
                      p_sec_input + p_urban_rural,
            family = binomial)

#################################################### 

# run anova

summ_n <- data.frame(cat = names(coef(summary(lm_n))[,4]),
                     value = unname(coef(summary(lm_n))[,1]),
                     st.err = unname(coef(summary(lm_n))[,2]),
                     p = unname(coef(summary(lm_n))[,4])) %>% 
  mutate(var = case_when(
    grepl('age_grp',cat) ~ 'age',
    grepl('sec_',cat) ~ 'nssec',
    grepl('size',cat) ~ 'hh_size',
    grepl('tenure',cat) ~ 'hh_tenure',
    grepl('hiqual',cat) ~ 'hiqual',
    grepl('ethnicity',cat) ~ 'ethnicity',
    grepl('engreg',cat) ~ 'region',
    grepl('urban',cat) ~ 'urban_rural'
  ),
  model = 'n_contacts')

summ_u <- data.frame(cat = names(coef(summary(lm_u))[,4]),
                     value = unname(coef(summary(lm_u))[,1]),
                     st.err = unname(coef(summary(lm_u))[,2]),
                     p = unname(coef(summary(lm_u))[,4])) %>% 
  mutate(var = case_when(
    grepl('age_grp',cat) ~ 'age',
    grepl('sec_',cat) ~ 'nssec',
    grepl('size',cat) ~ 'hh_size',
    grepl('tenure',cat) ~ 'hh_tenure',
    grepl('hiqual',cat) ~ 'hiqual',
    grepl('ethnicity',cat) ~ 'ethnicity',
    grepl('engreg',cat) ~ 'region',
    grepl('urban',cat) ~ 'urban_rural'
  ),
  model = 'prop_u18')

summ_df <- rbind(summ_n, summ_u) %>% filter(! cat %like% 'Interc')

summ_df <- summ_df %>% 
  mutate(cat = gsub('p_urban_rural','',
                         gsub('p_engreg','',
                              gsub('p_sec_input','',
                                   gsub('p_hiqual','',
                                        gsub('hh_tenure_nm','',
                                             gsub('hh_size_nm','',
                                                  gsub('p_ethnicity','',
                                                       gsub('age_grp','',cat)))))))))

summ_df %>% 
  ggplot(aes(x = var, y = p, col = var)) + 
  geom_point(size = 3, shape = 4) + 
  labs(x = 'Variable',
       y = 'p',
       col = '') +
  scale_color_brewer(palette = 'Paired') + 
  theme_bw() +
  facet_grid(model ~ ., scales = 'free') + 
  ylim(c(0,NA)) +
  # scale_y_log10() + 
  theme(text = element_text(size = 14))

plot_lm_values <- function(variable){
  
  summ_df_filt <- summ_df %>% 
    filter(var == variable) 
  
  if(variable == 'age'){
    summ_df_filt$cat <- factor(summ_df_filt$cat,
                               levels = c('Aged 4 years and under', 'Aged 5 to 9 years',
                                          'Aged 10 to 14 years', 'Aged 15 to 19 years',
                                          'Aged 20 to 24 years', 'Aged 25 to 29 years',
                                          'Aged 30 to 34 years', 'Aged 35 to 39 years',
                                          'Aged 40 to 44 years', 'Aged 45 to 49 years',
                                          'Aged 50 to 54 years', 'Aged 55 to 59 years',
                                          'Aged 60 to 64 years', 'Aged 65 to 69 years',
                                          'Aged 70 to 74 years', 'Aged 75+'))
  }
  
  plot <- summ_df_filt %>% 
    ggplot(aes(y = cat, x = value, col = var)) + 
    geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
    geom_errorbar(aes(y = cat, xmin = value - 1.96*st.err, 
                      xmax = value + 1.96*st.err)) + 
    geom_point(size = 3) + 
    labs(x = '',
         y = 'p',
         col = '') +
    scale_color_manual(values = variable_colors) + 
    theme_bw() + 
    xlim(c(min, max)) + 
    facet_grid(var ~ model, scales = 'free') + 
    scale_y_discrete(limits=rev) + 
    theme(text = element_text(size = 14),
          legend.position="none",
          axis.title.y=element_blank(),
          # axis.text.y=element_blank(),            
          axis.ticks.y=element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_text(angle = 0)); plot
  
  if(variable %notin% unique(summ_df$var)[1:2]){
    
    plot <- plot + 
      theme(strip.text.x = element_blank())
    
  }
  
  if(variable != unique(summ_df$var)[length(unique(summ_df$var))]){
    
    plot <- plot + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),            
            axis.ticks.x=element_blank())
    
  }
  
  plot
  
}

min <- min(summ_df$value - 1.96*summ_df$st.err); max <- max(summ_df$value + 1.96*summ_df$st.err)

lm_plots <- map(
  .x = unique(summ_df$var),
  .f = plot_lm_values
)

layout <- '
AABB
AABB
AABB
AAFF
AAFF
CCFF
CCGG
CCGG
DDGG
DDEE
DDEE
HHEE
'

patchwork::wrap_plots(lm_plots, ncol = 2, design = layout)

ggsave(here::here('output','figures','assignment','values_lm.png'),
       width = 14, height = 8)

# #################################################### 
# 
# # run anova
# 
# anova_n <- data.frame(Anova(lm_n, type = 2)) %>% rename(pr = `Pr..Chisq.`) 
# anova_u <- data.frame(Anova(lm_u, type = 2)) %>% rename(pr = `Pr..Chisq.`)
# 
# # ensure they align to the same variables
# vars_n <- rownames(anova_n) 
# vars_u <- rownames(anova_u) 
# if(sum(vars_n == vars_u) == length(vars_n)){vars <- vars_n}else{warning('vars_n neq vars_u')}
# 
# anova_n <- anova_n %>% mutate(var = vars)
# anova_u <- anova_u %>% mutate(var = vars)
# 
# w_n <- anova_n$pr
# w_u <- anova_u$pr
# 
# w_df <- data.frame(variable = vars, 
#                    n_contacts = w_n, 
#                    prop_u18 = w_u)
# 
# w_df %>% 
#   pivot_longer(!variable) %>% 
#   ggplot(aes(x = variable, y = value, col = name)) + 
#   geom_point(size = 3) + 
#   labs(x = 'Variable',
#        y = 'p',
#        col = '') +
#   scale_color_brewer(palette = 'Paired') + 
#   theme_bw() +
#   # ylim(c(0,NA)) +
#   scale_y_log10() +
#   theme(text = element_text(size = 14))


