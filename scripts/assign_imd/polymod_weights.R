
# load packages
library(readxl)
library(dplyr, warn.conflicts = FALSE)
library(readr)
options(dplyr.summarise.inform = FALSE)
library(data.table, warn.conflicts = FALSE)
library(socialmixr)
library(tidyr, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("data", "connect","connect_part.rds"),
  file.path("data", "connect","connect_contacts.rds"),
  file.path("data", "ons","ons_2022_age_structure.xlsx"),
  file.path("data", "connect","connect_contacts_formatted.rds")
) else commandArgs(trailingOnly = TRUE)

connect_part <- readRDS(.args[1]) %>% 
  mutate(`large_n_0-17` = add_u18_school + add_u18_work + add_u18_other,
         `large_n_18-64` = add_18_64_school + add_18_64_work + add_18_64_other,
         `large_n_65+` = add_65_school + add_65_work + add_65_other)

connect_contacts <- readRDS(.args[2]) %>% filter(p_id %in% unique(connect_part$p_id))

ons_data <- readxl::read_xlsx(.args[3],
                              skip = 5) %>% 
  filter(`Area name` == "UNITED KINGDOM") %>% 
  #Select columns age and population contains 2022
  select("age" = Age, contains("2022")) %>% 
  # Sum across Female and Male columns
  mutate(pop = `Mid-2022 population (Female)` + `Mid-2022 population (Male)`) %>% 
  select(age, pop)

age_breaks <- c(-Inf, 5*1:(75/5), Inf)
age_vals <- age_breaks[is.finite(age_breaks)]
age_labels <- c(paste0(c(0, age_vals[1:length(age_vals)-1]), '-', c(age_vals-1)), paste0(age_vals[length(age_vals)], '+'))

age_structure_fine <- ons_data %>%
  mutate(p_age_group = cut(age,
                           right = F,
                           breaks = age_breaks,
                           labels = age_labels)) %>% 
  group_by(p_age_group) %>%
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop))

# Function to get weights for large_n contacts from Polymod setting-specific matrices
polymod_weights <- function(
    broad_ages = c(18,65),
    fine_ages = age_vals,
    age_struc = age_structure_fine,
    locations = c('total','work','school','other')
){
  
  # add 0 to lower bounds if not there, same with 18
  fine_ages <- sort(unique(c(0, fine_ages)))
  age_labels <- c(paste0(c(fine_ages[1:length(fine_ages)-1]), '-', c(fine_ages[2:length(fine_ages)]-1)), paste0(fine_ages[length(fine_ages)], '+'))
  
  # broad ages labels 
  broad_ages <- sort(unique(c(0, broad_ages)))
  broad_age_labels <- c(paste0(c(broad_ages[1:length(broad_ages)-1]), '-', c(broad_ages[2:length(broad_ages)]-1)), paste0(broad_ages[length(broad_ages)], '+'))
  
  # overlaps between fine and broad age groups 
  overlaps <- CJ(age_labels, broad_age_labels, overlap = F)
  for(i in 1:nrow(overlaps)){
    broad_vec <- suppressWarnings(case_when(grepl('[+]', overlaps[i, broad_age_labels]) ~ 
                                              c(as.numeric(gsub('[+]','',overlaps[i, broad_age_labels])), 120),
                                            T ~ as.numeric(unlist(strsplit(overlaps[i, broad_age_labels],'-')))))
    c_vec <- suppressWarnings(case_when(grepl('[+]', overlaps[i, age_labels]) ~ 
                                          c(as.numeric(gsub('[+]','',overlaps[i, age_labels])), 120),
                                        T ~ as.numeric(unlist(strsplit(overlaps[i, age_labels],'-')))))
    
    if(length(intersect(c(broad_vec[1]:broad_vec[2]), c(c_vec[1]:c_vec[2]))) > 0){
      overlaps[i, overlap := T]
    }
  }
  
  # switch between age label styles (ukscs vs polymod)
  age_to_pm_lab <- function(chars){
    out <- c()
    for(char in chars){
      if(char == paste0(max(fine_ages), '+')){
        out <- c(out, char)
      }else{
        vals <- as.numeric(unlist(strsplit(char, '-')))
        val <- paste0('[',vals[1], ',', vals[2] + 1,')')
        out <- c(out, val)
      }
    }
    out
  }
  
  # contact matrices list
  matrices <- list()
  
  # per capita location-specific contact matrix, scaled up to 2025 population
  for(i in 1:length(locations)){
    location <- locations[i]
    filter_locn <- list(a = 1)
    names(filter_locn) <- paste0('cnt_', location)
    
    if(location == 'other'){
      out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                        per.capita = TRUE,
                                        filter = list(cnt_transport = 1), 
                                        missing.participant.age = 'remove',
                                        missing.contact.age = 'remove')$matrix.per.capita
      out <- out + socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                              per.capita = TRUE,
                                              filter = list(cnt_leisure = 1), 
                                              missing.participant.age = 'remove',
                                              missing.contact.age = 'remove')$matrix.per.capita
      out <- out + socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                              per.capita = TRUE,
                                              filter = list(cnt_otherplace = 1), 
                                              missing.participant.age = 'remove',
                                              missing.contact.age = 'remove')$matrix.per.capita
    }else{
      if(location=='total'){
        out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                          per.capita = TRUE,
                                          missing.participant.age = 'remove',
                                          missing.contact.age = 'remove')$matrix.per.capita  
      }else{
        out <- socialmixr::contact_matrix(polymod, countries = "United Kingdom", age.limits = fine_ages, 
                                          per.capita = TRUE,
                                          filter = filter_locn, 
                                          missing.participant.age = 'remove',
                                          missing.contact.age = 'remove')$matrix.per.capita    
      }
    }
    
    # scale up to 2022 population 
    for(age in 1:nrow(age_struc)){
      out[, age] <- out[, age]*age_struc$n[age]
    }
    
    matrices[[i]] <- out
  }
  
  names(matrices) <- locations
  
  # polymod weights for each p_age_group, c_age_group, c_location
  pmw <- data.table(CJ(p_age_group = age_labels, c_age_group = age_labels, 
                       broad_age_group = broad_age_labels, c_location = locations, prob = -8, sorted=F))
  
  for(i in 1:nrow(pmw)){
    # if no overlap, prob = 0
    if(overlaps[age_labels == pmw[i, c_age_group] & broad_age_labels == pmw[i, broad_age_group], overlap] == F){
      pmw[i, prob := 0]
    }else{
      matr <- data.table(matrices[[pmw[i, c_location]]]) # location-specfic matrix
      row <- matr[which(age_labels == pmw[i, p_age_group]), ] # participant's age row
      ages_to_subset <- overlaps[broad_age_labels == pmw[i, broad_age_group] & overlap == T, age_labels]
      ages_to_subset_pm <- age_to_pm_lab(ages_to_subset)
      subset_row <- row[,..ages_to_subset_pm] # contacts with feasible ages
      if(rowSums(subset_row) == 0){ # if all 0, allocated according to population size 2025
        filt_age <- data.table(age_struc[age_struc$p_age_group %in% ages_to_subset, ])
        # scale 15-19 value by 60% if broad age group is 0-17
        if(pmw[i, broad_age_group] == '0-17'){
          filt_age[p_age_group == '15-19', n := 0.6*n]
          pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
        }else{
          # scale 15-19 value by 40% if broad age group is 18-64
          if(pmw[i, broad_age_group] == '18-64'){
            filt_age[p_age_group == '15-19', n := 0.4*n]
            pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
          }else{
            pmw[i, prob := filt_age[p_age_group == pmw[i, c_age_group], n]/sum(filt_age$n)]  
          }
        }
      }else{
        col <- age_to_pm_lab(pmw[i, c_age_group])
        # scale 15-19 value by 60% if broad age group is 0-17
        if(pmw[i, broad_age_group] == '0-17'){
          subset_row[, '[15,20)'] <- 0.6*subset_row[, '[15,20)']
          suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
        }else{
          # scale 15-19 value by 40% if broad age group is 18-64
          if(pmw[i, broad_age_group] == '18-64'){
            subset_row[, '[15,20)'] <- 0.4*subset_row[, '[15,20)']
            suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
          }else{
            suppressWarnings(pmw[i, prob := subset_row[,..col]/rowSums(subset_row)])
          }
        }
      }
    }
  }
  
  # check all appropriate sums are 1
  test <- pmw %>% group_by(p_age_group, broad_age_group, c_location) %>% summarise(s=sum(prob)) 
  if(!all.equal(test$s, rep(1,nrow(test)))){warning('Some sums not 1')}
  
  pmw
}

p_weights <- polymod_weights() 

# format Connect contacts

contacts_by_age <- connect_contacts %>%
  group_by(p_id, c_age_group) %>% 
  summarise(n_cag = n()) %>% 
  ungroup() %>% group_by(c_age_group) %>% 
  complete(p_id = unique(connect_part$p_id),
           fill = list(n_cag = 0)) %>% 
  ungroup() %>% arrange(p_id) %>% 
  pivot_wider(values_from = n_cag,
              names_from = c_age_group,
              names_prefix = 'cag_') 

# add large group contacts

for(p_age_group_in in unique(p_weights$p_age_group)){
  for(c_age_group_in in unique(p_weights$c_age_group)){
    
    weights_filt <- p_weights %>% 
      filter(p_age_group == p_age_group_in,
             c_age_group == c_age_group_in)
    
    ids <- unique((connect_part %>% filter(p_age_group == p_age_group_in))$p_id)
    
    large_n_to_add <- connect_part %>% 
      filter(p_id %in% ids) %>% 
      select(p_id, starts_with('add_')) %>% 
      unique() %>% 
      pivot_longer(!p_id) %>% 
      mutate(broad_age_group = case_when(
        name %like% 'u18' ~ '0-17',
        name %like% '18_64' ~ '18-64',
        name %like% '65' ~ '65+'
      ), c_location = case_when(
        name %like% 'school' ~ 'school',
        name %like% 'work' ~ 'work',
        name %like% 'other' ~ 'other'
      )) %>% select(!name) %>% 
      left_join(weights_filt %>% select(broad_age_group, c_location, prob),
                by = c('broad_age_group', 'c_location')) %>% 
      mutate(added_value = value*prob) %>% 
      group_by(p_id) %>% summarise(added_value = sum(added_value)) %>% 
      complete(p_id = unique(connect_part$p_id),
               fill = list(added_value = 0))
    
    var_cag <- c(paste0('cag_', c_age_group_in))
    
    contacts_by_age <- contacts_by_age %>% 
      left_join(large_n_to_add, by = 'p_id') %>% 
      mutate(new_var_cag = !!sym(var_cag) + added_value) %>% 
      select(!c(!!sym(var_cag), added_value)) %>% 
      setnames('new_var_cag', var_cag)
    
  }
}

write_rds(contacts_by_age, file = .args[4])

