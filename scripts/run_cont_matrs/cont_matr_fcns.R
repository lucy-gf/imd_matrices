
###########################################
## FUNCTIONS FOR MAKING CONTACT MATRICES ##
###########################################

options(dplyr.summarise.inform = FALSE)

age_breaks <- c(-Inf, 5*1:(75/5), Inf)
age_vals <- age_breaks[is.finite(age_breaks)]
age_labels <- c(paste0(c(0, age_vals[1:length(age_vals)-1]), '-', c(age_vals-1)), paste0(age_vals[length(age_vals)], '+'))

# FUNCTION TO MAKE PARTICIPANT WEIGHTS ##
# (take this function from ukscs github)

weight_participants <- function(participant_input = part, 
                                eth_age_sex_structure = age_ethn_sex,
                                weighting = NULL,
                                group_vars = NULL,
                                truncation_percentile = c(0.05,0.95) # c(0,1)
){
  
  # align p_gender case
  participant_input <- participant_input %>% mutate(p_gender = tolower(p_gender)) 
  eth_age_sex_structure <- eth_age_sex_structure %>% mutate(p_gender = tolower(p_gender)) %>% select(!value)
  
  # add dataframe for day_week
  if('day_week' %in% weighting){
    day_week_structure <- data.table(day_week = c('weekday','weekend'),
                                     proportion = c(5/7, 2/7))
    if('c_day_of_week' %in% group_vars){
      stop("Can't weight by day_week and group by c_day_of_week")
    }
  }
  
  # specify which of age/gender/ethnicity are being weighted for
  age_gender_vec <- weighting[weighting %like% 'age|gender|ethn']
  
  # age_gender_vec plus p_adult_child if this is in the group_vars vector and p_age_group is in weighting vec
  a_g_e_grouping_vars <- if('p_adult_child' %in% group_vars & 'p_age_group' %in% weighting){
    c(age_gender_vec, 'p_adult_child')
  }else{age_gender_vec}
  
  a_g_e_grouping_vars_full <- unique(c(group_vars, a_g_e_grouping_vars))
  
  a_g_e_df <- if(length(age_gender_vec) > 0){
    if(length(group_vars) > 0){
      eth_age_sex_structure %>% 
        group_by(!!!syms(group_vars)) %>% 
        mutate(group_proportion = sum(proportion)) %>% 
        mutate(proportion = proportion/group_proportion) %>% 
        ungroup() %>% select(!group_proportion)
    }else{
      eth_age_sex_structure %>% 
        group_by(!!!syms(a_g_e_grouping_vars)) %>% 
        summarise(proportion = sum(proportion))
    }
  }else{data.frame()}
  
  # weight by age and/or gender within specified groups
  part_age <- participant_input %>% group_by(!!!syms(unique(c(group_vars, age_gender_vec)))) %>% 
    summarise(sample_totals = n()) %>% ungroup() %>% 
    group_by(!!!syms(group_vars)) %>% mutate(group_total = sum(sample_totals))
  
  if(nrow(a_g_e_df)>0){
    part_age <- part_age %>% left_join(a_g_e_df, by=(a_g_e_grouping_vars_full))
    
    not_in_prop_groups <- part_age %>% group_by(!!!syms(group_vars)) %>% 
      summarise(sum_in = sum(proportion, na.rm=T)) %>% ungroup()
    
    for(i in 1:nrow(part_age)){
      if(is.na(part_age$proportion[i]) & 'p_gender' %in% age_gender_vec){
        if((is.na(part_age$p_gender[i]) | part_age$p_gender[i] == 'other')){
          if('p_ethnicity' %in% age_gender_vec){
            if(part_age$p_ethnicity[i] %like% 'Prefer'){
              if('p_age_group' %in% a_g_e_grouping_vars){
                part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(a_g_e_df %>% group_by(p_age_group) %>% 
                                                                                  summarise(proportion = sum(proportion)) %>% 
                                                                                  left_join(part_age %>% group_by(!!!syms(a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'ethn|gender'])) %>% 
                                                                                              summarise(sample_totals = sum(sample_totals))), 
                                                                                by = a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'ethn|gender'],
                                                                                unmatched = "ignore"))
              }
            }
          }else{
            part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(a_g_e_df %>% group_by(!!!syms(a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'gender'])) %>% 
                                                                              summarise(proportion = sum(proportion)) %>% 
                                                                              left_join(part_age %>% group_by(!!!syms(a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'gender'])) %>% 
                                                                                          summarise(sample_totals = sum(sample_totals))), by = a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'gender'],
                                                                            unmatched = "ignore"))
          }
        }else{
        if(is.na(part_age$proportion[i]) & 'p_ethnicity' %in% age_gender_vec){
          if(part_age$p_ethnicity[i] %like% 'Prefer' & length(a_g_e_grouping_vars[!a_g_e_grouping_vars %like% 'ethn']) > 0){
            part_age[i, ] <- suppressMessages(part_age[i, ] %>% rows_update(a_g_e_df %>% group_by(!!!syms(a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'ethn'])) %>% 
                                                                              summarise(proportion = sum(proportion)) %>% 
                                                                              left_join(part_age %>% 
                                                                                          group_by(!!!syms(a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'ethn'])) %>% 
                                                                                          summarise(sample_totals = sum(sample_totals))), 
                                                                            by = a_g_e_grouping_vars_full[!a_g_e_grouping_vars_full %like% 'ethn'],
                                                                            unmatched = "ignore"))
          }
        }
      }
    }
    }
    }else{
      part_age <- part_age %>% mutate(proportion = NA)
  }
    
    # scale up proportions to account for missing categories
    if(length(group_vars) > 0){
      part_age <- part_age %>% left_join(not_in_prop_groups, by=group_vars) %>% 
        mutate(proportion = proportion/sum_in) %>% select(!sum_in)
    }else{
      part_age <- part_age %>% mutate(sum_in = not_in_prop_groups$sum_in[1]) %>% 
        mutate(proportion = proportion/sum_in) %>% select(!sum_in)
    }
  
  part_age <- part_age %>% mutate(group_prop = sample_totals/group_total,
                                  post_strat_weight = case_when(is.na(proportion) ~ 1,
                                                                T ~ proportion / group_prop))
  
  
  weighted_data <- participant_input %>%
    select(p_id, !!!(group_vars), !!!syms(weighting)) 
  if(length(age_gender_vec) == 0){
    weighted_data <- weighted_data %>% mutate(post_strat_weight = 1)
  }else{
    weighted_data <- weighted_data %>% 
      left_join(unique(part_age %>% select(!!!(group_vars), !!!syms(age_gender_vec), post_strat_weight)), by = unique(c(group_vars, age_gender_vec)))
  }
  
  if('day_week' %in% weighting){
    weighted_data <- weighted_data %>% left_join(day_week_structure %>% left_join(part %>% group_by(day_week, !!!syms(group_vars)) %>% 
                                                                                    summarise(sample_tot = n()) %>% ungroup() %>% 
                                                                                    group_by(!!!syms(group_vars)) %>% mutate(pop = sum(sample_tot)) %>% 
                                                                                    mutate(sample_proportion = sample_tot/pop) %>% ungroup(),
                                                                                  by = 'day_week') %>% 
                                                   mutate(weight_mult = proportion/sample_proportion) %>% select(day_week, !!!syms(group_vars), weight_mult),
                                                 by = c('day_week', group_vars)) %>% 
      mutate(post_strat_weight = case_when(
        is.na(day_week) ~ post_strat_weight,
        T ~ post_strat_weight*weight_mult)) %>% select(!weight_mult)
  }
  
  # truncate the weights that fall above the 99th percentile or below the 1% percentile
  percs <- quantile(weighted_data$post_strat_weight, truncation_percentile)
  if(n_distinct(weighted_data$post_strat_weight) >= 20){
    weighted_data <- weighted_data %>% 
      mutate(post_strat_weight = case_when(
        post_strat_weight < percs[1] ~ percs[1],
        post_strat_weight > percs[2] ~ percs[2],
        T ~ post_strat_weight
      ))
  }
  
  weighted_data
  
}


# FUNCTION TO SAMPLE PARTICIPANTS ##

fcn_sample_participants <- function(
  age
){

  selections <- part %>% filter(p_age_group == age) %>% 
    select(p_id, post_strat_weight)
  
  selected_ids <- sample(x = selections$p_id,
                         size = nrow(selections)*n_bootstraps,
                         prob = selections$post_strat_weight,
                         replace = T)
  
  selected_df <- data.frame(p_id = selected_ids,
                            p_age_group = age,
                            bootstrap_index = rep(1:n_bootstraps,
                                                  each = nrow(selections))
  )
  
  selected_df
  
}


## FUNCTION TO SAMPLE PARTICIPANT IMD ##

fcn_assign_imd_cm <- function(
  data_input,
  census_data,
  variables,
  modal = F # T = deterministic, F = probabilistic
){
  
  data <- data.table(data_input)
  census_data <- data.table(census_data)
  
  vars_of_int_census <- c(variables, 'imd_quintile')
  
  # remove participants who can't be assigned
  for(var in variables){
    data <- data[get(var) %in% unname(unlist(unique(census_data[,..var]))), ]
  }
  
  ## distribution of IMD quintiles
  probs_out <- census_data %>% group_by(!!!syms(vars_of_int_census)) %>% 
    summarise(n = sum(population)) %>% 
    group_by(!!!syms(variables)) %>% 
    mutate(n_tot = sum(n)) %>% 
    mutate(probability = n/n_tot) %>% 
    ungroup() %>% filter(probability > 0)
  
  if(!modal){
    
    data <- data.table(data)
    data_reps <- data.table()
    
    # for each possible combination of predictors,
    # only using those present in the survey data
    probs_unique <- unique(data[, ..variables])
    
    # set start time
    time <- Sys.time()
    paste0('\nStart time: ', time, '\n')
    
    for(i_row in 1:nrow(probs_unique)){
      
      # filter down to probabilities, calculate number of participants in this category
      dt <- data.table(probs_out)
      participants_filt <- data.table(data)
      for(var in variables){
        dt <- dt[get(var) == unlist(unname(probs_unique[i_row, ..var])),]
        participants_filt <- participants_filt[get(var) == unlist(unname(probs_unique[i_row, ..var])), ]
      }
      
      n_participants_filt <- nrow(participants_filt)
      
      # sample (or assign) IMD quintiles if any connect participants in category
      if(n_participants_filt > 0){
        if(nrow(dt) > 1){
          sampled_imd <- sample(dt$imd_quintile, n_participants_filt, 
                                replace = T, prob = dt$probability)
        }else{
          if(nrow(dt) == 1){
            sampled_imd <- rep(dt$imd_quintile, n_participants_filt)
          }else{
            sampled_imd <- rep(0, n_participants_filt)
          }
        }
        
        data_reps <- rbind(data_reps,
                           participants_filt %>% 
                             mutate(imd_quintile = sampled_imd))
      }
      
      if(i_row == 100){
        n <- (nrow(probs_unique)/100)
        cat('\nExpected time: ', 
            floor(n*difftime(Sys.time(), time, units = 'secs')[[1]]/60), ' mins ',
            round(n*difftime(Sys.time(), time, units = 'secs')[[1]] - 
                    60*floor(n*difftime(Sys.time(), time, units = 'secs')[[1]]/60)), ' secs',
            sep = '')
      }
      if(i_row %% 200 == 0){
        cat('\nRows done = ', i_row, '/', nrow(probs_unique), sep = '')
      }
      
    }
    
    out <- data_reps[imd_quintile != 0,]
    
  }else{
    
    probs_out_modal <- probs_out %>% 
      group_by(!!!syms(variables)) %>% 
      mutate(max_imd_prop = max(probability), nmax = sum(probability == max_imd_prop)) %>% 
      ## randomly mutate slightly if there could be two maximums
      ungroup() %>% 
      mutate(rands = rnorm(nrow(probs_out),0,0.01)) %>%
      mutate(probability = case_when(
        nmax > 1 ~ probability + rands,
        T ~ probability
      )) %>% select(!rands) %>%
      ## do again
      group_by(!!!syms(variables)) %>% 
      mutate(max_imd_prop = max(probability), nmax = sum(probability == max_imd_prop)) %>% 
      filter(probability == max_imd_prop) %>% 
      select(!c(n, n_tot, probability, max_imd_prop, nmax))
    
    data <- data %>% 
      left_join(probs_out_modal, by = variables)
    
    data <- data.table(data)
    
    out <- data[!is.na(imd_quintile),]
    
  }
  
  cat('\nTime taken: ', 
      floor(difftime(Sys.time(), time, units = 'secs')[[1]]/60), ' mins ',
      round(difftime(Sys.time(), time, units = 'secs')[[1]] - 
              60*floor(difftime(Sys.time(), time, units = 'secs')[[1]]/60)), ' secs',
      sep = '')
  
  out
  
}


## Function to get weights for large_n contacts from Polymod setting-specific matrices
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


## FUNCTION TO FILTER PARTICIPANT AGE GROUP ##

fit_matr_age_spec <- function(
    part,
    cont,
    distr,
    p_weights
){
  
  fit_matr_as <- function(age){
    
    out <- fit_matr(part_filt = part %>% filter(p_age_group == age),
                    cont_filt = cont %>% filter(p_age_group == age),
                    distr_filt = distr %>% filter(p_age_group == age),
                    p_weights_filt = p_weights %>% filter(p_age_group == age)
    )
    
    out
    
  }
  
  part <- part %>% arrange(p_age_group)
  
  age_spec_fits_list <- map(
    .x = unique(part$p_age_group),
    .f = fit_matr_as
  )
  
  age_spec_fits <- rbindlist(age_spec_fits_list)
  
  age_spec_fits
  
}


## FUNCTION TO FIT MATRICES ##

fit_matr <- function(
    part_filt,
    cont_filt,
    distr_filt,
    p_weights_filt
){
  
  ## sample large_contact ages   ## 
  
  # select probabilities and large_n for each sampled participant
  large_contacts <- part_filt %>% 
    select(row_id, p_id, p_age_group, bootstrap_index, 
           contains('add_')) %>% 
    pivot_longer(!c(row_id, p_id, p_age_group, bootstrap_index)) %>% 
    rename(large_n = value) %>% 
    filter(large_n != 0) %>% 
    mutate(name = gsub('18_64', '1864', name)) %>% 
    separate_wider_delim(name, delim = '_',
                         names = c('null','broad_age_group','c_location')) %>% 
    select(!null) %>% 
    mutate(broad_age_group = case_when(
      broad_age_group == 'u18' ~ '0-17',
      broad_age_group == '1864' ~ '18-64',
      broad_age_group == '65' ~ '65+'
    )) %>% 
    left_join(p_weights_filt %>% pivot_wider(names_from = c_age_group, values_from = prob),
              by = c('p_age_group','broad_age_group','c_location')) %>% 
    mutate(context = paste0(row_id,'_',broad_age_group,'_',c_location)) %>% 
    select(!c(row_id, p_id, p_age_group, bootstrap_index, broad_age_group, c_location))
  
  large_contacts <- data.table(large_contacts)
  
  # transpose so that can be vectorised
  large_contacts_flip <- dcast.data.table(melt.data.table(large_contacts, 
                                                          id.vars = c('context')),
                                          variable ~ context, value.var = 'value')
  
  # remove variable names
  large_contacts_flip <- large_contacts_flip[, 2:ncol(large_contacts_flip)]
  
  # run add_large_n
  large_contacts_sampled <- large_contacts_flip[, lapply(.SD, add_large_n)]
  
  # transpose back to original form
  large_contacts_sampled_wide <- data.table(cbind(colnames(large_contacts_sampled), t(large_contacts_sampled)))
  colnames(large_contacts_sampled_wide) <- c('context','large_n',age_labels)
  
  # make as.numeric
  large_contacts_sampled_wide <- large_contacts_sampled_wide[, lapply(.SD, as.numeric), by=context]
  
  # add back to participant data
  part_w_large <- part_filt %>% 
    select(row_id, p_id, p_age_group, p_imd_q, bootstrap_index, 
           contains('add_')) %>% 
    pivot_longer(!c(row_id, p_id, p_age_group, p_imd_q, bootstrap_index)) %>% 
    rename(large_n = value) %>% 
    filter(large_n != 0) %>% 
    mutate(name = gsub('18_64', '1864', name)) %>% 
    separate_wider_delim(name, delim = '_',
                         names = c('null','broad_age_group','c_location')) %>% 
    select(!null) %>% 
    mutate(broad_age_group = case_when(
      broad_age_group == 'u18' ~ '0-17',
      broad_age_group == '1864' ~ '18-64',
      broad_age_group == '65' ~ '65+'
    )) %>% 
    mutate(context = paste0(row_id,'_',broad_age_group,'_',c_location)) 
  
  part_w_large <- data.table(part_w_large)
  
  part_w_large <- part_w_large[large_contacts_sampled_wide, on = c('context','large_n')]
  
  part_w_large[, broad_age_group := NULL]
  part_w_large[, large_n := NULL]
  part_w_large[, context := NULL]
  
  id_vars <- c('row_id', 'p_id', 'p_age_group', 'p_imd_q', 'bootstrap_index', 'c_location')
  
  part_w_large <- part_w_large[, lapply(.SD, sum), by = id_vars]
  
  ## sample large_contact imd 
  
  part_l <- melt.data.table(part_w_large, id.vars = id_vars,
                            variable.name = 'c_age_group', value.name = 'n')
                  
  distr_dt <- data.table(distr_filt)
  distr_dt[, c_location := tolower(c_location)]
  distr_dt_w <- dcast.data.table(distr_dt, p_age_group + c_age_group + c_location + p_imd_q ~ c_imd_q, value.var = 'prop')
  
  part_l <- part_l[distr_dt_w, on = c('p_age_group','c_age_group','c_location','p_imd_q')]
  part_l <- part_l[!is.na(row_id)] # remove rows of distr_dt_w which don't occur in part_l
  part_l <- part_l[n != 0] # remove rows with no occurrences
  
  # remake 'context' column
  part_l[, context := paste0(row_id, '_', c_location, '_', c_age_group)]
  part_l <- part_l[, c('context','n','1','2','3','4','5')]
  
  # transpose so that can be vectorised
  large_contacts_imd_flip <- dcast.data.table(melt.data.table(part_l, 
                                                              id.vars = c('context')),
                                              variable ~ context, value.var = 'value')
  
  # remove variable names
  large_contacts_imd_flip <- large_contacts_imd_flip[, 2:ncol(large_contacts_imd_flip)]
  
  # run add_c_imd
  large_contacts_imd_sampled <- large_contacts_imd_flip[, lapply(.SD, add_c_imd)]
  
  # transpose back to original form
  large_contacts_imd_sampled_wide <- data.table(cbind(colnames(large_contacts_imd_sampled), t(large_contacts_imd_sampled)))
  colnames(large_contacts_imd_sampled_wide) <- c('context','large_n',as.character(1:5))
  
  # make as.numeric
  large_contacts_imd_sampled_wide <- large_contacts_imd_sampled_wide[, lapply(.SD, as.numeric), by=context]
  
  # remove those who couldn't be sampled
  large_contacts_imd_sampled_wide <- large_contacts_imd_sampled_wide[large_n != 0]
  
  # make longer
  large_contacts_imd_sampled_long <- melt.data.table(large_contacts_imd_sampled_wide,
                                                     id.vars = 'context')
                                                      
  large_contacts_imd_sampled_long <- large_contacts_imd_sampled_long[variable != 'large_n']
  setnames(large_contacts_imd_sampled_long, 'variable','c_imd_q')
  
  ## merge individual and large contacts
  
  # participants
  p_f <- part_filt[, c('row_id','p_id','p_age_group','bootstrap_index','p_imd_q')]
  
  # indiv contacts
  c_f <- cont_filt[, c('row_id','c_id','c_age_group','c_location','c_imd_q')]
  c_f[, c_location := tolower(c_location)]
  c_f[, context := paste0(row_id, '_', c_location, '_', c_age_group)]
  # aggregate
  c_f_agg <- c_f[, c('row_id','context','c_imd_q')][, n := 1][, lapply(.SD, sum), by = c('row_id','context','c_imd_q')]
  c_f_agg[, c_imd_q := as.factor(c_imd_q)]
  c_f_agg[, context := paste0(context, '_', c_imd_q)]
  
  # group contacts, adding in participants with no large n in specific age/location/c_imd_q
  large_contacts_imd_sampled_long[, context := paste0(context, '_', c_imd_q)]
  large_c <- large_contacts_imd_sampled_long[, c('context','value')]
  large_c_rbind <- rbind(large_c,
                         cbind(c_f_agg[context %notin% large_c$context, 
                                       c('context')],
                               value = 0))
  
  # merge contacts
  row_id_vecs <- unlist(strsplit(large_c_rbind$context, split = '_'))
  row_id_vec <- paste0(row_id_vecs[6*(1:nrow(large_c_rbind)) - 5],'_',
                       row_id_vecs[6*(1:nrow(large_c_rbind)) - 4],'_',
                       row_id_vecs[6*(1:nrow(large_c_rbind)) - 3])
  c_location_vec <- row_id_vecs[6*(1:nrow(large_c_rbind)) - 2]
  c_age_group_vec <- row_id_vecs[6*(1:nrow(large_c_rbind)) - 1]
  c_imd_q_vec <- row_id_vecs[6*(1:nrow(large_c_rbind))]
  large_c_rbind[, row_id := row_id_vec]
  large_c_rbind[, c_location := c_location_vec]
  large_c_rbind[, c_age_group := c_age_group_vec]
  large_c_rbind[, c_imd_q := c_imd_q_vec]
  
  contacts_merged <- c_f_agg[large_c_rbind, on = c('row_id','context','c_imd_q')]
  contacts_merged[is.na(n), n := 0]
  contacts_merged[, n := n + value]
  contacts_merged[, value := NULL]
  contacts_merged[, context := NULL]
  
  ## filling in participants with 0 contacts
  all_merged <- contacts_merged %>% 
    complete(row_id = p_f$row_id,
             c_imd_q = as.character(1:5),
             c_location = unique(distr_filt$c_location),
             c_age_group = age_labels,
             fill = list(n = 0)) %>% 
    left_join(p_f, by = 'row_id')
  
  all_merged <- data.table(all_merged)
  
  ## fit - using mean for now
  # TODO change to negative binomial estimation

  out_df <- all_merged[, c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q','c_location', 'n')]
  out_df <- out_df[, lapply(.SD, mean), by = c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q','c_location')]
  
  out_df
  
}


## VECTORISED FUNCTION TO ADD LARGE_N

add_large_n <- function(vec){
  
  n <- vec[1]
  probs <- vec[2:17]
  
  samples <- sample(x = 1:length(probs),
                    size = n,
                    prob = probs,
                    replace = T)
  
  occurrence_vec <- sapply(1:length(probs), FUN = function(x) length(samples[samples==x]))
  
  out_vec <- c(n, occurrence_vec)
  
  out_vec
  
}

## VECTORISED FUNCTION TO ADD IMD

add_c_imd <- function(vec){

  n <- vec[1]
  probs <- vec[2:6]
  
  # if no occurrences in the indiv contact data 
  # (i.e. sum(probs) == 0)
  # don't sample large group contacts
  if(sum(probs) == 0){return(rep(0, 6))}
  
  samples <- sample(x = 1:5,
                    size = n,
                    prob = probs,
                    replace = T)
  
  occurrence_vec <- sapply(1:length(probs), FUN = function(x) length(samples[samples==x]))
  
  out_vec <- c(n, occurrence_vec)
  
  out_vec
  
}

## TEXT FORMATTING ##
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}












