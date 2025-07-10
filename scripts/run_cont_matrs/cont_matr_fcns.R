
###########################################
## FUNCTIONS FOR MAKING CONTACT MATRICES ##
###########################################

options(dplyr.summarise.inform = FALSE)

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





