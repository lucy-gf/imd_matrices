
## FUNCTION TO INFER IMD FROM PCD1 AND CENSUS DATA ##

infer_imd <- function(
    data_input,
    census_data,
    variables,
    testing = T, # T if training, F if running on unknowns
    save_suffix = NULL,
    modal = F # T = deterministic, F = probabilistic
){
  
  method <- if(modal){'deterministic'}else{'probabilistic'}
  
  data <- copy(data_input)
  
  ## group data by only variables of interest (and IMD if in testing)
  
  vars_of_int <- if(testing){c(variables, 'lsoa21cd')}else{variables}
  data <- data %>% group_by(!!!syms(vars_of_int)) %>% 
    summarise(n = sum(population))
  data <- data.table(data)
  
  ## probabilities to distribute by
  
  vars_of_int_census <- c(variables, 'imd_quintile')
  probs_out <- census_data %>% group_by(!!!syms(vars_of_int_census)) %>% 
    summarise(n = sum(population)) %>% 
    group_by(!!!syms(variables)) %>% 
    mutate(n_tot = sum(n)) %>% 
    mutate(probability = n/n_tot) %>% 
    ungroup() %>% 
    complete(!!!syms(vars_of_int_census), 
             fill = list(n = 0,
                         n_tot = 0,
                         probability = 0)) %>% 
    arrange(imd_quintile)
  
  # print(nrow(data))
  
  if(modal == F){
    
    ## assign IMD according to probabilities
    
    st <- Sys.time()
    
    # add probabilities
    for(i_row in 1:nrow(data)){
      
      # filter down to probabilities
      dt <- data.table(probs_out)
      for(var in variables){
        dt <- dt[get(var) == unlist(unname(data[i_row, ..var])),]
      }
      
      data[i_row, probs := list(dt$probability)]
      
      if(data[i_row, n] != 0 & sum(unlist(data[i_row, probs])) > 0){
        data[i_row, imd_samples := list(sample_imd(data[i_row, n],
                                                   data[i_row, probs]))]
      }else{
        data[i_row, imd_samples := list(c(0,0,0,0,0))]
      }
      
      if(i_row == floor(nrow(data)/100)){cat('Expected runtime: ', format(100*(Sys.time() - st), format = '%m'), '\n', sep = '')}
      if(i_row %% floor(nrow(data)/100) == 0){cat(round(100*i_row/nrow(data), 1),'%, ', sep = '')}
    }
    
    if(!is.null(save_suffix)){
      write_rds(data, here::here('output','data','exploratory',method,paste0('imd_samples_', save_suffix,'.rds')))
    }
    
    return(data)
    
  }else{
    
    ## assign IMD according to modal quintile
    
    probs_deterministic <- probs_out %>% 
      group_by(!!!syms(variables)) %>% 
      summarise(max_prop = max(probability),
                which_max = which.max(probability))
    probs_deterministic <- data.table(probs_deterministic)
    
    if('pcd1' %in% variables){
      data <- data[probs_deterministic[pcd1 %in% unique(data$pcd1),], on = variables]
    }else{
      data <- data[probs_deterministic, on = variables]
    }
    
    if(!is.null(save_suffix)){
      write_rds(data, here::here('output','data','exploratory',method,paste0('imd_samples_', save_suffix,'.rds')))
    }
    
    return(data)
  
  }
  
}


## FUNCTION TO SAMPLE IMD FROM PROBABILITIES ##

sample_imd <- function(
    n,
    probs
){
  
  probabilities <- unlist(probs)
  
  x <- sample(1:length(probabilities), n,
              replace = T, prob = probabilities)
  
  y <- data.frame(table(x)) %>% 
    complete(x = as.character(1:length(probabilities)),
             fill = list(Freq = 0))
  
  return(y$Freq)
  
}


## FUNCTION TO SIMPLIFY VARIABLE LABELS ##

simp_labels <- function(string){
  string <- gsub('_cd','',string)
  string <- gsub('_nm','',string)
  string <- gsub('_grp','',string)
}


## FUNCTION TO PRODUCE VALIDATION OUTPUTS ##

validate_imd <- function(
    samples,
    variables,
    lsoa_data,
    interval_width = 0.5,
    output_area = 'lsoa21cd',
    modal = F # T = deterministic, F = probabilistic
){
  
  method <- if(modal){'deterministic'}else{'probabilistic'}
  
  simp_variables <- unname(sapply(variables, simp_labels))
  
  lsoa_data <- data.table(lsoa_data)
  
  # how does the classification do?
  
  # format sampled data
  if(modal == F){
    select_vec <- unique(c(output_area,'lsoa21cd','imd_samples'))
    sum_vec <- unique(c(output_area,'lsoa21cd','sampled_imd_quintile'))
    lsoa_samples <- samples[, ..select_vec]
    lsoa_samples_l <- data.table(unnest(lsoa_samples, 'imd_samples'))
    lsoa_samples_l[, sampled_imd_quintile := rep(1:5, nrow(lsoa_samples_l)/5)]
    lsoa_samples_l <- lsoa_samples_l[, lapply(.SD, sum), by = sum_vec]
  }else{
    select_vec <- unique(c(output_area,'lsoa21cd','n','which_max'))
    sum_vec <- unique(c(output_area,'lsoa21cd','sampled_imd_quintile'))
    lsoa_samples_l <- samples[, ..select_vec]
    colnames(lsoa_samples_l) <- c(output_area,'imd_samples','sampled_imd_quintile')
    lsoa_samples_l <- lsoa_samples_l[, lapply(.SD, sum), by = sum_vec]
    lsoa_samples_l <- lsoa_samples_l %>%
      complete(!!sym(output_area),
               sampled_imd_quintile = 1:5,
               fill = list(imd_samples = 0)) 
    lsoa_samples_l <- data.table(lsoa_samples_l)
  }

  # add in LSOA info (including true IMD quintile)
  
  vars_of_int <- c('lsoa21cd','imd_quintile','urban_rural','eng_reg')
  lsoa_check <- unique(lsoa_data[, ..vars_of_int])
  
  lsoa_combined <- lsoa_samples_l[lsoa_check[lsoa21cd %in% unique(lsoa_samples_l[,lsoa21cd])], on = 'lsoa21cd']
  
  vars_of_int <- c(output_area,'n_lsoa')
  if(output_area == 'lsoa21cd'){vars_of_int <- c(vars_of_int, 'imd_quintile','urban_rural','eng_reg')}
  
  if(output_area == 'pcd1'){
    lsoa_combined_out <- lsoa_combined %>% 
      group_by(!!sym(output_area)) %>% 
      mutate(n_lsoa = sum(imd_samples)) %>% 
      filter(sampled_imd_quintile == imd_quintile) %>% 
      group_by(!!!syms(vars_of_int)) %>%
      summarise(imd_samples = sum(imd_samples),
                urban_rural = names(which(table(urban_rural) == max(table(urban_rural)))[1]),
                eng_reg = names(which(table(eng_reg) == max(table(eng_reg)))[1])) %>% 
      mutate(prop_correct = imd_samples/n_lsoa,
             prop_incorrect = (n_lsoa - imd_samples)/n_lsoa)
    
    lsoa_closest <- lsoa_combined %>% 
      group_by(!!sym(output_area)) %>% 
      mutate(n_lsoa = sum(imd_samples)) %>% 
      mutate(correct_imd = (sampled_imd_quintile == imd_quintile),
             one_away_imd = abs(sampled_imd_quintile - imd_quintile) == 1) %>% 
      filter(correct_imd | one_away_imd) %>% 
      group_by(!!!syms(vars_of_int)) %>% 
      mutate(urban_rural = names(which(table(urban_rural) == max(table(urban_rural)))[1]),
             eng_reg = names(which(table(eng_reg) == max(table(eng_reg)))[1])) %>% 
      group_by(!!!syms(vars_of_int), urban_rural, eng_reg, correct_imd, one_away_imd) %>% 
      summarise(imd_samples = sum(imd_samples)) %>% 
      ungroup() %>% 
      mutate(distance = case_when(correct_imd ~ 'correct_imd',
                                  T ~ 'one_away_imd')) %>% 
      select(! c(correct_imd, one_away_imd)) %>% 
      pivot_wider(id_cols = c(all_of(vars_of_int),'urban_rural','eng_reg','n_lsoa'),
                  names_from = distance, values_from = imd_samples) %>% 
      mutate(prop_correct = correct_imd/n_lsoa,
             prop_one_away = one_away_imd/n_lsoa)
  }else{
    lsoa_combined_out <- lsoa_combined %>% 
      group_by(!!sym(output_area)) %>% 
      mutate(n_lsoa = sum(imd_samples)) %>% 
      filter(sampled_imd_quintile == imd_quintile) %>% 
      group_by(!!!syms(vars_of_int)) %>%
      summarise(imd_samples = sum(imd_samples)) %>% 
      mutate(prop_correct = imd_samples/n_lsoa,
             prop_incorrect = (n_lsoa - imd_samples)/n_lsoa)
    
    lsoa_closest <- lsoa_combined %>% 
      group_by(!!sym(output_area)) %>% 
      mutate(n_lsoa = sum(imd_samples)) %>% 
      mutate(correct_imd = (sampled_imd_quintile == imd_quintile),
             one_away_imd = abs(sampled_imd_quintile - imd_quintile) == 1) %>% 
      filter(correct_imd | one_away_imd) %>% 
      group_by(!!!syms(vars_of_int), correct_imd, one_away_imd) %>% 
      summarise(imd_samples = sum(imd_samples)) %>% 
      ungroup() %>% 
      mutate(distance = case_when(correct_imd ~ 'correct_imd',
                                  T ~ 'one_away_imd')) %>% 
      select(! c(correct_imd, one_away_imd)) %>% 
      pivot_wider(id_cols = c(all_of(vars_of_int), 'n_lsoa'),
                  names_from = distance, values_from = imd_samples) %>% 
      mutate(prop_correct = correct_imd/n_lsoa,
             prop_one_away = one_away_imd/n_lsoa)
  }
  
  cat('Using vars: ', paste(variables, collapse = ', '), ' (', method,' method) (output area = ',output_area,')\n',
      'Total assigned correctly: ', round(sum(lsoa_combined_out$imd_samples)/1e6, 1), 
      ' mill (', 100*signif(sum(lsoa_combined_out$imd_samples)/sum(lsoa_combined_out$n_lsoa), 2),
      '%, ', 100*interval_width,'% int. ', 100*signif(quantile(lsoa_combined_out$imd_samples/lsoa_combined_out$n_lsoa, (1 - interval_width)/2), 2),
      '% - ', 100*signif(quantile(lsoa_combined_out$imd_samples/lsoa_combined_out$n_lsoa, (1 - (1 - interval_width)/2)), 2),
      '%)\n\n', 
      sep = '')
  
  measure_in <- if(modal){'mean'}else('median')
    
  # overall density 
  plot1 <- plot_density(lsoa_combined_out)
  tab1 <- plot_median_table(lsoa_combined_out, measure = measure_in)
  
  # urban/rural
  plot2 <- plot_density(lsoa_combined_out, 'urban_rural')
  tab2 <- plot_median_table(lsoa_combined_out, 'urban_rural', measure = measure_in)
  
  # regions of england
  plot3 <- plot_density(lsoa_combined_out, 'eng_reg')
  tab3 <- plot_median_table(lsoa_combined_out, 'eng_reg', measure = measure_in)
  
  if(output_area == 'lsoa21cd'){
    # IMD quintiles
    plot4 <- plot_density(lsoa_combined_out, 'imd_quintile')
    tab4 <- plot_median_table(lsoa_combined_out, 'imd_quintile', measure = measure_in)
    
    plot1 + tab1 + plot3 + tab3 + plot2 + tab2 + plot4 + tab4 + 
      plot_layout(nrow = 2, guides = 'collect', axis_titles = 'collect',
                  widths = c(6,1,6,1)) +
      plot_annotation(title = paste0('Proportion of ', format_legend(output_area),' population assigned correct IMD, using: ', paste0(variables, collapse = ', ')),
                      theme = theme(plot.title = element_text(size = 16))) 
    
    ggsave(here::here('output','figures','exploratory',method,'lsoa_props',paste0('imd_fit_',output_area,'_',paste0(simp_variables, collapse = '_'),'.png')),
           width = 12, height = 10)
  }else{
    layout <- '
    AAAAAABCCCCCCD
    AAAAAABCCCCCCD
    EEEEEEFCCCCCCD
    EEEEEEFCCCCCCD
    '
    plot1 + tab1 + plot3 + tab3 + plot2 + tab2 + 
      plot_layout(nrow = 2, guides = 'collect', axis_titles = 'collect',
                  design = layout) +
      plot_annotation(title = paste0('Proportion of ', format_legend(output_area),' population assigned correct IMD, using: ', paste0(variables, collapse = ', ')),
                      theme = theme(plot.title = element_text(size = 16))) 
    
    ggsave(here::here('output','figures','exploratory',method,'pcd1_props',paste0('imd_fit_',output_area,'_',paste0(simp_variables, collapse = '_'),'.png')),
           width = 12, height = 7)
  }
  
  # how many within 1 quintile?
  
  # overall density 
  plot1 <- plot_density(lsoa_closest, within_one = T)
  tab1 <- plot_median_table(lsoa_closest, measure = measure_in, within_one = T)
  
  # urban/rural
  plot2 <- plot_density(lsoa_closest, 'urban_rural', within_one = T)
  tab2 <- plot_median_table(lsoa_closest, 'urban_rural', measure = measure_in, within_one = T)
  
  # regions of england
  plot3 <- plot_density(lsoa_closest, 'eng_reg', within_one = T)
  tab3 <- plot_median_table(lsoa_closest, 'eng_reg', measure = measure_in, within_one = T)
  
  if(output_area == 'lsoa21cd'){
    # IMD quintiles
    plot4 <- plot_density(lsoa_closest, 'imd_quintile', within_one = T)
    tab4 <- plot_median_table(lsoa_closest, 'imd_quintile', measure = measure_in, within_one = T)
    
    plot1 + tab1 + plot3 + tab3 + plot2 + tab2 + plot4 + tab4 + 
      plot_layout(nrow = 2, guides = 'collect', axis_titles = 'collect',
                  widths = c(6,1,6,1)) +
      plot_annotation(title = paste0('Proportion of ', format_legend(output_area),' population assigned correct IMD within 1 quintile, using: ', paste0(variables, collapse = ', ')),
                      theme = theme(plot.title = element_text(size = 16))) 
    
    ggsave(here::here('output','figures','exploratory',method,'lsoa_props_within_one',paste0('imd_fit_within_one_',output_area,'_',paste0(simp_variables, collapse = '_'),'.png')),
           width = 12, height = 10)
    
  }else{
    layout <- '
    AAAAAABBCCCCCCDD
    AAAAAABBCCCCCCDD
    EEEEEEFFCCCCCCDD
    EEEEEEFFCCCCCCDD
    '

    plot1 + tab1 + plot3 + tab3 + plot2 + tab2 + 
      plot_layout(nrow = 2, guides = 'collect', axis_titles = 'collect',
                  design = layout) +
      plot_annotation(title = paste0('Proportion of ', format_legend(output_area),' population assigned correct IMD within 1 quintile, using: ', paste0(variables, collapse = ', ')),
                      theme = theme(plot.title = element_text(size = 16))) 
    
    ggsave(here::here('output','figures','exploratory',method,'pcd1_props_within_one',paste0('imd_fit_within_one_',output_area,'_',paste0(simp_variables, collapse = '_'),'.png')),
           width = 12, height = 7)
    
  }
  
  if(length(variables[which(!variables == 'pcd1')]) > 0 &
     modal == F){
    ## by input variables 
    non_pcd_vars <- variables[which(!variables == 'pcd1')]
    vars_select <- c('lsoa21cd',non_pcd_vars,'imd_samples')
    
    # format sampled data
    lsoa_samples <- samples[, ..vars_select]
    lsoa_samples_l <- data.table(unnest(lsoa_samples, 'imd_samples'))
    lsoa_samples_l[, sampled_imd_quintile := rep(1:5, nrow(lsoa_samples_l)/5)]
    lsoa_samples_l <- lsoa_samples_l[, lapply(.SD, sum), by = c('lsoa21cd', non_pcd_vars, 'sampled_imd_quintile')]
      
    vars_of_int <- c('lsoa21cd','imd_quintile','urban_rural','eng_reg')
    lsoa_check <- unique(lsoa_data[, ..vars_of_int])
    lsoa_combined <- lsoa_samples_l[lsoa_check, on = 'lsoa21cd']
    
    lsoa_combined_out_w_vars <- lsoa_combined %>% 
      group_by(!!!syms(c(vars_of_int, non_pcd_vars))) %>% 
      mutate(n_lsoa = sum(imd_samples)) %>% 
      filter(sampled_imd_quintile == imd_quintile, 
             n_lsoa > 0) %>% 
      mutate(prop_correct = imd_samples/n_lsoa,
             prop_incorrect = (n_lsoa - imd_samples)/n_lsoa) %>% 
      select(! sampled_imd_quintile)
    
    for(k in 1:length(non_pcd_vars)){
      
      plot_var <- non_pcd_vars[k]
      
      ggplot(lsoa_combined_out_w_vars) + 
        geom_density(aes(x = prop_correct, fill = as.factor(!!sym(plot_var)), col = as.factor(!!sym(plot_var)), group = !!sym(plot_var)), 
                     alpha = 0.6, lwd = 0.8) + labs(fill = plot_var, color = plot_var) +
        theme_bw() + labs(x = 'Proportion correct', y = 'Density')
      
      ggsave(here::here('output','figures','exploratory',method,'var_distribution',paste0('imd_fit_',length(non_pcd_vars),'_',simp_labels(plot_var),'_by_var.png')),
             width = 7, height = 6)
      
    }
    
    out_dt2 <- lsoa_combined_out_w_vars %>% 
      group_by(!!!syms(non_pcd_vars)) %>% 
      summarise(med = median(prop_correct), 
                lower = quantile(prop_correct, (1 - interval_width)/2),
                upper = quantile(prop_correct, (1 - (1 - interval_width)/2)))
    
    write_csv(out_dt2, here::here('output','data','exploratory',method,paste0('prop_correct_', paste(unname(sapply(non_pcd_vars, simp_labels)), collapse = '_'),'.rds')))
    
    if(length(non_pcd_vars) == 2){
      out_dt2 %>% 
        ggplot() + 
        geom_tile(aes(x = get(non_pcd_vars[1]), y = get(non_pcd_vars[2]),
                      fill = med)) + 
        geom_text(aes(x = get(non_pcd_vars[1]), y = get(non_pcd_vars[2]),
                      label = signif(med, 3)), col = 'white') + 
        theme_bw() + 
        labs(x = simp_labels(non_pcd_vars[1]),
             y = simp_labels(non_pcd_vars[2]),
             fill = 'Median')
       ggsave(here::here('output','figures','exploratory',method,'var_distribution',
                         paste0('median_fit_',paste(non_pcd_vars, collapse = '_'),'.png')),
             width = 14, height = 10)
    }
      
  }
  
  ## save dt
  
  save_vars <- c('urban_rural','eng_reg')
  if(output_area == 'lsoa21cd'){save_vars <- c(save_vars, 'imd_quintile')}
  
  out_dt <- data.frame()
  for(var in save_vars){
    if(modal){
      out_dt <- rbind(out_dt,
                      lsoa_combined_out %>% mutate(variable = var) %>% group_by(variable, !!sym(var)) %>% 
                        rename(category = !!sym(var)) %>% mutate(category = as.character(category)) %>% 
                        summarise(mean = mean(prop_correct), 
                                  lower = quantile(prop_correct, (1 - interval_width)/2),
                                  upper = quantile(prop_correct, (1 - (1 - interval_width)/2))))
    }else{
      out_dt <- rbind(out_dt,
                      lsoa_combined_out %>% mutate(variable = var) %>% group_by(variable, !!sym(var)) %>% 
                        rename(category = !!sym(var)) %>% mutate(category = as.character(category)) %>% 
                        summarise(median = median(prop_correct), 
                                  lower = quantile(prop_correct, (1 - interval_width)/2),
                                  upper = quantile(prop_correct, (1 - (1 - interval_width)/2))))
    }
  }
  
  out_dt
  
}


# function to plot density with a facet

plot_density <- function(data,
                         variable = NULL,
                         within_one = F){
  
  if(is.null(variable)){
    
    data_plot <- if(within_one == T){
      data %>% mutate(prop_correct = prop_correct + prop_one_away) 
    }else{
      data 
    }
    
    ggplot(data_plot) + 
      geom_density(aes(x = prop_correct, weight = n_lsoa), fill = 'grey', lwd = 0.8) +
      theme_bw() + labs(x = paste0('Proportion ', ifelse(within_one, 'within one IMD quintile', 'correct')),
                        y = 'Density') + xlim(c(0,1))
    
  }else{
    
    name_to_change <- which(colnames(data) == variable)
    names <- colnames(data)
    names[name_to_change] <- 'variable'
    colnames(data) <- names
    
    legend_name <- format_legend(variable)
    
    data_plot <- if(within_one == T){
      data %>% mutate(prop_correct = prop_correct + prop_one_away) 
    }else{
      data 
    }
    
    ggplot(data_plot) + 
      geom_density(aes(x = prop_correct, fill = as.factor(variable), 
                       col = as.factor(variable), weight = n_lsoa), 
                   alpha = 0.6, lwd = 0.8) + 
      scale_fill_manual(values = get(paste0(variable,'_colors'))) + 
      scale_color_manual(values = get(paste0(variable,'_colors'))) + 
      theme_bw() + labs(x = paste0('Proportion ', ifelse(within_one, 'within one IMD quintile', 'correct')),
                        y = 'Density',
                        fill = legend_name,
                        color = legend_name) + xlim(c(0,1))
    
  }
  
}

# function to plot median/mean proportions

plot_median_table <- function(data,
                              variable = NULL,
                              measure = 'median',
                              within_one = F){
  
  if(is.null(variable)){
    
    data_agg <- if(within_one == F){
      data %>% 
      group_by() %>% 
      summarise(m = get(measure)(prop_correct),
                x = 1, y = 1)
    }else{
      data %>% 
        group_by() %>% 
        summarise(m = get(measure)(prop_correct + prop_one_away),
                  x = 1, y = 1)
    }
    
    ggplot(data_agg) + 
      geom_tile(aes(x = x, y = y), fill = 'grey') +
      geom_text(aes(x = x, y = y,
                    label = signif(m, 2))) +
      theme_void() +
      labs(x = '', y = '') +
      theme(legend.position = 'none',
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  }else{
    
    name_to_change <- which(colnames(data) == variable)
    names <- colnames(data)
    names[name_to_change] <- 'variable'
    colnames(data) <- names
    
    legend_name <- format_legend(variable)
    
    data_agg <- if(within_one == F){
      data %>% 
      group_by(variable) %>% 
      summarise(m = get(measure)(prop_correct),
                x = 1)
    }else{
      data %>% 
        group_by(variable) %>% 
        summarise(m = get(measure)(prop_correct + prop_one_away),
                  x = 1)
    }
    
    ggplot(data_agg) + 
      geom_tile(aes(x = x, y = ordered(as.factor(variable), 
                                       levels = rev(levels(as.factor(variable)))),
                    fill = as.factor(variable)), alpha = 0.6) +
      geom_text(aes(x = x, y = ordered(as.factor(variable), 
                                       levels = rev(levels(as.factor(variable)))),
                    label = signif(m, 2))) +
      scale_fill_manual(values = get(paste0(variable, '_colors'))) + 
      theme_void() +
      labs(x = '', y = '') +
      theme(legend.position = 'none',
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  }
  
}


# function to turn column names into nice text

format_legend <- function(string){
  
  if(string == 'imd_quintile'){return('IMD quintile')}
  if(string == 'eng_reg'){return('Region')}
  if(string == 'urban_rural'){return('Urban/rural')}
  if(string == 'lsoa21cd'){return('LSOA')}
  if(string == 'pcd1'){return('PCD1')}
  
}






