
options(dplyr.summarise.inform = FALSE)

## FUNCTION TO ASSIGN IMD TO CONNECT PARTICIPANTS ##

fcn_assign_imd <- function(
    data_input,
    census_data,
    variables,
    n_bootstraps = NULL,
    modal = F # T = deterministic, F = probabilistic
){
  
  n_row_original <- nrow(data_input)
  
  data <- data.table(data_input)
  census_data <- data.table(census_data)
    
  # remove participants who can't be assigned
  for(var in variables){
    
    data <- data[get(var) %in% unname(unlist(unique(census_data[,..var]))), ]
    
    # cat(round(100*(1 - nrow(data)/n_row_original), 1), 
    #     '% of Connect participants removed after variable: ', var, '\n', sep = '')
  }
  
  vars_of_int_census <- c(variables, 'imd_quintile')
  
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
    
    # set progress bar
    pb <- txtProgressBar(min = 0, max = nrow(probs_unique), initial = 0, style = 3) 
    
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
          sampled_imd <- sample(dt$imd_quintile, n_bootstraps*n_participants_filt, 
                                replace = T, prob = dt$probability)
        }else{
          if(nrow(dt) == 1){
            sampled_imd <- rep(dt$imd_quintile, n_bootstraps*n_participants_filt)
          }else{
            sampled_imd <- rep(0, n_bootstraps*n_participants_filt)
          }
        }
        
        data_reps <- rbind(data_reps,
                           participants_filt %>% slice(rep(row_number(), each = n_bootstraps)) %>% 
                             mutate(bootstrap = rep(1:n_bootstraps, n_participants_filt),
                                    imd_quintile = sampled_imd))
      }
      
      setTxtProgressBar(pb,i_row)
      
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
      left_join(probs_out_modal, by = variables) %>% 
      mutate(bootstrap = 1)
    
    data <- data.table(data)
      
    out <- data[!is.na(imd_quintile),]
    
  }
  
  # write_rds(out, here::here('output','data','exploratory','assignment',
  #                           paste0('connect_output_', 
  #                                  paste(unname(unlist(lapply(variables, FUN = simp_labels))), 
  #                                        collapse = '_'), 
  #                                  ifelse(modal,'_det','_probab'), 
  #                                  ifelse(modal, '', paste0('_', n_bootstraps)), '.rds')))
  
  out
  
}



## FUNCTIONS TO PLOT IMD ASSIGNMENT AGAINST VARS ##

fcn_barplot_imd <- function(
    data_output,
    vars_list,
    save = T
){
  
  n_col <- ceiling(sqrt(length(vars_list)))
  n_row <- ceiling(length(vars_list)/n_col)
  
  fcn_barplot <- function(var){
    
    if(var == 'p_income'){
      data_output$p_income <- factor(data_output$p_income,
                                     levels = c("Less than £20,000","£20,000 - £39,999",
                                                "£40,000 - £59,999","£60,000 - £100,000", 
                                                "Over £100,000", "Child (Not Applic.)"))
    }
    
    p <- data_output %>% 
      group_by(imd_quintile, !!sym(var)) %>% 
      count() %>% 
      ggplot() + 
      geom_bar(aes(x = !!sym(var), y = n, fill = as.factor(imd_quintile)),
               position = 'fill', stat = 'identity') + 
      theme_bw() + labs(fill = 'IMD quintile', x = '', y = '') + 
      ggtitle(format_legend(var)) + 
      scale_fill_manual(values = imd_quintile_colors) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = 'none')
    
  }
  
  plots <- map(
    .x = vars_list,
    .f = fcn_barplot
  )
  
  patched <- patchwork::wrap_plots(plots, ncol = n_col);patched
  
  if(save){
    ggsave(here::here(directory_plots,
                      'imd_vs_vars_barplot.png'),
           width = 8*n_col, height = 8*n_row, dpi = 800)  
  }
  
  patched

}

fcn_lineplot_imd <- function(
    data_output,
    vars_list,
    ci_width = 0.5
){
  
  n_col <- ceiling(sqrt(length(vars_list)))
  n_row <- ceiling(length(vars_list)/n_col)
  
  fcn_lineplot <- function(var){
    
    if(var == 'p_income'){
      data_output$p_income <- factor(data_output$p_income,
                                     levels = c("Less than £20,000","£20,000 - £39,999",
                                                "£40,000 - £59,999","£60,000 - £100,000", 
                                                "Over £100,000", "Child (Not Applic.)"))
    }
    
    p <- data_output %>% 
      group_by(bootstrap, !!sym(var)) %>% 
      mutate(n_tot = n()) %>% 
      group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
      summarise(n_imd = n()) %>% 
      mutate(prop_bs = n_imd/n_tot) %>% 
      group_by(imd_quintile, !!sym(var)) %>% 
      summarise(med_prop = median(prop_bs),
                lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
      ggplot() + 
      geom_ribbon(aes(x = !!sym(var), ymin = lower_prop, ymax = upper_prop, 
                    fill = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                  alpha = 0.5) + 
      geom_line(aes(x = !!sym(var), y = med_prop, 
                    color = as.factor(imd_quintile), group = as.factor(imd_quintile)),
               lwd = 0.8) + 
      theme_bw() + labs(col = 'IMD quintile', fill = 'IMD quintile', x = '', y = '') + 
      ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI')) + ylim(c(0,NA)) + 
      scale_fill_manual(values = imd_quintile_colors) +
      scale_color_manual(values = imd_quintile_colors) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1));p
    
  }
  
  plots <- map(
    .x = vars_list,
    .f = fcn_lineplot
  )
  
  patched <- patchwork::wrap_plots(plots, ncol = n_col);patched
  
  ggsave(here::here(directory_plots,
                    'imd_vs_vars_lineplot.png'),
         width = 8*n_col, height = 8*n_row, dpi = 800)
  
  patched
  
}

fcn_errorbarplot_imd <- function(
    data_output,
    vars_list,
    true_vals = NULL,
    ci_width = 0.5
){
  
  n_col <- ceiling(sqrt(length(vars_list)))
  n_row <- ceiling(length(vars_list)/n_col)
  
  fcn_errorbarplot <- function(var){
    
    if(var == 'p_income'){
      data_output$p_income <- factor(data_output$p_income,
                                     levels = c("Less than £20,000","£20,000 - £39,999",
                                                "£40,000 - £59,999","£60,000 - £100,000", 
                                                "Over £100,000", "Child (Not Applic.)"))
    }
    
    if(is.null(true_vals)){
      p <- data_output %>% 
        group_by(bootstrap, !!sym(var)) %>% 
        mutate(n_tot = n()) %>% 
        group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
        summarise(n_imd = n()) %>% 
        mutate(prop_bs = n_imd/n_tot) %>% 
        group_by(imd_quintile, !!sym(var)) %>% 
        summarise(med_prop = median(prop_bs),
                  lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                  upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
        ggplot() + 
        geom_errorbar(aes(x = !!sym(var), ymin = lower_prop, ymax = upper_prop, 
                          col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                      position = position_dodge(width = 0.9)) + 
        geom_point(aes(x = !!sym(var), y = med_prop, 
                       color = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                   size = 2, position = position_dodge(width = 0.9)) + 
        theme_bw() + labs(col = 'IMD quintile', fill = 'IMD quintile', x = '', y = '') + 
        ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI')) + 
        ylim(c(0,NA)) + 
        scale_color_manual(values = imd_quintile_colors) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1));p
    }else{
      
      imd_props <- data_output %>% 
        group_by(imd_quintile) %>% 
        summarise(n = n(), n_tot = nrow(data_output), true_prop = n/n_tot)
      imd_props_true <- true_vals %>% 
        group_by(imd_quintile) %>% 
        summarise(n = sum(n), n_tot = sum(n_tot), true_prop = n/n_tot)
      
      p <- data_output %>% 
        group_by(bootstrap, !!sym(var)) %>% 
        mutate(n_tot = n()) %>% 
        group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
        summarise(n_imd = n()) %>% 
        mutate(prop_bs = n_imd/n_tot) %>% 
        group_by(imd_quintile, !!sym(var)) %>% 
        summarise(med_prop = median(prop_bs),
                  lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                  upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
        drop_na() %>% 
        left_join(imd_props, by = c('imd_quintile')) %>% 
        ggplot() + 
        geom_errorbar(aes(x = !!sym(var), ymin = lower_prop/true_prop, ymax = upper_prop/true_prop, 
                          col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                      position = position_dodge(width = 0.9)) + 
        geom_point(aes(x = !!sym(var), y = med_prop/true_prop, 
                       color = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                   size = 2, position = position_dodge(width = 0.9)) + 
        geom_point(data = true_vals %>% left_join(imd_props_true, by = 'imd_quintile') %>% drop_na(),
                   aes(x = !!sym(var), y = prop/true_prop, 
                       color = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                   size = 3, position = position_dodge(width = 0.9), shape = 4) + 
        theme_bw() + labs(col = 'IMD quintile', fill = 'IMD quintile', x = '', y = '') + 
        ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI, Predictors = ', 
                       paste(unname(unlist(lapply(variables_input, FUN = format_legend))),
                             collapse = ', '), ',\nMethod = ', 
                       ifelse(modal_in, 'deterministic', 
                              paste0('probabilistic, ',  n_bootstraps, ' bootstraps')))) + 
        ylim(c(0,NA)) + 
        scale_color_manual(values = imd_quintile_colors) +
        geom_hline(yintercept = 1, lty = 2, alpha = 0.2) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1));p
    
    }
    
  }
  
  plots <- map(
    .x = vars_list,
    .f = fcn_errorbarplot
  )
  
  patched <- patchwork::wrap_plots(plots, ncol = n_col) 
  
  # add main title if a patchwork plot
  if(is.null(true_vals)){
    patched <- patched + plot_annotation(title = paste0('Predictors = ', 
                       paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                             collapse = ', '), ',\nMethod = ', 
                       ifelse(modal_in, 'deterministic', 
                              paste0('probabilistic, ',  n_bootstraps, ' bootstraps'))))
  }
  
  patched
  
  if(is.null(true_vals)){
    ggsave(here::here(directory_plots,
                      'imd_vs_vars_errorbarplot.png'),
           width = 8*n_col, height = 8*n_row, dpi = 800)
  }else{
    ggsave(here::here(directory_plots,
                      paste0('imd_vs_', paste(unname(unlist(lapply(vars_list, FUN = simp_labels))), 
                                              collapse = '_') ,'_errorbarplot.png')),
           width = 1.5*max(c(n_distinct(true_vals %>% select(!!sym(vars_list[1]))), 6)), 
           height = 8, dpi = 800)
  }
  
  patched
  
}

## THIS FUNCTION SHOWS REVERSED RISK RATIOS (MAYBE PREFERRED?) ##
fcn_rev_errorbarplot_imd <- function(
    data_output,
    vars_list,
    true_vals = NULL,
    ci_width = 0.5,
    facet = F,
    true_distr = F
){
  
  n_bootstraps <- n_distinct(data_output$bootstrap)
  
  n_col <- ceiling(sqrt(length(vars_list)))
  n_row <- ceiling(length(vars_list)/n_col)
  
  fcn_errorbarplot <- function(var){
    
    if(var == 'p_income'){
      data_output$p_income <- factor(data_output$p_income,
                                     levels = c("Less than £20,000","£20,000 - £39,999",
                                                "£40,000 - £59,999","£60,000 - £100,000", 
                                                "Over £100,000", "Child (Not Applic.)"))
    }
    if(var == 'p_ethnicity'){
      data_output <- data_output %>% 
        filter(!p_ethnicity %like% 'Prefer')
    }
    if(var == 'p_sec_input'){
      data_output <- data_output %>% 
        filter(p_sec_input %in% as.character(1:7))
      
      true_vals <- true_vals %>%
        mutate(p_sec_input = as.character(p_sec_input)) %>% 
        filter(p_sec_input %in% as.character(1:7))
    }
    if(var == 'p_hiqual'){
      data_output <- data_output %>% 
        filter(p_hiqual != 'Other',
               !p_hiqual %like% 'Child')
    }
    
    if(is.null(true_vals)){
      p <- data_output %>% 
        group_by(bootstrap, imd_quintile) %>% 
        mutate(n_tot = n()) %>% 
        group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
        summarise(n_var = n()) %>% drop_na() %>% 
        mutate(prop_bs = n_var/n_tot) %>% 
        group_by(imd_quintile, !!sym(var)) %>% 
        summarise(med_prop = median(prop_bs),
                  lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                  upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
        ggplot() + 
        geom_errorbar(aes(x = imd_quintile, ymin = lower_prop, ymax = upper_prop, 
                          col = !!sym(var), group = !!sym(var)),
                      position = position_dodge(width = 0.9)) + 
        geom_point(aes(x = imd_quintile, y = med_prop, 
                       color = !!sym(var), group = !!sym(var)),
                   size = 2, position = position_dodge(width = 0.9)) + 
        theme_bw() + labs(col = format_legend(var), fill = format_legend(var), 
                          x = 'IMD quintile', y = 'Proportion') + 
        ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI')) + 
        ylim(c(0,NA)) + scale_color_brewer(palette = 'Set2'); p
    }else{
      if(true_distr == F){
        
        var_props_in_data <- data_output %>% 
          select(!!sym(var)) %>% drop_na() %>% 
          group_by(!!sym(var)) %>% 
          summarise(n = n(), n_tot = nrow(data_output), general_data_prop = n/n_tot)
        var_props_census <- true_vals %>% ungroup() %>% 
          mutate(tot_pop = sum(n)) %>% 
          group_by(!!sym(var), tot_pop) %>% 
          summarise(n_tot = sum(n)) %>% 
          mutate(general_imd_prop = n_tot/tot_pop)
        var_imd_props_census <- true_vals %>% 
          group_by(imd_quintile) %>% 
          mutate(n_tot = sum(n)) %>% 
          group_by(imd_quintile, !!sym(var), n_tot) %>% 
          summarise(n = sum(n)) %>% 
          mutate(true_var_imd_prop = n/n_tot)
        
        plot_input <- data_output %>% 
          group_by(bootstrap, imd_quintile) %>% 
          mutate(n_tot = n()) %>% 
          group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
          summarise(n_var = n()) %>% drop_na() %>% 
          mutate(prop_bs = n_var/n_tot) %>% 
          group_by(imd_quintile, !!sym(var)) %>% 
          summarise(med_prop = median(prop_bs),
                    lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                    upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
          left_join(var_props_in_data %>% select(!!sym(var), general_data_prop), by = var) %>% 
          left_join(var_props_census %>% select(!!sym(var), general_imd_prop), by = var) %>% 
          left_join(var_imd_props_census %>% select(!!sym(var), imd_quintile, true_var_imd_prop), by = c(var, 'imd_quintile')) 
        
        if(var == 'p_age_group'){
          plot_input$p_age_group <- factor(plot_input$p_age_group,
                                           levels = c('0-4','5-9',
                                                      '10-14','15-19',
                                                      '20-24','25-29',
                                                      '30-34','35-39',
                                                      '40-44','45-49',
                                                      '50-54','55-59',
                                                      '60-64','65-69',
                                                      '70-74','75+'))
        }
        
        if(facet){
          
          p <- plot_input %>% 
            ggplot() + 
            geom_errorbar(aes(x = !!sym(var), ymin = lower_prop/general_data_prop, ymax = upper_prop/general_data_prop, 
                              col = !!sym(var)),
                          position = position_dodge(width = 0.9), width = 0.5) + 
            geom_point(aes(x = !!sym(var), y = med_prop/general_data_prop,
                           color = !!sym(var)),
                       size = 2, position = position_dodge(width = 0.9)) +
            geom_point(aes(x = !!sym(var), y = true_var_imd_prop/general_imd_prop),
                       size = 3, position = position_dodge(width = 0.9), shape = 4) + 
            theme_bw() + labs(col = '', x = '', y = 'Proportion') +
            geom_hline(yintercept = 1, lty = 2, alpha = 0.4) + 
            ylim(c(0,NA)) + facet_grid(imd_quintile~.) + 
            theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) + 
            scale_color_manual(values = get(paste0('colors_', var))) +
            ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI,\nPredictors = ', 
                           paste(unname(unlist(lapply(variables_input, FUN = format_legend))),
                                 collapse = ', '), ',\nMethod = ', 
                           ifelse(modal_in, 'deterministic', 
                                  paste0('probabilistic, ',  n_bootstraps, ' bootstraps')))); p
          
        }else{
          
          p <- plot_input %>% 
            ggplot() + 
            geom_errorbar(aes(x = imd_quintile, ymin = lower_prop/general_data_prop, ymax = upper_prop/general_data_prop, 
                              col = !!sym(var), group = !!sym(var)),
                          position = position_dodge(width = 0.9)) + 
            geom_point(aes(x = imd_quintile, y = med_prop/general_data_prop,
                           color = !!sym(var), group = !!sym(var)),
                       size = 2, position = position_dodge(width = 0.9)) +
            geom_point(aes(x = imd_quintile, y = true_var_imd_prop/general_imd_prop, 
                           color = !!sym(var), group = !!sym(var)),
                       size = 3, position = position_dodge(width = 0.9), shape = 4) + 
            theme_bw() + labs(col = format_legend(var), fill = format_legend(var), 
                              x = 'IMD quintile', y = 'Proportion') +
            geom_hline(yintercept = 1, lty = 2, alpha = 0.4) + 
            scale_color_manual(values = get(paste0('colors_', var))) + 
            ylim(c(0,NA)) + 
            ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI,\nPredictors = ', 
                           paste(unname(unlist(lapply(variables_input, FUN = format_legend))),
                                 collapse = ', '), ',\nMethod = ', 
                           ifelse(modal_in, 'deterministic', 
                                  paste0('probabilistic, ',  n_bootstraps, ' bootstraps')))); p
          
        }
        
      }else{
        var_props_in_data <- data_output %>%
          select(!!sym(var)) %>% drop_na() %>%
          group_by(!!sym(var)) %>%
          summarise(n = n(), n_tot = nrow(data_output), general_data_prop = n/n_tot) 
        var_props_in_data[,1] <- as.character(unlist(var_props_in_data[,1]))
        var_props_census <- true_vals %>% ungroup() %>% drop_na() %>% 
          mutate(tot_pop = sum(n)) %>%
          group_by(!!sym(var), tot_pop) %>%
          summarise(n_tot = sum(n)) %>%
          mutate(general_imd_prop = n_tot/tot_pop)
        var_imd_props_census <- true_vals %>% drop_na() %>% 
          group_by(imd_quintile) %>%
          mutate(n_tot = sum(n)) %>%
          group_by(imd_quintile, !!sym(var), n_tot) %>%
          summarise(n = sum(n)) %>%
          mutate(true_var_imd_prop = n/n_tot)
        
        variable_order <- names(get(paste0('colors_', var)))
        
        var_props_in_data <- var_props_in_data %>% arrange(factor(!!sym(var), levels = variable_order))
        var_props_census <- var_props_census %>% arrange(factor(!!sym(var), levels = variable_order))
        
        plot_input <- data_output %>% 
          group_by(bootstrap, imd_quintile) %>% 
          mutate(n_tot = n()) %>% 
          group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
          summarise(n_var = n()) %>% drop_na() %>% 
          mutate(prop_bs = n_var/n_tot) %>% 
          group_by(imd_quintile, !!sym(var)) %>% 
          summarise(med_prop = median(prop_bs),
                    lower_prop = quantile(prop_bs, (1 - ci_width)/2),
                    upper_prop = quantile(prop_bs, (1 - (1 - ci_width)/2))) %>% 
          # left_join(var_props_in_data %>% select(!!sym(var), general_data_prop), by = var) %>% 
          # left_join(var_props_census %>% select(!!sym(var), general_imd_prop), by = var) %>% 
          left_join(var_imd_props_census %>% 
                      select(!!sym(var), imd_quintile, true_var_imd_prop), by = c(var, 'imd_quintile')) %>% 
          mutate(new_var = factor(!!sym(var), levels = variable_order))
        
        if(var == 'p_age_group'){
          plot_input$p_age_group <- factor(plot_input$p_age_group,
                                           levels = c('0-4','5-9',
                                                      '10-14','15-19',
                                                      '20-24','25-29',
                                                      '30-34','35-39',
                                                      '40-44','45-49',
                                                      '50-54','55-59',
                                                      '60-64','65-69',
                                                      '70-74','75+'))
        }
        
        if(facet){
          
          p <- plot_input %>%
            ggplot() + 
            geom_errorbar(aes(x = new_var, 
                              ymin = lower_prop, ymax = upper_prop, 
                              col = new_var),
                          position = position_dodge(width = 0.9), width = 0.5) + 
            geom_point(aes(x = new_var, y = med_prop,
                           color = new_var),
                       size = 2, position = position_dodge(width = 0.9)) +
            geom_point(aes(x = new_var, y = true_var_imd_prop),
                       size = 3, position = position_dodge(width = 0.9), shape = 4) + 
            theme_bw() + labs(col = '', x = '', y = 'Proportion') +
            ylim(c(0,NA)) + facet_grid(imd_quintile~.) + 
            theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) +
            scale_color_manual(values = get(paste0('colors_', var)),
                               labels = paste0(names(get(paste0('colors_', var))),
                                               ' (', 
                                               round(var_props_in_data$general_data_prop, 2),
                                               ' vs ',
                                               round(var_props_census$general_imd_prop, 2),
                                               ')')) + 
            ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI,\nPredictors = ', 
                           paste(unname(unlist(lapply(variables_input, FUN = format_legend))),
                                 collapse = ', '), ',\nMethod = ', 
                           ifelse(modal_in, 'deterministic', 
                                  paste0('probabilistic, ',  n_bootstraps, ' bootstraps')))); p
          
        }else{
          
          p <- plot_input %>% 
            ggplot() + 
            geom_errorbar(aes(x = imd_quintile, ymin = lower_prop, ymax = upper_prop, 
                              col = new_var, group = new_var),
                          position = position_dodge(width = 0.9)) + 
            geom_point(aes(x = imd_quintile, y = med_prop,
                           color = new_var, group = new_var),
                       size = 2, position = position_dodge(width = 0.9)) +
            geom_point(aes(x = imd_quintile, y = true_var_imd_prop, 
                           color = new_var, group = new_var),
                       size = 3, position = position_dodge(width = 0.9), shape = 4) + 
            theme_bw() + labs(col = format_legend(var), fill = format_legend(var), 
                              x = 'IMD quintile', y = 'Proportion') +
            scale_color_manual(values = get(paste0('colors_', var)),
                               labels = paste0(names(get(paste0('colors_', var))),
                                               ' (',
                                               round(var_props_in_data$general_data_prop, 2),
                                               ' vs ',
                                               round(var_props_census$general_imd_prop, 2),
                                               ')')) +
            ylim(c(0,NA)) + 
            ggtitle(paste0(format_legend(var), ', ', 100*ci_width, '% CI,\nPredictors = ', 
                           paste(unname(unlist(lapply(variables_input, FUN = format_legend))),
                                 collapse = ', '), ',\nMethod = ', 
                           ifelse(modal_in, 'deterministic', 
                                  paste0('probabilistic, ',  n_bootstraps, ' bootstraps')))); p
          
        }
        
      }
      
    }
    
  }
  
  plots <- map(
    .x = vars_list,
    .f = fcn_errorbarplot
  )
  
  patched <- patchwork::wrap_plots(plots, ncol = n_col) 
  
  # add main title if a patchwork plot
  if(is.null(true_vals)){
    patched <- patched + plot_annotation(title = paste0('Predictors = ', 
                                                        paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                                                              collapse = ', '), ',\nMethod = ', 
                                                        ifelse(modal_in, 'deterministic', 
                                                               paste0('probabilistic, ',  n_bootstraps, ' bootstraps'))))
  }
  
  patched
  
  # if(is.null(true_vals)){
  #   ggsave(here::here(directory_plots,
  #                     'imd_vs_vars_errorbarplot.png'),
  #          width = 8*n_col, height = 8*n_row, dpi = 800)
  # }else{
  #   ggsave(here::here(directory_plots,
  #                     paste0('imd_vs_', paste(unname(unlist(lapply(vars_list, FUN = simp_labels))), 
  #                                             collapse = '_') ,'_errorbarplot',
  #                            ifelse(true_distr, '_true_distr',''),'.png')),
  #          width = 2*max(c(n_distinct(true_vals %>% select(!!sym(vars_list[1]))), 6)), 
  #          height = 8, dpi = 800)
  # }
  
  patched
  
}

 ## FUNCTION TO EVALUATE IMD ASSIGNMENT ##

fcn_evaluate_imd <- function(
    data_input,
    census_data_list,
    predictors,
    modal,
    summary_stat = 'wis', # c('wis','mse')
    scores 
){
  
  data <- copy(data_input)
  
  n_bootstraps <- n_distinct(data$bootstrap)
  n_participants <- nrow(data)/n_bootstraps
  
  ci_width <- 0.95
  
  title_patch <- paste0('Predictors = ', 
                  paste(unname(unlist(lapply(predictors, FUN = simp_labels))),
                        collapse = ', '), '\nMethod = ', 
                  ifelse(modal, 'deterministic', 
                         paste0('probabilistic, ', n_bootstraps, ' bootstraps')))
  
  main_table_imd <- data %>% 
    group_by(imd_quintile, bootstrap) %>% 
    summarise(prop_imd = n()/n_participants) %>% 
    group_by(imd_quintile) %>% 
    summarise(med_prop = median(prop_imd),
              lower_prop = quantile(prop_imd, (1 - ci_width)/2),
              upper_prop = quantile(prop_imd, (1 - (1 - ci_width)/2))) 
  
  plot1 <- main_table_imd %>% 
    ggplot() + 
    geom_errorbar(aes(x = imd_quintile, ymin = lower_prop, ymax = upper_prop, 
                      col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                  width = 0.4) + 
    geom_point(aes(x = imd_quintile, y = med_prop, 
                   color = as.factor(imd_quintile), group = as.factor(imd_quintile))) + 
    geom_label(aes(x = imd_quintile, y = 0.06 + upper_prop, label = round(med_prop,2), 
                  fill = as.factor(imd_quintile)), col = c('white','white','black','black','black')) + 
    theme_bw() + labs(col = '', x = 'IMD quintile', y = 'Proportion of survey participants') + 
    ylim(c(0,0.4)) + theme(legend.position = 'none') + 
    scale_color_manual(values = imd_quintile_colors) +
    scale_fill_manual(values = imd_quintile_colors); plot1
  
  bars <- fcn_barplot_imd(data, 
                          c('p_age_group','p_ethnicity','p_income','p_engreg'),
                          save = F)
                          
  age_spec_contacts <- data %>% 
    group_by(p_age_group, imd_quintile, bootstrap) %>% 
    summarise(mean = mean(n_contacts + large_n)) %>% 
    group_by(p_age_group, imd_quintile) %>% 
    summarise(med = median(mean),
              lower = quantile(mean, (1 - ci_width)/2),
              upper = quantile(mean, (1 - (1 - ci_width)/2))) %>% 
    ggplot() +
    geom_errorbar(aes(p_age_group, ymin = lower, ymax = upper, 
                      col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                  width = 0.2, lwd = 0.8, position = position_dodge(width = 0.9)) +
    geom_point(aes(p_age_group, med, col = as.factor(imd_quintile)), 
               size = 3, position = position_dodge(width = 0.9)) +
    theme_bw() + ylim(c(0,25)) + 
    labs(color = 'IMD quintile', fill = 'IMD quintile', 
         x = 'Age group', y = 'Mean contacts') + 
    ggtitle('Age-specific mean contacts') + 
    scale_color_manual(values = imd_quintile_colors) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'none'); age_spec_contacts
                         
  # plot 
  plot_summary <- function(
    i){
  
    census <- census_data_list[[i]]
    var_name <- colnames(census)[colnames(census) %notin% c('imd_quintile','n','n_tot','prop')]
    
    summ_filt <- scores %>%
      filter(variable == var_name) %>% 
      mutate(stat = mean(stat))
    
    data_filt <- data.table(data)
    data_filt <- data_filt[get(var_name) %in% unname(unlist(unique(census[,var_name]))), ]
    
    p <- fcn_rev_errorbarplot_imd(data_output = data_filt, 
                                  vars_list = var_name,
                                  true_vals = census,
                                  ci_width = 0.95,
                                  facet = T
    )
    
    p + ggtitle(paste0(format_legend(var_name), ', ', toupper(summary_stat),' = ', 
                       round(mean(scores$stat), 3)))
    
  }
  
  plots <- map(
    .x = 1:length(census_data_list),
    .f = plot_summary
  )
  
  n_col <- ceiling(sqrt(length(census_data_list)))
  n_row <- ceiling(length(census_data_list)/n_col)
  
  patched_summ <- patchwork::wrap_plots(plots, ncol = n_col); patched_summ
  
  ### ALL PLOTS ###
  
  layout <- '
  AAAADDDDDDDDDD
  BBBBDDDDDDDDDD
  BBBBDDDDDDDDDD
  CCCCDDDDDDDDDD
  '
  
  patched_total <- plot1 + bars + age_spec_contacts + patched_summ + plot_layout(design = layout) +
    plot_annotation(title = title_patch,
                    theme = theme(plot.title = element_text(size = 18)))
  
  patched_total
  
}


## FUNCTION TO CALCULATE MEAN SQUARED ERROR OF SURVEY PROP IN ##
## IMD QUINTILE COMPARED TO TRUE CENSUS PROP IN IMD ##

fcn_mse <- function(
    survey_data,
    census_data_l
){
  
  errors <- data.table()
  
  for(i in 1:length(census_data_l)){
    
    census <- census_data_l[[i]]
    var <- colnames(census)[colnames(census) %notin% c('imd_quintile','n','n_tot','prop')]
  
    if(length(var) > 1){stop(paste0('More than one usable column name (list entry ', i, ')'))}
    
    if(var == 'p_ethnicity'){
      survey_data <- survey_data %>% 
        filter(!p_ethnicity %like% 'Prefer')
    }
    if(var == 'p_sec_input'){
      survey_data <- survey_data %>% 
        filter(p_sec_input %in% as.character(1:7))
      
      census <- census %>% 
        mutate(p_sec_input = as.character(p_sec_input))
    }
    survey_data <- data.table(survey_data)
    survey_data <- survey_data[get(var) %in% unname(unlist(unique(census[,var]))), ]
    
    survey_var_props <- survey_data %>% 
      select(!!sym(var)) %>% drop_na() %>% 
      group_by(!!sym(var)) %>% 
      summarise(n = n(), n_tot = nrow(survey_data), survey_var_prop = n/n_tot)
    
    survey_props <- survey_data %>% 
      group_by(bootstrap, imd_quintile) %>% 
      mutate(n_tot = n()) %>% 
      group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
      summarise(n_imd = n()) %>% 
      mutate(prop_bs = n_imd/n_tot) %>% 
      group_by(imd_quintile, !!sym(var)) %>% 
      summarise(survey_var_imd_prop_med = median(prop_bs)) %>% 
      drop_na() %>% 
      left_join(survey_var_props, by = var)
    
    census_var_props <- census %>% 
      ungroup() %>% mutate(n_tot = sum(n)) %>% 
      group_by(!!sym(var), n_tot) %>% 
      summarise(n = sum(n)) %>% 
      mutate(census_var_prop = n/n_tot)
    
    census_props <- census %>% ungroup() %>% 
      group_by(imd_quintile) %>% 
      mutate(n_tot = sum(n)) %>% 
      mutate(census_var_imd_prop = n/n_tot) %>% 
      left_join(census_var_props %>% select(!!sym(var), census_var_prop), by = var) %>% 
      drop_na()
    
    all_props <- survey_props %>% 
      select(imd_quintile, !!sym(var), survey_var_imd_prop_med, survey_var_prop) %>% 
      left_join(census_props %>% select(imd_quintile, !!sym(var), census_var_imd_prop, census_var_prop), 
                by = c('imd_quintile', var)) %>% 
      mutate(stat = ((survey_var_imd_prop_med/survey_var_prop) - 
                              (census_var_imd_prop/census_var_prop))^2) %>% 
      drop_na() %>% 
      mutate(above_below = '')
    
    for(k_row in 1:nrow(all_props)){
      if(all_props$survey_var_imd_prop_med[k_row]/all_props$survey_var_prop[k_row] >= 
         all_props$census_var_imd_prop[k_row]/all_props$census_var_prop[k_row]){
        all_props$above_below[k_row] <- 'above'
      }else{
        all_props$above_below[k_row] <- 'below'
      }
    }
    
    errors_i <- all_props %>% 
      group_by(imd_quintile) %>% 
      mutate(mean_sq_err = mean(stat)) %>% 
      mutate(variable = var)
    
    colnames(errors_i)[2] <- 'category'
    
    errors <- rbind(errors, errors_i)
    
  }
  
  errors
  
}


## FUNCTION TO CALCULATE WEIGHTED INTERVAL SCORES OF SURVEY PROP IN ##
## IMD QUINTILE COMPARED TO TRUE CENSUS PROP IN IMD ##

fcn_wis <- function(
    survey_data,
    census_data_l
){
  
  wis_out <- data.table()
  
  for(i in 1:length(census_data_l)){
    
    census <- census_data_l[[i]]
    var <- colnames(census)[colnames(census) %notin% c('imd_quintile','n','n_tot','prop')]
    
    if(length(var) > 1){stop(paste0('More than one usable column name (list entry ', i, ')'))}
    
    if(var == 'p_ethnicity'){
      survey_data <- survey_data %>% 
        filter(!p_ethnicity %like% 'Prefer')
    }
    if(var == 'p_sec_input'){
      survey_data <- survey_data %>% 
        filter(p_sec_input %in% as.character(1:7))
      
      census <- census %>% 
        mutate(p_sec_input = as.character(p_sec_input))
    }
    survey_data <- data.table(survey_data)
    survey_data <- survey_data[get(var) %in% unname(unlist(unique(census[,var]))), ]
    
    survey_var_props <- survey_data %>% 
      select(!!sym(var)) %>% drop_na() %>% 
      group_by(!!sym(var)) %>% 
      summarise(n = n(), n_tot = nrow(survey_data), survey_var_prop = n/n_tot)
    
    survey_props <- survey_data %>% 
      group_by(bootstrap, imd_quintile) %>% 
      mutate(n_tot = n()) %>% 
      group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
      summarise(n_imd = n()) %>% 
      mutate(survey_var_imd_prop_bs = n_imd/n_tot) %>% 
      drop_na() %>% 
      left_join(survey_var_props, by = var)
    
    census_var_props <- census %>% 
      ungroup() %>% mutate(n_tot = sum(n)) %>% 
      group_by(!!sym(var), n_tot) %>% 
      summarise(n = sum(n)) %>% 
      mutate(census_var_prop = n/n_tot)
    
    census_props <- census %>% ungroup() %>% 
      group_by(imd_quintile) %>% 
      mutate(n_tot = sum(n)) %>% 
      mutate(census_var_imd_prop = n/n_tot) %>% 
      left_join(census_var_props %>% select(!!sym(var), census_var_prop), by = var) %>% 
      drop_na()
    
    errors <- survey_props %>% 
      select(bootstrap, imd_quintile, !!sym(var), survey_var_imd_prop_bs, survey_var_prop) %>% 
      left_join(census_props %>% select(imd_quintile, !!sym(var), census_var_imd_prop, census_var_prop), 
                by = c('imd_quintile', var)) %>% 
      mutate(survey_ratio = (survey_var_imd_prop_bs/survey_var_prop),
             census_ratio = (census_var_imd_prop/census_var_prop))
    
    errors <- data.table(errors)
    
    unique_scores <- unique(errors %>% ungroup() %>% select(imd_quintile, !!sym(var))) %>% 
      mutate(stat = NA,
             variable = var,
             above_below = '')
    
    for(i_row in 1:nrow(unique_scores)){
      
      wis_1 <- errors[imd_quintile == unname(unlist(unique_scores[i_row, 'imd_quintile'])) &
                      get(var) == unname(unlist(unique_scores[i_row, ..var]))]
      
      quantile_levels <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
      
      predicted_intervals <- expand.grid(quantile_levels, wis_1$survey_ratio) %>% 
        group_by(Var1) %>%
        summarise(q = quantile(Var2, probs = Var1[1]))
      
      wis_score <- wis(observed = unique(wis_1$census_ratio),
          predicted = predicted_intervals$q,
          quantile_level = quantile_levels)
      
      unique_scores$stat[i_row] <- wis_score
      
      if(median(wis_1$survey_ratio) >= unique(wis_1$census_ratio)){
        unique_scores$above_below[i_row] <- 'above'
      }else{
        unique_scores$above_below[i_row] <- 'below'
      }
      
    }
    
    colnames(unique_scores)[2] <- 'category'
    
    wis_out <- rbind(wis_out, unique_scores)
    
  }
  
  wis_out
  
}


# FUNCTION TO CALCULATE CONTINUOUS RANKED PROBABILITY SCORES OF SURVEY PROP IN ##
## IMD QUINTILE COMPARED TO TRUE CENSUS PROP IN IMD ##

fcn_crps <- function(
    survey_data,
    census_data_l
){
  
  crps_out <- data.table()
  
  for(i in 1:length(census_data_l)){
    
    census <- census_data_l[[i]]
    var <- colnames(census)[colnames(census) %notin% c('imd_quintile','n','n_tot','prop')]
    
    if(length(var) > 1){stop(paste0('More than one usable column name (list entry ', i, ')'))}
    
    if(var == 'p_ethnicity'){
      survey_data <- survey_data %>% 
        filter(!p_ethnicity %like% 'Prefer')
    }
    if(var == 'p_sec_input'){
      survey_data <- survey_data %>% 
        filter(p_sec_input %in% as.character(1:7))
      
      census <- census %>% 
        mutate(p_sec_input = as.character(p_sec_input))
    }
    survey_data <- data.table(survey_data)
    survey_data <- survey_data[get(var) %in% unname(unlist(unique(census[,var]))), ]
    
    survey_var_props <- survey_data %>% 
      select(!!sym(var)) %>% drop_na() %>% 
      group_by(!!sym(var)) %>% 
      summarise(n = n(), n_tot = nrow(survey_data), survey_var_prop = n/n_tot)
    
    survey_props <- survey_data %>% 
      group_by(bootstrap, imd_quintile) %>% 
      mutate(n_tot = n()) %>% 
      group_by(bootstrap, imd_quintile, !!sym(var), n_tot) %>% 
      summarise(n_imd = n()) %>% 
      mutate(survey_var_imd_prop_bs = n_imd/n_tot) %>% 
      drop_na() %>% 
      left_join(survey_var_props, by = var)
    
    census_var_props <- census %>% 
      ungroup() %>% mutate(n_tot = sum(n)) %>% 
      group_by(!!sym(var), n_tot) %>% 
      summarise(n = sum(n)) %>% 
      mutate(census_var_prop = n/n_tot)
    
    census_props <- census %>% ungroup() %>% 
      group_by(imd_quintile) %>% 
      mutate(n_tot = sum(n)) %>% 
      mutate(census_var_imd_prop = n/n_tot) %>% 
      left_join(census_var_props %>% select(!!sym(var), census_var_prop), by = var) %>% 
      drop_na()
    
    errors <- survey_props %>% 
      select(bootstrap, imd_quintile, !!sym(var), survey_var_imd_prop_bs, survey_var_prop) %>% 
      left_join(census_props %>% select(imd_quintile, !!sym(var), census_var_imd_prop, census_var_prop), 
                by = c('imd_quintile', var)) %>% 
      mutate(survey_ratio = (survey_var_imd_prop_bs/survey_var_prop),
             census_ratio = (census_var_imd_prop/census_var_prop))
    
    errors <- data.table(errors)
    
    unique_scores <- unique(errors %>% ungroup() %>% select(imd_quintile, !!sym(var))) %>% 
      mutate(stat = NA,
             variable = var,
             above_below = '')
    
    for(i_row in 1:nrow(unique_scores)){
      
      crps_1 <- errors[imd_quintile == unname(unlist(unique_scores[i_row, 'imd_quintile'])) &
                        get(var) == unname(unlist(unique_scores[i_row, ..var]))]
      
      crps_score <- scoringutils::crps_sample(
        observed = unique(crps_1$census_ratio),
        predicted = crps_1$survey_ratio)
      
      unique_scores$stat[i_row] <- crps_score
      
      if(median(crps_1$survey_ratio) >= unique(crps_1$census_ratio)){
        unique_scores$above_below[i_row] <- 'above'
      }else{
        unique_scores$above_below[i_row] <- 'below'
      }
      
    }
    
    colnames(unique_scores)[2] <- 'category'
    
    crps_out <- rbind(crps_out, unique_scores)
    
  }
  
  crps_out
  
}


## FUNCTION TO SIMPLIFY VARIABLE LABELS ##

simp_labels <- function(string){
  if(string == 'p_sec_input'){return('nssec')}
  string <- gsub('_grp','',string)
  string <- gsub('p_','',string)
  string <- gsub('c_','',string)
  string <- gsub('_cd','',string)
  string <- gsub('_nm','',string)
  string <- gsub('_input','',string)
  return(string)
}



# function to turn column names into nice text

format_legend <- function(string){
  
  if(string == 'imd_quintile'){return('IMD quintile')}
  if(string %in% c('eng_reg','p_engreg','engreg')){return('Region')}
  if(string == 'urban_rural'){return('Urban/rural')}
  if(string == 'lsoa21cd'){return('LSOA')}
  if(string == 'pcd1'){return('PCD1')}
  if(string == 'p_income'){return('Household income')}
  if(string == 'p_hiqual'){return('Highest qualification')}
  if(string == 'p_age_group'){return('Age group')}
  if(string == 'p_ethnicity'){return('Ethnicity')}
  if(string == 'p_emp_1'){return('Employment status')}
  if(string == 'p_sec_input'){return('NS-SEC')}
  if(string == 'sec_input'){return('NS-SEC')}
  
  # else just try something
  out_str <- gsub('_grp','',string)
  out_str <- gsub('p_','',out_str)
  out_str <- gsub('hh', 'Household', out_str)
  out_str <- gsub('_cd', '', out_str)
  out_str <- gsub('_nm', '', out_str)
  out_str <- gsub('_', ' ', out_str)
  firstup(out_str)
  
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

  
variables_from_name <- function(varname){
  # out <- if(varname == 'engreg'){c('eng_reg')}else{
  #   if(varname == 'pcd1'){c('pcd1')}else{
  #     if(varname == 'pcd1age'){c('pcd1','age_grp')}else{
  #       if(varname == 'pcd1ageethn'){c('pcd1','age_grp_6','p_ethnicity')}else{
  #         if(varname == 'pcd1household'){c('pcd1','hh_size_nm','hh_tenure_nm')}else{
  #           if(varname == 'pcd1agehiqualnssec'){c('pcd1','age_grp_8','p_sec_input','p_hiqual')}}}}}}
  
  if(! varname %in% names(variables_and_names)){stop('varname not found')}
  
  out <- variables_and_names[[varname]]
  
  out
}

name_from_variables <- function(vars){
  # out <- if(vars == c('eng_reg')){'engreg'}else{
  #   if(vars == c('pcd1')){'pcd1'}else{
  #     if(vars == c('pcd1','age_grp')){'pcd1age'}else{
  #       if(vars == c('pcd1','age_grp_6','p_ethnicity')){'pcd1ageethn'}else{
  #         if(vars == c('pcd1','hh_size_nm','hh_tenure_nm')){'pcd1household'}else{
  #           if(vars == c('pcd1','age_grp_8','p_sec_input','p_hiqual')){'pcd1agehiqualnssec'}}}}}}
  
  i <- 0; j <- 0
  
  for(k in 1:length(variables_and_names)){
    if(length(variables_and_names[[k]]) == length(vars)){
      if(sum(variables_and_names[[k]] == vars) == length(vars)){
        i <- k
        j <- j + 1
      }
    }
  }
  
  if(j > 1){stop('vars match more than one list entry')}
  if(j == 0){stop("vars don't match any list entries")}
  
  out <- names(variables_and_names)[i]
  
  out
}

variables_and_names <- list(
  'engreg' = c('eng_reg'),
  'pcd1' = c('pcd1'),
  'pcd1age' = c('pcd1','age_grp'),
  'pcd1ageethn' = c('pcd1','age_grp_6','p_ethnicity'),
  'pcd1household' = c('pcd1','hh_size_nm','hh_tenure_nm'),
  'pcd1agehiqualnssec' = c('pcd1','age_grp_8','p_sec_input','p_hiqual'),
  'pcd1nssectenure' = c('pcd1','p_sec_input','hh_tenure_nm'),
  'pcd1ethntenure' = c('pcd1','p_ethnicity','hh_tenure_nm')
)














