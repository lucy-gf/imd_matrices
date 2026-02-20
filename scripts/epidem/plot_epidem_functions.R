
#### PLOTTING FUNCTIONS ####

plot_trajectory <- function(
    data_in,
    variables = c(),
    cumulative = F,
    regional = F
){
  
  data_in <- data.table(data_in)
  
  ## column name checks
  if('sim' %notin% colnames(data_in) | 'time' %notin% colnames(data_in)){
    stop('Sim/time not in column names')
  }
  if(regional & 'p_engreg' %notin% colnames(data_in)){
    stop('Region not in column names')
  }
  for(var in variables){
    if(var %notin% colnames(data_in)){
      stop(paste0(var, ' not in column names'))
    }
  }
  
  ## grouping vars
  grouping_vars_in <- if(!regional){variables}else{c(variables, 'p_engreg')}
  grouping_vars <- c('sim','time', grouping_vars_in)
  time_var_vec <- c('time', grouping_vars_in)
  
  ## y axis label
  y_lab <- if(cumulative){'Cumulative infections per 1000 population'}else{
    'Infections per 1000 population'
  }
  
  ## total population data table
  pop_vec <- c(grouping_vars, 'pop')
  tot_pop <- data_in[, ..pop_vec][, lapply(.SD, sum), by = grouping_vars]
  
  ## total infections data table
  inf_vec <- c(grouping_vars, 'infections')
  dat_inf <- data_in[, ..inf_vec][, lapply(.SD, sum), by = grouping_vars]
  dat_inf <- dat_inf[tot_pop, on = grouping_vars]
  dat_inf[, attack_rate := infections/pop]
  
  ## median and CI data table
  attack_rate_vec <- c(time_var_vec, 'attack_rate')
  dat_med <- dat_inf[, ..attack_rate_vec][, lapply(.SD, median), by = time_var_vec]
  setnames(dat_med, 'attack_rate', 'median')
  dat_l <- dat_inf[, ..attack_rate_vec][, lapply(.SD, l95_func), by = time_var_vec]
  setnames(dat_l, 'attack_rate', 'l95')
  dat_u <- dat_inf[, ..attack_rate_vec][, lapply(.SD, u95_func), by = time_var_vec]
  setnames(dat_u, 'attack_rate', 'u95')
  
  dat_agg <- cbind(dat_med, l95 = dat_l$l95, u95 = dat_u$u95)
  
  ## basic plot
  p <- ggplot(dat_agg, aes(x=time)) + 
    theme_bw() +
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = y_lab, x = "Day")
  
  ## no stratifications
  if(length(variables) == 0){
    
    if(!regional){
      
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95), alpha=0.25, fill = 'darkgreen')  +
        geom_line(aes(y = 1000*median), lwd=0.8, col = 'darkgreen') 
      
    }else{
      
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, fill = p_engreg), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = p_engreg), lwd=0.8) +
        scale_fill_manual(values = colors_p_engreg) + 
        scale_color_manual(values = colors_p_engreg) +
        labs(col = 'Region', fill = 'Region')
      
    }
    
  }else{
    
    ## age-stratified
    if(variables == c('age')){
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, 
                        fill = age, group = age), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = age, group = age), lwd=0.8) +
        scale_fill_manual(values = age_colors) + 
        scale_color_manual(values = age_colors) +
        labs(col = 'Age', fill = 'Age')
    }
    
    ## imd-stratified
    if(variables == c('imd')){
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, 
                        fill = imd, group = imd), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = imd, group = imd), lwd=0.8) +
        scale_fill_manual(values = imd_quintile_colors) + 
        scale_color_manual(values = imd_quintile_colors) +
        labs(col = 'IMD quintile', fill = 'IMD quintile')
    }
    
  }
  
  ## facet if regional (and some other stratification)
  if(regional & length(variables) > 0){p <- p + facet_wrap(. ~ p_engreg, scales = 'free')}
  
  p
  
}

barchart_plot <- function(data_in, regional = F){
  
  vec <- c('imd','age')
  if(regional){ vec <- c(vec, 'p_engreg') }
  
  plot_dat <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(median = median(attack_rate),
              l95 = l95_func(attack_rate),
              u95 = u95_func(attack_rate)) 
    
  p <- ggplot(plot_dat) + 
    geom_bar(aes(x = age, y = 1000*median, fill = as.factor(imd)),
             stat = 'identity', position = 'dodge') +
    geom_errorbar(aes(x = age, ymin = 1000*l95, ymax = 1000*u95, 
                      group = as.factor(imd)), 
                  width = 0.4, position = position_dodge(width = 0.9), alpha= 0.75) +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Attack rate per 1000 population", x = "Age group", color = "IMD quintile", fill = 'IMD quintile')

  if(regional){
    
    p <- p +  facet_wrap(.~ p_engreg, scales = 'free') 
    
  }
  
  p
  
}

age_spec_infections <- function(data_in){
  
  p1 <- data_in %>% 
    group_by(age, imd) %>% 
    summarise(med_ar = median(attack_rate),
              l_ar = l95_func(attack_rate),
              u_ar = u95_func(attack_rate)) %>% 
    ggplot() +
    geom_ribbon(aes(x = age, ymin = 1000*l_ar, ymax = 1000*u_ar,
                    fill = as.factor(imd), group = as.factor(imd)),
                alpha = 0.25) +
    geom_line(aes(x = age, y = 1000*med_ar, col = as.factor(imd), group = as.factor(imd)),
                lwd = 0.8) +
    ylim(c(0,NA)) + 
    scale_color_manual(values = imd_quintile_colors) +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme_bw() + 
    labs(x = 'Age group', y = 'Attack rate per 1000 population',
         col = 'IMD quintile', fill = 'IMD quintile') +
    theme(text=element_text(size=12)); p1
  
  p2 <- data_in %>% 
    group_by(age, imd) %>% 
    summarise(med_inf = median(infections),
              l_inf = l95_func(infections),
              u_inf = u95_func(infections)) %>% 
    ggplot() +
    geom_ribbon(aes(x = age, ymin = l_inf/1000, ymax = u_inf/1000,
                    fill = as.factor(imd), group = as.factor(imd)),
                alpha = 0.25) +
    geom_line(aes(x = age, y = med_inf/1000, col = as.factor(imd), group = as.factor(imd)),
              lwd = 0.8) +
    ylim(c(0,NA)) + 
    scale_color_manual(values = imd_quintile_colors) +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme_bw() + 
    labs(x = 'Age group', y = 'Infections (thousands)',
         col = 'IMD quintile', fill = 'IMD quintile') +
    theme(text=element_text(size=12)); p2
  
  p1 + p2 + plot_layout(nrow = 2, guides = 'collect') + 
    plot_annotation(tag_levels = 'a',tag_prefix = '(', tag_suffix = ')')
  
}

imd_violin_plot <- function(data_in, regional = F, combined = F){
  
  if(regional + combined == 2){stop("Can't be in regional and combined analyses")}
  
  vec <- c('sim','imd')
  if(regional){vec <- c(vec, 'p_engreg')}
  if(combined){vec <- c(vec, 'model')}
  vec_no_sim <- vec[!vec=='sim']
  
  p <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>% 
    ungroup() %>% mutate(attack_rate = infections/pop) %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(attack_rate)) %>% 
    ggplot() + 
    theme_bw() + 
    ylim(c(0,NA)) + 
    theme(text=element_text(size=12)) +
    labs(y = "Attack rate per 1000 population", x = "IMD Quintile", color = "IMD quintile", fill = 'IMD quintile')
  
  if(!combined){
    p <- p + 
      geom_violin(aes(x = imd, y = 1000*attack_rate, fill = imd, col = imd), alpha = 0.4)  +
      geom_point(aes(x = imd, y = 1000*median, col = imd), size = 4) +
      scale_fill_manual(values = imd_quintile_colors) + 
      scale_color_manual(values = imd_quintile_colors) +
      theme(legend.position = 'none')
  }else{
    p <- p + 
      geom_violin(aes(x = imd, y = 1000*attack_rate, fill = interaction(imd,model), col = interaction(imd,model), 
                      group = interaction(model,imd)), alpha = 0.4)  +
      geom_point(aes(x = imd, y = 1000*median, col = interaction(imd,model), group = model), 
                 size = 4, position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = imd_model_colors,
                        labels = imd_model_labels) + 
      scale_color_manual(values = imd_model_colors,
                         labels = imd_model_labels) + 
      labs(col = '',fill = '')
  }
  
  if(regional){
    p <- p + 
      facet_wrap(.~p_engreg, scales = 'fixed')
  }
  
  p
  
}

rel_age_violin_plot <- function(data_in,
                                base_age,
                                regional = F){
  
  vec <- c('sim','age')
  if(regional){ vec <- c(vec, 'p_engreg') }
  vec_no_age <- vec[!vec=='age']
  
  age_ars <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>%
    mutate(attack_rate = infections/pop) %>% 
    select(!!!syms(vec), attack_rate) %>% ungroup()
  
  base_age_ars <- age_ars %>% 
    filter(age == base_age) %>% 
    rename(base_attack_rate = attack_rate) %>% 
    select(!age)
  
  rel_age_ars <- age_ars %>% 
    left_join(base_age_ars, by = vec_no_age) %>% 
    mutate(rel_ar = attack_rate/base_attack_rate)
  
  p <- rel_age_ars %>% 
    group_by(age) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=age)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_violin(aes(x = age, y = rel_ar, fill = age, col = age), alpha = 0.4)  +
    geom_point(aes(x = age, y = median, col = age), size = 4)  +
    theme_bw() +
    scale_fill_manual(values = colors_p_age_group) + 
    scale_color_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=12),
          legend.position = 'none') +
    labs(y = "Relative attack rate", x = 'Age')
  
  if(regional){
    p <- p + 
      facet_wrap(. ~ p_engreg, scales = 'free') 
  }
  
  p
  
}

rel_imd_violin_plot <- function(data_in,
                                base_imd,
                                regional = F,
                                combined = F){
  
  if(regional + combined == 2){stop("Can't be in regional and combined analyses")}
  
  vec <- c('sim','imd')
  if(regional){ vec <- c(vec, 'p_engreg') }
  if(combined){vec <- c(vec, 'model')}
  vec_no_imd <- vec[!vec=='imd']
  vec_no_sim <- vec[!vec=='sim']
  
  imd_ars <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>%
    mutate(attack_rate = infections/pop) %>% ungroup() %>% 
    select(!!!syms(vec), attack_rate)
  
  base_imd_ars <- imd_ars %>% 
    filter(imd == base_imd) %>% 
    rename(base_attack_rate = attack_rate) %>% 
    select(!imd)
  
  rel_imd_ars <- imd_ars %>% 
    left_join(base_imd_ars, by = vec_no_imd) %>% 
    mutate(rel_ar = attack_rate/base_attack_rate)
  
  p <- rel_imd_ars %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    theme_bw() +
    theme(legend.position = 'none',
          text=element_text(size=12)) +
    labs(y = "Relative attack rate", x = 'IMD Quintile')
  
  if(!combined){
    p <- p + 
      geom_violin(aes(x = imd, y = rel_ar, fill = imd, col = imd), alpha = 0.4)  +
      geom_point(aes(x = imd, y = median, col = imd), size = 4) +
      scale_fill_manual(values = imd_quintile_colors) + 
      scale_color_manual(values = imd_quintile_colors) 
  }else{
    p <- p + 
      geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                      col = interaction(imd,model), group = interaction(model,imd)), alpha = 0.4)  +
      geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
                 size = 4, position = position_dodge(width = 0.9)) +
      ylim(c(0.7,1.3)) +
      scale_fill_manual(values = imd_model_colors,
                        labels = imd_model_labels) + 
      scale_color_manual(values = imd_model_colors,
                         labels = imd_model_labels) + 
      labs(col = '',fill = '')
  }
  
  if(regional){
    p <- p +
      facet_wrap(. ~ p_engreg)
  }
  
  p
  
}

age_standardised_rel_imd_violin_plot <- function(
    demog_in,
    data_in,
    base_imd,
    regional = F,
    combined = F
    ){
  
  if(regional + combined == 2){stop("Can't be in regional and combined analyses")}
  
  pop_vec <- c('age')
  if(regional){pop_vec <- c(pop_vec, 'p_engreg')}
  reg_vec <- if(!regional){c()}else{c('p_engreg')}
  
  ## standard population
  standard_pop <- demog_in %>% 
    rename(age = Age) %>% 
    group_by(!!!syms(pop_vec)) %>% 
    summarise(st_pop = sum(Population)) %>% 
    group_by(!!!syms(reg_vec)) %>% 
    mutate(st_total_pop = sum(st_pop),
           standard_prop = st_pop/st_total_pop) 
  
  vec <- c('sim','imd')
  if(regional){vec <- c(vec, 'p_engreg')}
  if(combined){vec <- c(vec, 'model')}
  vec_no_imd <- vec[vec!='imd']
  vec_no_sim <- vec[vec!='sim']
  
  age_standardised_ars <- data_in %>% 
    left_join(standard_pop, by = pop_vec) %>% 
    mutate(imd_ar = infections/pop,
           infected = imd_ar*standard_prop) %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(as_attack_rate = sum(infected)) %>% 
    ungroup()
  
  base_imd_ars <- age_standardised_ars %>% 
    filter(imd == base_imd) %>% 
    rename(base_as_attack_rate = as_attack_rate) %>% 
    select(!imd)
  
  rel_imd_ars_as <- age_standardised_ars %>% 
    left_join(base_imd_ars, by = vec_no_imd) %>% 
    mutate(rel_ar = as_attack_rate/base_as_attack_rate)
  
  cat('\n')
  cat((rel_imd_ars_as %>% 
               group_by(!!!syms(vec_no_sim)) %>% 
               summarise(median = median(rel_ar),
                         l = l95_func(rel_ar),
                         u = u95_func(rel_ar),
                         neat = paste0(round(100*(median-1), 3), ' (', 
                                       round(100*(l-1),3), ' - ', round(100*(u-1),3), ')')))$neat,
      sep = ', ')
  cat('\n')
  
  p <- rel_imd_ars_as %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    theme_bw() +
    theme(text=element_text(size=12)) +
    labs(y = "Relative attack rate (age-standardised)", x = 'IMD Quintile')
  
  if(!combined){
    p <- p + 
      geom_violin(aes(x = imd, y = rel_ar, fill = imd, col = imd), alpha = 0.4)  +
      geom_point(aes(x = imd, y = median, col = imd), size = 4) +
      scale_fill_manual(values = imd_quintile_colors) + 
      scale_color_manual(values = imd_quintile_colors) + 
      theme(legend.position = 'none')
  }else{
    p <- p + 
      geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                      col = interaction(imd,model), group = interaction(model,imd)), alpha = 0.4)  +
      geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
                 size = 4, position = position_dodge(width = 0.9)) +
      ylim(c(0.7,1.3)) +
      scale_fill_manual(values = imd_model_colors,
                        labels = imd_model_labels) + 
      scale_color_manual(values = imd_model_colors,
                         labels = imd_model_labels) + 
      labs(col = '',fill = '')
  }
  
  if(regional){
    p <- p +
      facet_wrap(.~ p_engreg)
  }
  
  p
  
}

