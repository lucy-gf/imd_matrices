
## NUMBER OF RECONNECT PARTICIPANTS IN EACH ASSIGNMENT MODEL ##

# max: number of reconnect participants living in england
reconnect_part <- readRDS(file.path('data','reconnect','reconnect_part.rds'))
eng_reconnect_part <- reconnect_part %>% filter(p_country == 'England')
max_n <- n_distinct(eng_reconnect_part$p_id)

names <- list.files(path = file.path('output','data','assignment'))
names <- names[grepl('connect_', names)]

read_func <- function(path){ readRDS(file.path('output','data','assignment',path)) }

datasets <- map(
  .x = names,
  .f = read_func
)
names(datasets) <- gsub('connect_','',gsub('.rds','',names))

n_part_func <- function(data){ data.table(n = n_distinct(data$p_id)) }

unique_vals <- map(
  .x = datasets,
  .f = n_part_func
)

unique_vals_dt <- rbindlist(unique_vals, idcol = 'model')

unique_vals_dt <- unique_vals_dt %>% 
  mutate(method = case_when(grepl('det_', model) ~ 'det',
                            T ~ 'prob'),
         model = gsub('det_|prob_', '', model)) %>% 
  mutate(n_1 = paste0(n, ' (', round(100*n/max_n, 1), '%)'))

unique_vals_dt_w <- unique_vals_dt %>% 
  pivot_wider(names_from = method, values_from = n)

unique_vals_dt_w_1 <- unique_vals_dt %>% 
  pivot_wider(names_from = method, values_from = n_1)

if(sum(abs(unique_vals_dt_w$det - unique_vals_dt_w$prob)) > 0){
  cat('Some model not equal in det and prob')
}

write_csv(unique_vals_dt_w_1, file.path('output','data','assignment','n_reconnect_participants.csv'))

