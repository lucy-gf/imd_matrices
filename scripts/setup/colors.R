
## COLORS FOR IMD ANALYSIS ##

eng_reg_colors <- c("London"="#31688EFF", "North West" = '#CC4678FF',
                    'Yorkshire and The Humber' = '#65156EFF', "North East" = '#006837', 
                    "West Midlands" = '#F89441FF', 'East Midlands' = '#e78ac3',
                    'South West' = '#253494', 'East of England' = '#bd0026', 'South East' = '#c2e699')

colors_p_engreg <- c('East Midlands' = '#e78ac3', 'East of England' = '#bd0026', 'Greater London' = '#31688EFF',
                     'North East' = '#006837', 'North West' = '#CC4678FF', 'South East' = '#c2e699', 
                     'South West' = '#253494', 'West Midlands' = '#F89441FF', 'Yorkshire and the Humber' = '#65156EFF')

urban_rural_colors <- c('Urban' = '#756bb1', 'Rural' = '#e6550d')

imd_quintile_colors <- c('1' = '#7a0177', '2' = '#c51b8a', '3' = '#f768a1', '4' = '#fa9fb5', '5' = '#fcc5c0')

colors_p_urban_rural <- c('Urban' = 'dodgerblue3', 'Rural' = 'green4')

colors_p_sec_input <- c('1' = '#d9f0a3', '2' = '#addd8e', '3' = '#78c679',
                        '4' = '#41ab5d', '5' = '#238443', '6' = '#006837', '7' = '#004529')

colors_p_hiqual <- c('No qualifications' = '#08589e', '1-4 GCSEs' = '#2b8cbe', '5+ GCSEs' = '#4eb3d3',
                        '2+ A levels' = '#7bccc4', 'Degree' = '#a8ddb5', 'Apprentice/vocational' = '#41ae76')

colors_p_ethnicity <- c('Asian' = '#67a9cf', 'Black' = '#ef8a62', 'Mixed' = '#af8dc3',
                        'Other' = '#2166ac', 'White' = '#b2182b')

colors_hh_size_nm <- c('1 person in household' = '#dadaeb', '2 people in household' = '#bcbddc', 
                       '3 people in household' = '#9e9ac8', '4 people in household' = '#807dba', 
                       '5 people in household' = '#6a51a3', '6 or more people in household' = '#4a1486')

colors_hh_tenure_nm <- c('Owned: Owns outright' = 'darkorange2', 
                         'Owned: Owns with a mortgage or loan or shared ownership' = 'orangered2', 
                         'Private rented: Other private rented or lives rent free' = 'lightskyblue2', 
                         'Private rented: Private landlord or letting agency' = 'steelblue2', 
                         'Social rented: Other social rented' = 'violet', 
                         'Social rented: Rents from council or Local Authority' = 'mediumpurple')

colors_p_age_group <- c('0-4' = '#4d004b', '5-9' = '#7a0177',
                        '10-14' = '#ae017e', '15-19' = '#dd3497',
                        '20-24' = '#f768a1', '25-29' = '#fa9fb5',
                        '30-34' = '#fcc5c0', '35-39' = '#fec44f',
                        '40-44' = '#fe9929', '45-49' = '#ec7014',
                        '50-54' = '#ef6548', '55-59' = '#d7301f',
                        '60-64' = '#990000', '65-69' = '#08589e',
                        '70-74' = '#3690c0', '75+' = '#a6bddb')

# model_colors <- c('det_engreg' = '#bdd7e7', 'prob_engreg' = '#08519c',
#                   'det_pcd1' = '#bae4b3', 'prob_pcd1' = '#006d2c',
#                   'det_pcd1age' = '#fbb4b9', 'prob_pcd1age' = '#dd1c77',
#                   'det_pcd1ageethn' = '#fecc5c', 'prob_pcd1ageethn' = '#d95f0e',
#                   'det_pcd1agehiqualnssec' = '#cab2d6', 'prob_pcd1agehiqualnssec' = '#7a0177',
#                   'det_pcd1household' = '#fcae91', 'prob_pcd1household' = '#a50f15')

model_colors <- c('engreg' = '#006d2c', 'pcd1' = '#7a0177', 
                  'pcd1age' = '#fd8d3c', 'pcd1ageethn' = '#6baed6', 
                  'pcd1agehiqualnssec' = '#de2d26', 'pcd1household' = '#f768a1')

method_shapes <- c('det' = 1, 'prob' = 19)  
method_names <- c('Deterministic','Probabilistic')
names(method_names) <- c('det','prob')
