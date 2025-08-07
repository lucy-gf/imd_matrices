################################################################################

proj_name <- 'epidem_model'

### folders
input_dir  <- paste0(getwd(),"/scripts/epidem/",proj_name,"/data")
source_dir <- paste0(getwd(),"/scripts/epidem/",proj_name,"/codes")
output_dir <- paste0(getwd(),"/scripts/epidem/",proj_name,"/output")

### Basic setting
source(file = paste0(source_dir,"/setup.r")) #repo

### Diseases cycle
pset$Disease <- "Influenza"
#   run model, plot figures, write summaries
source(file = paste0(source_dir,"/modelrun.r")) #repo


################################################################################
