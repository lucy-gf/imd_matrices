
### folders
source_dir <- paste0(getwd(),"/scripts/epidem/codes")
output_dir <- paste0(getwd(),"/output")

### Basic setting
source(file = paste0(source_dir,"/setup.r")) #repo

### Diseases cycle
pset$Disease <- "Influenza"
#   run model, plot figures, write summaries
source(file = paste0(source_dir,"/modelrun.r")) #repo

