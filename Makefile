
###### INTENDED OUTPUTS ########################################################

default: localdef

localdef: allplots 

###### SUPPORT DEFINITIONS #####################################################

# if need to override directories e.g.
-include local.makefile

# convenience make definitions
R = $(strip Rscript $^ $(1) $@)

# analysis directories + build rules
CODEDIR ?= scripts
ASSIGNDIR ?= ${CODEDIR}/assign_imd
INPUTDIR ?= data
CONNECTDIR ?= ${INPUTDIR}/connect
CENSUSDIR ?= ${INPUTDIR}/census
ONSDIR ?= ${INPUTDIR}/ons
OUTDIR ?= output
FIGDIR ?= ${OUTDIR}/figures
DATDIR ?= ${OUTDIR}/data

# figure extension filetype
FIGEXT ?= png

# data extension filetype
DATAEXT ?= rds

${DATDIR} ${OUTDIR} ${FIGDIR}:
	mkdir -p $@

RENV = .Rprofile

# build renv/library & other renv infrastructure
${RENV}: install.R 
	 Rscript --vanilla $^

# scenarios for IMD assignment
ASSIGNVAR ?= engreg pcd1 pcd1age pcd1ageethn pcd1agehiqualnssec pcd1household

# methods for IMD assignment
ASSIGNMETHOD ?= prob det

# create all combinations of IMD assignments
$(foreach method,${ASSIGNMETHOD},$(foreach var,${ASSIGNVAR},$(call $(eval ALLSCN += ${method}_${var}))))

# methods of scoring fits
SCOREVAR ?= mse wis
# TODO include CPRS

# functions to make into assigned .rds
makeassigndet = $(addprefix ${DATDIR}/assignment/connect_det_,$(patsubst %,%.${DATAEXT},$(1))) 
makeassignprob = $(addprefix ${DATDIR}/assignment/connect_prob_,$(patsubst %,%.${DATAEXT},$(1))) 

# TODO add back in later? 
#clean:
#	rm -rf ${RENV}
#	rm -rf ${DATDIR}
#	rm -rf ${OUTDIR}
#	rm -rf ${FIGDIR}
#	rm -rf renv/library

##### INPUTS ###################################################################

##### Turn Census data into inputs ########## 

# TODO - currently need to source setup/packages.R, assign_imd/load_pcd_imd_census_data.R, make_imd_assignment_data_inputs.R

##### Assign IMD for each scenario ########## 

##### Deterministic ########## 

${DATDIR}/assignment/connect_det_%.rds: ${ASSIGNDIR}/assign_imd_det.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/%.csv
	$(call R,$(firstword $(subst _, ,$*)))

allassignmentdet: $(call makeassigndet, ${ASSIGNVAR})

##### Probabilistic ########## 

#${DATDIR}/assigment/connect_prob_%.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/%.csv
#	$(call R,$(firstword $(subst _, ,$*)))

#allassigmentprob: $(call makeassignprob, ${ASSIGNVAR})

##### EVALUATIONS ##############################################################

##### True distribution plots ########## 

${FIGDIR}/assignment/%/true_distrs.png: ${ASSIGNDIR}/true_distribution_plots.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

alltruedistplots: $(patsubst %,${FIGDIR}/assignment/%/true_distrs.png, ${ALLSCN})

##### Age-specific mean contact plots ########## 

${FIGDIR}/assignment/%/mean_age_contacts.png: ${ASSIGNDIR}/age_contact_plots.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allageplots: $(patsubst %,${FIGDIR}/assignment/%/mean_age_contacts.png, ${ALLSCN})

##### Make error scores ########## 

# MSE 

${DATDIR}/assignment/mse/%_scores.csv: ${ASSIGNDIR}/make_MSE_error_scores.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allmse: $(patsubst %,${DATDIR}/assignment/mse/%_scores.csv, ${ALLSCN})

# WIS 

${DATDIR}/assignment/wis/%_scores.csv: ${ASSIGNDIR}/make_WIS_error_scores.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allwis: $(patsubst %,${DATDIR}/assignment/wis/%_scores.csv, ${ALLSCN})

# Merge scores

clean:
	rm ${DATDIR}/assignment/mse/merged_scores.csv
	rm ${DATDIR}/assignment/mse/merged_scores.csv

${DATDIR}/assignment/mse/merged_scores.csv: $(patsubst %,${DATDIR}/assignment/mse/%_scores.csv,${ALLSCN})
	cat $^> $@

allmsemerged: ${DATDIR}/assignment/mse/merged_scores.csv
	
${DATDIR}/assignment/wis/merged_scores.csv: $(patsubst %,${DATDIR}/assignment/wis/%_scores.csv,${ALLSCN})
	cat $^> $@

allwismerged: ${DATDIR}/assignment/wis/merged_scores.csv

##### Main evaluation plots ########## 

${FIGDIR}/assignment/evaluation_%.png: ${ASSIGNDIR}/evaluation_plots.R ${DATDIR}/assignment/connect_%.rds ${DATDIR}/assignment/wis/%_scores.csv
	$(call R, $*)

allevalplots: $(patsubst %,${FIGDIR}/assignment/evaluation_%.png, ${ALLSCN})

##### Plot error scores ########## 

# Scatter

${FIGDIR}/assignment/eval_scatter_%.png: ${ASSIGNDIR}/plot_error_scores_scatter.R ${DATDIR}/assignment/%/merged_scores.csv
	$(call R, $*)

allscatterplots: $(patsubst %,${FIGDIR}/assignment/eval_scatter_%.png, ${SCOREVAR})

# Heatmaps

${FIGDIR}/assignment/eval_heatmap_%.png: ${ASSIGNDIR}/plot_error_scores_heatmap.R ${DATDIR}/assignment/%/merged_scores.csv
	$(call R, $*)

allheatplots: $(patsubst %,${FIGDIR}/assignment/eval_heatmap_%.png, ${SCOREVAR})

allerrorplots: allscatterplots allheatplots





##### Needed outputs: ##########################################################

allplots: alltruedistplots allageplots allevalplots allerrorplots

