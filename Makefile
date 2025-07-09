
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
SETUP ?= setup
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

${OUTDIR} ${DATDIR} ${FIGDIR}:
	mkdir -p $@

RENV = .Rprofile

# build renv/library & other renv infrastructure
${RENV}: install.R 
	 Rscript --vanilla $^

# scenarios for IMD assignment
RUNVAR ?= engreg pcd1 pcd1age pcd1ageethn utlaageethn ageethn pcd1agehiqualnssec pcd1household pcd1ethntenure pcd1ethnhiqual pcd1agenssec pcd1ethn pcd1agehiqual pcd1hhsize pcd1hhtenure pcd1ethnnssec utlaethnnssec ethnnssec
ANALYSEVAR ?= pcd1 pcd1age pcd1ageethn pcd1agehiqualnssec pcd1household pcd1ethnhiqual pcd1agenssec pcd1ethn pcd1agehiqual pcd1ethnnssec pcd1ageethnnssec utlaageethnnssec

# methods for IMD assignment
ASSIGNMETHOD ?= prob det

# create all combinations of IMD assignments
$(foreach method,${ASSIGNMETHOD},$(foreach var,${ANALYSEVAR},$(call $(eval ALLSCN += ${method}_${var}))))

# methods of scoring fits
SCOREVAR ?= mse wis crps

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

## Script to turn raw census data into inputs is: scripts/assign_imd/make_census_inputs.R

##### Polymod-weighted large group contacts ########## 

${CONNECTDIR}/connect_contacts_formatted.rds: ${ASSIGNDIR}/polymod_weights.R ${CONNECTDIR}/connect_part.rds ${CONNECTDIR}/connect_contacts.rds ${ONSDIR}/ons_2022_age_structure.xlsx
	$(call R)

##### Assign IMD for each scenario ########## 

##### Deterministic ########## 

${DATDIR}/assignment/connect_det_%.rds: ${ASSIGNDIR}/assign_imd_det.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/%.csv
	$(call R,$(firstword $(subst _, ,$*)))

allassignmentdet: $(call makeassigndet, ${RUNVAR})

##### Probabilistic - running one by one to avoid reruns ########## 

#${DATDIR}/assignment/connect_prob_engreg.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/engreg.csv
#	$(call R, engreg)

#${DATDIR}/assignment/connect_prob_pcd1.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1.csv
#	$(call R, pcd1)

#${DATDIR}/assignment/connect_prob_pcd1age.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1age.csv
#	$(call R, pcd1age)

#${DATDIR}/assignment/connect_prob_ageethn.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/ageethn.csv
#	$(call R, ageethn)

#${DATDIR}/assignment/connect_prob_pcd1ageethn.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1ageethn.csv
#	$(call R, pcd1ageethn)

#${DATDIR}/assignment/connect_prob_pcd1agehiqualnssec.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1agehiqualnssec.csv
#	$(call R, pcd1agehiqualnssec)

#${DATDIR}/assignment/connect_prob_pcd1household.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1household.csv
#	$(call R, pcd1household)

#${DATDIR}/assignment/connect_prob_pcd1ethntenure.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1ethntenure.csv
#	$(call R, pcd1ethntenure)

#${DATDIR}/assignment/connect_prob_pcd1ethnhiqual.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1ethnhiqual.csv
#	$(call R, pcd1ethnhiqual)

#${DATDIR}/assignment/connect_prob_pcd1agenssec.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1agenssec.csv
#	$(call R, pcd1agenssec)

#${DATDIR}/assignment/connect_prob_pcd1ethn.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1ethn.csv
#	$(call R, pcd1ethn)

#${DATDIR}/assignment/connect_prob_pcd1agehiqual.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1agehiqual.csv
#	$(call R, pcd1agehiqual)

#${DATDIR}/assignment/connect_prob_pcd1hhsize.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1hhsize.csv
#	$(call R, pcd1hhsize)

#${DATDIR}/assignment/connect_prob_pcd1hhtenure.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1hhtenure.csv
#	$(call R, pcd1hhtenure)

#${DATDIR}/assignment/connect_prob_ethnnssec.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/ethnnssec.csv
#	$(call R, ethnnssec)

#${DATDIR}/assignment/connect_prob_pcd1ethnnssec.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/pcd1ethnnssec.csv
#	$(call R, pcd1ethnnssec)

#${DATDIR}/assignment/connect_prob_utlaageethn.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/utlaageethn.csv
#	$(call R, utlaageethn)

#${DATDIR}/assignment/connect_prob_utlaethnnssec.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/connect_part.rds ${CENSUSDIR}/utlaethnnssec.csv
#	$(call R, utlaethnnssec)

##### Make merged age/ethnicity/nssec datasets ########## 

${DATDIR}/assignment/connect_%_ageethnnssec.rds: ${ASSIGNDIR}/merge/merge_ageethnnssec.R ${DATDIR}/assignment/connect_%_ageethn.rds ${DATDIR}/assignment/connect_%_ethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_ageethnnssec.rds, ${ASSIGNMETHOD})

${DATDIR}/assignment/connect_%_pcd1ageethnnssec.rds: ${ASSIGNDIR}/merge/merge_pcd1ageethnnssec.R ${DATDIR}/assignment/connect_%_pcd1ageethn.rds ${DATDIR}/assignment/connect_%_pcd1ethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_pcd1ageethnnssec.rds, ${ASSIGNMETHOD})

${DATDIR}/assignment/connect_%_utlaageethnnssec.rds: ${ASSIGNDIR}/merge/merge_utlaageethnnssec.R ${DATDIR}/assignment/connect_%_utlaageethn.rds ${DATDIR}/assignment/connect_%_utlaethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_utlaageethnnssec.rds, ${ASSIGNMETHOD})

##### EVALUATIONS ##############################################################

##### Linear model coefficient plots ########## 

${FIGDIR}/assignment/values_lm.png: ${ASSIGNDIR}/linear_models.R ${CONNECTDIR}/connect_part.rds ${CONNECTDIR}/connect_contacts.rds
	$(call R)
	
alllmplots: ${FIGDIR}/assignment/values_lm.png

##### True distribution plots ########## 

${FIGDIR}/assignment/%/true_distrs.png: ${ASSIGNDIR}/true_distribution_plots.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

alltruedistplots: $(patsubst %,${FIGDIR}/assignment/%/true_distrs.png, ${ALLSCN})

##### Age-specific mean contact and proportion u18 plots ########## 

${FIGDIR}/assignment/%/mean_age_contacts.png: ${ASSIGNDIR}/age_contact_plots.R ${DATDIR}/assignment/connect_%.rds ${CONNECTDIR}/connect_contacts.rds
	$(call R, $*)

allageplots: $(patsubst %,${FIGDIR}/assignment/%/mean_age_contacts.png, ${ALLSCN})

##### Crude contact matrices ########## 

${FIGDIR}/assignment/%/part_imd_contact_matrs.png: ${ASSIGNDIR}/part_imd_contact_matrs_plots.R ${DATDIR}/assignment/connect_%.rds ${CONNECTDIR}/connect_contacts_formatted.rds
	$(call R, $*)

allCMplots: $(patsubst %,${FIGDIR}/assignment/%/part_imd_contact_matrs.png, ${ALLSCN})

##### Make error scores ########## 

# MSE 

${DATDIR}/assignment/mse/%_scores.csv: ${ASSIGNDIR}/make_MSE_error_scores.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allmse: $(patsubst %,${DATDIR}/assignment/mse/%_scores.csv, ${ALLSCN})

# WIS 

${DATDIR}/assignment/wis/%_scores.csv: ${ASSIGNDIR}/make_WIS_error_scores.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allwis: $(patsubst %,${DATDIR}/assignment/wis/%_scores.csv, ${ALLSCN})

# CRPS

${DATDIR}/assignment/crps/%_scores.csv: ${ASSIGNDIR}/make_CRPS_error_scores.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

allcrps: $(patsubst %,${DATDIR}/assignment/crps/%_scores.csv, ${ALLSCN})

# Merge scores

clean:
	rm ${DATDIR}/assignment/mse/merged_scores.csv
	rm ${DATDIR}/assignment/wis/merged_scores.csv
	rm ${DATDIR}/assignment/crps/merged_scores.csv

${DATDIR}/assignment/mse/merged_scores.csv: $(patsubst %,${DATDIR}/assignment/mse/%_scores.csv,${ALLSCN})
	cat $^> $@

allmsemerged: ${DATDIR}/assignment/mse/merged_scores.csv
	
${DATDIR}/assignment/wis/merged_scores.csv: $(patsubst %,${DATDIR}/assignment/wis/%_scores.csv,${ALLSCN})
	cat $^> $@

allwismerged: ${DATDIR}/assignment/wis/merged_scores.csv

${DATDIR}/assignment/crps/merged_scores.csv: $(patsubst %,${DATDIR}/assignment/crps/%_scores.csv,${ALLSCN})
	cat $^> $@

allwismerged: ${DATDIR}/assignment/crps/merged_scores.csv

##### Main evaluation plots ########## 

${FIGDIR}/assignment/%/evaluation.png: ${ASSIGNDIR}/evaluation_plots.R ${DATDIR}/assignment/connect_%.rds ${DATDIR}/assignment/wis/%_scores.csv
	$(call R, $*)

allevalplots: $(patsubst %,${FIGDIR}/assignment/%/evaluation.png, ${ALLSCN})

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

allplots: alltruedistplots allageplots allCMplots allevalplots allerrorplots alllmplots

