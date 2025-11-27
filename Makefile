
###### INTENDED OUTPUTS ########################################################

default: localdef

localdef: all_cm_inputs

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
CONNECTDIR ?= ${INPUTDIR}/reconnect
CENSUSDIR ?= ${INPUTDIR}/census
ONSDIR ?= ${INPUTDIR}/ons
OUTDIR ?= output
FIGDIR ?= ${OUTDIR}/figures
DATDIR ?= ${OUTDIR}/data
CONTCODE ?= ${CODEDIR}/run_cont_matrs
CONTDATA ?= ${DATDIR}/cont_matrs
CONTFIG ?= ${FIGDIR}/cont_matrs
EPIDCODE ?= ${CODEDIR}/epidem
EPIDDATA ?= ${DATDIR}/epidem
EPIDFIG ?= ${FIGDIR}/epidem

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
RUNVAR ?= engreg pcd pcdage pcdethn pcdageethn pcdagehiqualnssec pcdethnhiqual pcdethnnssec pcdhousehold utlaageethn utlaethnnssec
ANALYSEVAR ?= engreg pcd pcdage pcdethn pcdageethn pcdagehiqualnssec pcdethnhiqual pcdethnnssec pcdhousehold pcdageethnnssec utlaageethnnssec

# methods for IMD assignment
ASSIGNMETHOD ?= prob det

# create all combinations of IMD assignments
$(foreach method,${ASSIGNMETHOD},$(foreach var,${ANALYSEVAR},$(call $(eval ALLSCN += ${method}_${var}))))

# methods of scoring fits
SCOREVAR ?= mse wis crps

# functions to make into assigned .rds
makeassigndet = $(addprefix ${DATDIR}/assignment/connect_det_,$(patsubst %,%.${DATAEXT},$(1))) 
makeassignprob = $(addprefix ${DATDIR}/assignment/connect_prob_,$(patsubst %,%.${DATAEXT},$(1))) 

# ages for fitting contact matrices
ALLAGES ?= 0-4 5-9 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 65-69 70-74 75+

# assignment sensitivity analyses
A_SENS_ANALYSES ?= base regional old_imd

# matrix fitting sensitivity analyses
M_SENS_ANALYSES ?= ${A_SENS_ANALYSES} large_n_age no_cap_100 

# epidemic sensitivity analyses
E_SENS_ANALYSES ?= ${M_SENS_ANALYSES} balance_sett_spec

E_SENS_ANALYSES_NO_REG ?= base large_n_age no_cap_100 balance_sett_spec

# functions to make into assigned .rds
makeagesuffix = $(addprefix ${CONTDATA}/fitted_matrs_,$(patsubst %,%.${DATAEXT},$(1))) 

clean:
	rm ${DATDIR}/assignment/mse/merged_scores.csv
	rm ${DATDIR}/assignment/wis/merged_scores.csv
	rm ${DATDIR}/assignment/crps/merged_scores.csv
	rm ${CONTDATA}/fitted_matrs.csv

##### INPUTS ###################################################################

## Script to turn raw census data into inputs is: scripts/assign_imd/make_census_inputs_2025.R

##### Polymod-weighted large group contacts ########## 

${CONNECTDIR}/connect_contacts_formatted.rds: ${ASSIGNDIR}/polymod_weights.R ${CONNECTDIR}/reconnect_part.rds ${CONNECTDIR}/reconnect_contacts.rds ${ONSDIR}/ons_2022_age_structure.xlsx
	$(call R)

##### Assign IMD for each scenario ########## 

##### Deterministic ########## 

${DATDIR}/assignment/connect_det_%.rds: ${ASSIGNDIR}/assign_imd_det.R ${CONNECTDIR}/reconnect_part.rds ${CENSUSDIR}/%.csv
	$(call R,$(firstword $(subst _, ,$*)))

allassignmentdet: $(call makeassigndet, ${RUNVAR})

##### Probabilistic ########## 

${DATDIR}/assignment/connect_prob_%.rds: ${ASSIGNDIR}/assign_imd_prob.R ${CONNECTDIR}/reconnect_part.rds ${CENSUSDIR}/%.csv
	$(call R,$(firstword $(subst _, ,$*)))

allassignmentprob: $(call makeassignprob, ${RUNVAR})

##### Make merged age/ethnicity/nssec datasets ########## 

${DATDIR}/assignment/connect_%_ageethnnssec.rds: ${ASSIGNDIR}/merge/merge_ageethnnssec.R ${DATDIR}/assignment/connect_%_ageethn.rds ${DATDIR}/assignment/connect_%_ethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_ageethnnssec.rds, ${ASSIGNMETHOD})

${DATDIR}/assignment/connect_%_pcdageethnnssec.rds: ${ASSIGNDIR}/merge/merge_pcdageethnnssec.R ${DATDIR}/assignment/connect_%_pcdageethn.rds ${DATDIR}/assignment/connect_%_pcdethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_pcdageethnnssec.rds, ${ASSIGNMETHOD})

${DATDIR}/assignment/connect_%_utlaageethnnssec.rds: ${ASSIGNDIR}/merge/merge_utlaageethnnssec.R ${DATDIR}/assignment/connect_%_utlaageethn.rds ${DATDIR}/assignment/connect_%_utlaethnnssec.rds
	$(call R, $(firstword $(subst _, ,$*)))
	
allmerged: $(patsubst %,${DATDIR}/assignment/connect_%_utlaageethnnssec.rds, ${ASSIGNMETHOD})

##### EVALUATIONS ##############################################################

##### Linear model coefficient plots ########## 

${FIGDIR}/assignment/values_lm.png: ${ASSIGNDIR}/linear_models.R ${CONNECTDIR}/reconnect_part.rds ${CONNECTDIR}/reconnect_contacts.rds
	$(call R)
	
alllmplots: ${FIGDIR}/assignment/values_lm.png

##### True distribution plots ########## 

${FIGDIR}/assignment/%/true_distrs.png: ${ASSIGNDIR}/true_distribution_plots.R ${DATDIR}/assignment/connect_%.rds
	$(call R, $*)

alltruedistplots: $(patsubst %,${FIGDIR}/assignment/%/true_distrs.png, ${ALLSCN})

##### Age-specific mean contact and proportion u18 plots ########## 

${FIGDIR}/assignment/%/mean_age_contacts.png: ${ASSIGNDIR}/age_contact_plots.R ${DATDIR}/assignment/connect_%.rds ${CONNECTDIR}/reconnect_contacts.rds
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

allassignplots: alltruedistplots allageplots allCMplots allevalplots allerrorplots alllmplots

################################################################################

##### Make contact matrices ########## 

${CONTDATA}/%/participants.rds: ${CONTCODE}/sample_participants.R ${CONNECTDIR}/reconnect_part.rds ${ONSDIR}/age_ethn_sex.xlsx ${CENSUSDIR}/pcdageethn.csv ${CENSUSDIR}/pcdethnnssec.csv
	$(call R, $*)

allsampledpart: $(patsubst %,${CONTDATA}/%/participants.rds, ${A_SENS_ANALYSES})

${CONTDATA}/%/indiv_contacts.rds: ${CONTCODE}/individual_contacts.R ${CONTDATA}/%/participants.rds ${CONNECTDIR}/reconnect_contacts.rds ${CENSUSDIR}/utlaageethn.csv ${CENSUSDIR}/utlaethnnssec.csv
	$(call R, $*)
	
allsampledcont: $(patsubst %,${CONTDATA}/%/indiv_contacts.rds, ${A_SENS_ANALYSES})

${CONTDATA}/%/cont_imd_distr.rds: ${CONTCODE}/cont_imd_distr.R ${CONTDATA}/%/indiv_contacts.rds 
	$(call R, $*)
	
allcontdistr: $(patsubst %,${CONTDATA}/%/cont_imd_distr.rds, ${A_SENS_ANALYSES})
	
${ONSDIR}/polymod_weights.rds: ${CONTCODE}/polymod_weights.R ${ONSDIR}/age_ethn_sex.xlsx
	$(call R, $*)
	
${CONTDATA}/reconnect_weights.rds: ${CONTCODE}/reconnect_weights.R ${ONSDIR}/age_ethn_sex.xlsx
	$(call R, $*)

allweights: ${ONSDIR}/polymod_weights.rds ${CONTDATA}/reconnect_weights.rds

all_cm_inputs: allsampledcont allcontdistr allbalanced allweights
	
${CONTDATA}/base/fitted_matrs_%.csv: ${CONTCODE}/fit_cont_matrs.R ${CONTDATA}/base/participants.rds ${CONTDATA}/base/indiv_contacts.rds ${CONTDATA}/base/cont_imd_distr.rds ${ONSDIR}/polymod_weights.rds ${CONTDATA}/reconnect_weights.rds
	$(call R, base $*)
	
allagematrs_base: $(patsubst %,${CONTDATA}/base/fitted_matrs_%.csv, ${ALLAGES})

${CONTDATA}/regional/fitted_matrs_%.csv: ${CONTCODE}/fit_cont_matrs.R ${CONTDATA}/regional/participants.rds ${CONTDATA}/regional/indiv_contacts.rds ${CONTDATA}/regional/cont_imd_distr.rds ${ONSDIR}/polymod_weights.rds ${CONTDATA}/reconnect_weights.rds
	$(call R, regional $*)
	
allagematrs_regional: $(patsubst %,${CONTDATA}/regional/fitted_matrs_%.csv, ${ALLAGES})

${CONTDATA}/no_cap_100/fitted_matrs_%.csv: ${CONTCODE}/fit_cont_matrs.R ${CONTDATA}/base/participants.rds ${CONTDATA}/base/indiv_contacts.rds ${CONTDATA}/base/cont_imd_distr.rds ${ONSDIR}/polymod_weights.rds ${CONTDATA}/reconnect_weights.rds
	$(call R, no_cap_100 $*)
	
allagematrs_no_cap_100: $(patsubst %,${CONTDATA}/no_cap_100/fitted_matrs_%.csv, ${ALLAGES})

${CONTDATA}/large_n_age/fitted_matrs_%.csv: ${CONTCODE}/fit_cont_matrs.R ${CONTDATA}/base/participants.rds ${CONTDATA}/base/indiv_contacts.rds ${CONTDATA}/base/cont_imd_distr.rds ${ONSDIR}/polymod_weights.rds ${CONTDATA}/reconnect_weights.rds
	$(call R, large_n_age $*)
	
allagematrs_large_n_age: $(patsubst %,${CONTDATA}/large_n_age/fitted_matrs_%.csv, ${ALLAGES})

allagematrs: allagematrs_base allagematrs_regional allagematrs_no_cap_100 allagematrs_large_n_age

# merge for each sensitivity analysis (merge regional and then split into regional files, file too big otherwise)

${CONTDATA}/base/fitted_matrs.csv: $(patsubst %,${CONTDATA}/base/fitted_matrs_%.csv, ${ALLAGES})
	cat $^> $@

${CONTDATA}/no_cap_100/fitted_matrs.csv: $(patsubst %,${CONTDATA}/no_cap_100/fitted_matrs_%.csv, ${ALLAGES})
	cat $^> $@
	
${CONTDATA}/large_n_age/fitted_matrs.csv: $(patsubst %,${CONTDATA}/large_n_age/fitted_matrs_%.csv, ${ALLAGES})
	cat $^> $@
	
${CONTDATA}/regional/fitted_matrs.csv: ${CONTCODE}/merge_and_split_regions.R $(patsubst %,${CONTDATA}/regional/fitted_matrs_%.csv, ${ALLAGES})
	$(call R)

allmergedmatrs: $(patsubst %,${CONTDATA}/%/fitted_matrs.csv, ${M_SENS_ANALYSES}) 

# balance

${CONTDATA}/%/fitted_matrs_balanced.csv: ${CONTCODE}/balance_matrs.R ${CONTDATA}/%/fitted_matrs.csv
	$(call R, $*)
	
${CONTDATA}/balance_sett_spec/fitted_matrs_balanced.csv: ${CONTCODE}/balance_matrs_sett_spec.R ${CONTDATA}/base/fitted_matrs.csv
	$(call R, $*)

allbalanced: $(patsubst %,${CONTDATA}/%/fitted_matrs_balanced.csv, ${E_SENS_ANALYSES}) 

# plot 

${CONTFIG}/%/fitted_matrs.png: ${CONTCODE}/plot_cont_matrs.R ${CONTDATA}/%/fitted_matrs_balanced.csv ${CENSUSDIR}/imd_age.csv
	$(call R, $*)
	
allmatrsplots_agg: $(patsubst %,${CONTFIG}/%/fitted_matrs.png, ${E_SENS_ANALYSES}) 
	
${CONTFIG}/%/fitted_matrs_locn.png: ${CONTCODE}/plot_cont_matrs_locn.R ${CONTDATA}/%/fitted_matrs.csv
	$(call R, $*)

allmatrsplots_locn: $(patsubst %,${CONTFIG}/%/fitted_matrs_locn.png, ${E_SENS_ANALYSES_NO_REG}) 

allmatrsplots: allmatrsplots_agg allmatrsplots_locn

################################################################################

##### Epidemic simulations ########## 

${EPIDDATA}/%/byall.rds: ${EPIDCODE}/modelrun.r ${CONTDATA}/%/fitted_matrs_balanced.csv 
	$(call R, $*)

${EPIDFIG}/%/attack_rate_bars.png: ${EPIDCODE}/plot_epidem.r ${EPIDDATA}/%/byall.rds
	$(call R, $*)
	
allepidplots: $(patsubst %,${EPIDFIG}/%/attack_rate_bars.png, ${E_SENS_ANALYSES}) 








