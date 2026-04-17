# Local-level deprivation-specific social contact patterns in England

Creating social contact matrices stratified by age group and Index of Multiple Deprivation (IMD) quintiles in England, using data from the [Reconnect Social Contact Survey]([https://www.lshtm.ac.uk/research/centres/centre-mathematical-modelling-infectious-diseases/connect](https://zenodo.org/records/17339866)). These matrices can be used in epidemic models estimate differential disease burden across socioeconomic groups, and investigate the impact of interventions on these inequalities. Matrix outputs are available from `matrices/`.

---

## Overview

The workflow covers:

1. **IMD assignment:** assigning Reconnect participants IMD quintiles based on geographic and demographic information provided, by linking to data from the 2021 Census
2. **Contact matrix fitting:** constructing age- and IMD-stratified contact matrices from individual-level contact data. This is set up to run on an HPC due to computational intensity.
3. **Balancing:** balancing the "total" contact matrices to ensure reciprocity of contacts.
4. **Epidemic simulation:** running an SEIR model using the fitted matrices, and underlying demography, to estimate infection attack rates by age group and IMD quintile.

---

## Data Sources

| Dataset | Description |
|---|---|
| **Reconnect Survey** (`data/reconnect/`) | Individual-level contact survey data (participants and contacts) |
| **Census data** (`data/census/`) | LSOA-level demographic data used for IMD assignment |
| **ONS data** (`data/ons/`) | Age/ethnicity/sex population structure for survey weighting, LSOA to IMD lookups, etc. |

> **Note:** Raw data files are not included in this repository. 

---

## Repository Structure

```
imd_matrices/
├── scripts/
|   ├── setup/               # Setting key inputs, i.e. colour schemes, variable names
│   ├── assign_imd/          # IMD assignment, evaluation of models
│   ├── run_cont_matrs/      # Sampling participant and contact IMD, contact matrix fitting, balancing, and plotting
│   └── epidem/              # Epidemic simulation and plotting
├── renv/                    # R package environment (managed by renv)
├── Makefile                 # Reproducible pipeline (except for fitting contact matrices on HPC)
├── install.R                # renv setup script
└── renv.lock                # Locked package versions
```

Output is written to `output/`, with subdirectories for figures (`output/figures/`) and data (`output/data/`).

---

## Sensitivity Analyses

The following sensitivity analyses are implemented throughout the pipeline:

| Label | Description |
|---|---|
| `base` | Baseline analysis |
| `regional` | Regional contact matrices |
| `old_imd` | 2019 IMD, instead of than 2025 |
| `nhs_ages` | Secondary age bands |
| `large_n_age` | Sample large group contacts' age from Reconnect |
| `no_cap_100` | No cap on large group contact numbers |
| `balance_sett_spec` | Setting-specific matrix balancing |
