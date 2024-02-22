Code ReadMe
================
Last compiled on 22 February, 2024

<!-- README.md is generated from README.RMD; knit at end -->

This folder contains all code to replicate our analysis. All code was
run using R version 4.2.1 and JAGS 4.3.1.

### Files (listed in order that they should be run):

- `functions.R` contains the following functions:
  - `generate.code()`
    - This function generates JAGS code using the **mgcv** package to
      account for the splines in the model.
- `observations.R`
  - **Purpose:** To summarize the number of observations and detection
    rate in each training dataset
  - **Inputs:** `data/train1.csv`, `data/train2.csv`, `data/train3.csv`,
    `data/train4.csv`, `data/covdat.csv`, `data/covdat2.csv`,
    `data/hdat.csv`
  - **Outputs:** `outputs/observations.jpg`,
    `outputs/detection-rate.jpg`
- `spp-models/` contains individual scripts for each species x model
  combination labeled as “species”“model number”.R (e.g., cawa1.R).
  Species are abbreviated as outlined below.
  - **Inputs:** `data/train1.csv`, `data/train2.csv`, `data/train3.csv`,
    `data/train4.csv`, `data/covdat.csv`, `data/covdat2.csv`,
    `data/hdat.csv`
  - **Outputs:** `results/out/cawam1.RData` and the like (not loaded to
    GitHub due to file size)
- `process-output.R`
  - **Purpose:** To import RData files generated from JAGS model and
    save performance metrics (brier score, AUC, deviance)
  - **Inputs:** `results/out/cawam1.RData` and the like (not loaded to
    GitHub due to file size)
  - **Outputs:** `results/performancemetrics.rds`
- `results.R`
  - **Purpose:** To post-process, assemble output, and generate a figure
  - **Inputs:** `results/performancemetrics.rds`
  - **Outputs:** `outputs/metrics.jpg`, `outputs/metricstab1.csv`,
    `outputs/metricstab2.csv`
- `run-jams.R`
  - **Purpose:** To generate a reduced GAM object suitable for plotting
    and prediction (sim2jam) for each species-model combination
  - **Inputs:** `functions.R`, `results/out/*.Rdata`
  - **Outputs:** `results/jams/*jam.RData` (not loaded to GitHub due to
    file size)
- `process-jams.R`
  - **Purpose:** To extract covariate effects from fitted GAMs
  - **Inputs:** `results/jams/*.Rdata`
  - **Outputs:** `results/gams.rds`
- `plot-parameter-estimates.R`
  - **Purpose:** To generate figures of nonlinear covariate effects
  - **Inputs:** `results/gams.rds`
  - **Outputs:** `outputs/parameter-partial.jpg`,
    `outputs/parameter-full.jpg`
- `make-maps.R`
  - **Purpose:** To create maps of predicted occurrence for four species
  - **Inputs:**
    - `data/covariates.Rdata` (code could be run without this; not
      loaded to GitHub due to file size)
    - `data/grid_pa_1km.RData` summarized covariate data at the 1km grid
      scale
    - `results/jams/cawam1jam.RData`
    - `results/jams/cerwm1jam.RData`
    - `results/jams/gwwam1jam.RData`
    - `results/jams/wothm1jam.RData`
  - **Outputs:** `outputs/fig3.jpg`

### Species Abbreviations

| Species Name | Abbreviation          |
|--------------|-----------------------|
| CAWA         | Canadian Warbler      |
| CERW         | Cerulean Warbler      |
| GWWA         | Golden-winged Warbler |
| WOTH         | Wood Thrush           |

### Model list

| Model Name (code) | Model Name (text) | Description                                                 |
|-------------------|-------------------|-------------------------------------------------------------|
| Model 1           | R1                | Full eBird, BBA, BBS                                        |
| Model 2           | F1                | Unfiltered eBird data                                       |
| Model 3           | F2                | Filtered eBird data but no spatial balancing                |
| Model 4           | F3                | Unfiltered, no spatial balancing                            |
| Model 5           | F5                | No eBird effort model                                       |
| Model 6           | F6                | Unfiltered, no spatial balancing, no effort model for eBird |
| Model 7           | I1                | False-positive model for eBird                              |
| Model 8           | I2                | eBird treated as a covariate                                |
| Model 10          | I3                | eBird removed, BBA & BBS only                               |
| Model 11          | F4                | Stationary eBird, no travelling                             |
| Model 12          | R2                | Full model (like R1) but validated on current data          |
| Model 13          | O3                | Older BBA data removed, new eBird and BBS only              |
| Model 14          | O2                | Older BBA data treated as covariate                         |
| Model 15          | O1                | False positive for BBA, validate on new data                |
| Model 16          | I4                | Different intercepts for eBird and BBS                      |
