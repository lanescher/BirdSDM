Species Futures ReadMe
================
Last compiled on 20 February, 2024

<!-- README.md is generated from README.RMD; knit at end -->

This repository contains code associated with **Integrating unreliable
data into species distribution models to improve occupancy predictions
of avian species of concern** written by Fiona Lunt, C. Lane Scher,
Riley O. Mummah, and David A.W. Miller.

The code and data are maintained by [Riley
Mummah](mailto:%20rmummah@usgs.gov).

All code was run using R version 4.2.1 and JAGS XXX

### Repository Structure

- `data/` contains all data used for fitting the models
- `code/` contains all R functions and scripts to fit models. All code
  to run models are written separately by model and species in subfolder
  `spp-models/`.
- `models/` contains all JAGS models
- `results/` contains summarized model output
- `outputs/` contains the compiled figures and tables

Each folder contains a FileDescription that outlines all files in the
folder.

This readme currently includes all parameter and function information.

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
