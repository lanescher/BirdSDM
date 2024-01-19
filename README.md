# BirdSDM
Integrated species distribution model

Model list

| Model Name (code) | Model Name (text) | Description |
|-------------------|-------------------|-------------|
| Model 1           | R1                | Full eBird, BBA, BBS joint-likelihood |
| Model 2           | F1                | unfiltered eBird data, joint-likelihood |
| Model 3           | F2                | filtered but no spatial balancing eBird data, joint-likelihood |
| Model 4           | F3                | unfiltered, no spatial balancing, joint-likelihood |
| Model 5           | E1                | remove eBird effort model |
| Model 6           | E2                | Unfiltered, no spatial balancing, no effort for eBird |
| Model 7           | I1                | false-positive model for eBird, joint-likelihood |
| Model 8           | I2                | covariate model using eBird |
| Model 10          | I3                | no eBird, BBA & BBS only |
| Model 11          | F4                | stationary eBird in joint-likelihood, no travelling |
| Model 12          | R2                | full model (like m1) but validated on current data |
| Model 13          | O3                | remove BBA and older data, new eBird and BBS only |
| Model 14          | O2                | treat BBA and old data as a covariate |
| Model 15          | O1                | false positive for BBA, validate on new data |
| Model 16          | I4                | different intercept model |



