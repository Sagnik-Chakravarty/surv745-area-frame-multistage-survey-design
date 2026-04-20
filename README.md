Multistage Area Frame Sampling Design – Detroit Transportation Study

This repository contains the implementation of a multistage probability sampling design developed for estimating transportation insecurity among adults in Detroit, Michigan. The project was completed as part of SURV 745 (Sampling Techniques) at the University of Maryland.

--------------------------------------------------
PROJECT OVERVIEW
--------------------------------------------------
The objective of this study is to produce reliable estimates of transportation-related outcomes among adults aged 18 and older in Detroit. Key outcomes include vehicle access, public transportation use, missed activities due to transportation barriers, and reliance on transportation assistance.

Estimates are produced separately for four age domains:
- 18–34
- 35–49
- 50–64
- 65+

The design targets 1,000 completed responses (250 per domain), accounting for differential response rates across age groups.

--------------------------------------------------
SAMPLING DESIGN
--------------------------------------------------

Stage 1: Selection of Primary Sampling Units (PSUs)
Census tracts are selected using probability proportional to size (PPS) systematic sampling. The measure of size is adjusted for response rates across age domains. A total of 40 PSUs are selected.

Stage 2: Selection of Secondary Sampling Units (SSUs)
Within each selected tract, one block group is selected using PPS sampling. Tracts with only one block group are included with certainty.

Stage 3: Selection of Persons
Individuals are sampled within selected block groups, stratified by age group. Allocation is proportional to population size while ensuring domain-level targets are met.

Household Sampling
Households are selected systematically within each sampled block group. Allocation is adjusted using inverse selection probabilities to maintain balance across stages.

--------------------------------------------------
MEASURE OF SIZE (MOS)
--------------------------------------------------
The response-rate-adjusted measure of size is defined as a weighted combination of age-specific population counts, adjusted by expected response rates. This ensures higher sampling intensity in areas with lower response propensity.

--------------------------------------------------
OUTPUTS
--------------------------------------------------

Frame Files:
- Tract-level (PSU) frame
- Block group-level (SSU) frame
Includes population counts, age distributions, MOS variables, and feasibility indicators.

Sample File:
Contains selected units, inclusion probabilities, household allocations, within-domain sampling rates, and base weights.

Report:
A complete methodological report detailing the sampling design, allocation strategy, weighting, and variance estimation.

--------------------------------------------------
REPRODUCIBILITY
--------------------------------------------------
All sampling steps are implemented in a shared script:
- sample_selection_core.R

Both Quarto documents source this script to ensure consistency and reproducibility.

To reproduce results:
1. Run the core script
2. Render the Quarto files

--------------------------------------------------
KEY FEATURES
--------------------------------------------------
- Multistage PPS sampling design
- Response-rate-adjusted allocation
- Approximate self-weighting within domains
- Explicit handling of edge cases (zero population, small areas, certainty selections)

--------------------------------------------------
AUTHORS
--------------------------------------------------
Sagnik Chakravarty  
Gloria Guzman  

University of Maryland, College Park

--------------------------------------------------
COURSE CONTEXT
--------------------------------------------------
SURV 745: Sampling Techniques  
Joint Program in Survey Methodology (JPSM)

--------------------------------------------------
LICENSE
--------------------------------------------------
This repository is intended for academic use.
