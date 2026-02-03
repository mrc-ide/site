# site 2.0.0

### Sitefile release
* malariaverse_01_2026

### Major changes

* The second full release of site files, these include latest (2025) World Malaria Report data.
* Update of population distributions using new WorldPop rasters.
* Update of rainfall data to the new CHIRPS v3.0.
* Update of Malaria Atlas Project (MAP) P. falciparum and P. Vivax prevalence. ITN usage (Africa), IRS coverage (Africa) and effective treatment coverage input rasters to the latest release (08/2025). Note: new effective treatment covearge maps are at the country-level which constitutes a reduction of the spatial resolution for this input for many country-year combinations (reason unknown).
* Update of the malaria vaccine doses delivered data from UNICEF. Note: we still do not have a clear data-source for sub-national targeted or vaccine-type for these delivered doses. Where possible, we also include specific primary dose scheduling and delivery mode by country for vaccination.
* Additional and more specific flexibility in the implementation of interventions. This includes a more modular,
granular breakdown of intervention implmentation in `site_file$interventions`. For example, specific days of the year for SMC rounds and IRS rounds, flexibility to specify both annual mass and quarterly routine bednet distributions etc. 
* Plasmodium vivax implementation now runs the vivax-specific version of malariasimulation.
* Switch to use the MAP-style logistic retention curve for bed nets. This replaces the previous exponential retention curve. 
* More flexible specification of age ranges for rates outputs.
* Documentation updated to include descriptions of every element in the site-file, and better tracking of which data inputs where used.

### Minor improvement and bug fixes

# site 1.0.5

### Sitefile release
* malariaverse_01_2025

### Major changes

* The first full release of site files, these include latest World Malaria Report 
2024 data
* Update to most recent available intervention, burden and boundary data
* Overhaul of site file creation and access to use orderly2 and packit
* Empirically-driven move from longer (5 years) assumed net retention half life
to country specific, shorter retentions
* Inclusion of age-disaggregated population estimates
* Inclusion of boundary geometry
* Inclusion of all historical, monthly rainfall data
* Inclusion of blood disorder data
* Inclusion of travel time data
* Foundation of vaccine (RTS,S and R21) input monitoring

### Minor improvement and bug fixes

* Fixed demography bug that led to misspecified population age-structure
* Fixed insecticide resistance half life bug



