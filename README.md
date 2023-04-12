# krat_remote_sensing_scripts

### Scripts and files associated with manuscript, *Individual fitness in a small desert mammal predicted by remotely sensed environmental measurements*. Currently available on *[Authorea](https://www.authorea.com/users/605635/articles/635046-individual-fitness-in-a-small-desert-mammal-predicted-by-remotely-sensed-environmental-measurements)* and submitted for peer-review.

### Scripts for downloading Landsat 5 TM scenes and conducting all downstream transformations and statistical analyses
* *01_landsat2_dl_aws_s3.sh*: downloads scenes of interest (listed in c2l2_scenes_full_list.txt) using AWS s3api ([USGS guide to commercial cloud downloads](https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-2032-Landsat-Commercial-Cloud-Direct-Access-Users-Guide-v2.pdf.pdf))
* *02_c2l2_ls5_scenes_avail.R*: examines and summarizes information about the C2L2 scenes available for the study site from Jan. 1 1989 - Dec. 31 2005
* *03a_landsat_processing_C2.R*: crops files to extent of study site, processes surface reflectance data (surface temperature band and Tasseled Cap transformation), and writes transformation/index output for statistical analysis
* *03b_c2l2readMeta.R*: an updated version of the RStoolbox function *readMeta()* that is compatible with Collection 2 Level 2 metadata files (*_MTL.txt)
* *04_summarize_cells_with_mounds.R*: identifies cells to be analyzed for each year and summarizes remote sensing the PRISM data for each time interval (annual, summer and winter rainy seasons), equalizing across meteorological seasons for the annual data
* *05a_env_x_numoff_individuals.R*: backwards stepwise regressions to identify predictors of individual fitness (number of offspring) for each time interval
* *05b_env_x_numsurv_individuals.R*: backwards stepwise regressions to identify predictors of individual fitness (number of offspring surviving to age 1) for each time interval
* *06_poplevel_analyses.R*: tests relationships between remote sensing and PRISM data and population-level measures (*i.e.*, number of active mounds, absolute and proportional changes in population size, average number of offspring per female, and average number of offspring surviving to age 1 per female). includes permutation tests for final models

### Additional scripts
* *98_fig_01_map.R*: generates maps presented in Figure 1
* *98_landcover_mapping.R*: generates landcover maps presented in SI Figure ##

### Files
* *99_c2l2_scenes_full_list.txt*: list of scenes to be downloaded using the 01_landsat2_dl_aws_s3.sh script
