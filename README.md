# krat_remote_sensing_scripts

### Scripts and files associated with manuscript, *Individual fitness in a small desert mammal predicted by remotely sensed environmental measurements*. Currently available on [bioRxiv](https://www.biorxiv.org/[link]) and submitted for peer-review.

### Scripts for downloading and analyzing Landsat 5 TM scenes 
* *01_landsat2_dl_aws_s3.sh*: downloads scenes of interest (listed in c2l2_scenes_full_list.txt) using AWS s3api ([USGS guide to commercial cloud downloads](https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-2032-Landsat-Commercial-Cloud-Direct-Access-Users-Guide-v2.pdf.pdf))
* *02_c2l2_ls5_scenes_avail.R*: examines and summarizes information about the C2L2 scenes available for the study site from Jan. 1 1989 - Dec. 31 2005
* *03a_landsat_processing_C2.R*: crops files to extent of study site, processes surface reflectance data (surface temperature band and Tasseled Cap transformation), and writes transformation/index output for statistical analysis
* *03b_c2l2readMeta.R*: an updated version of the RStoolbox function *readMeta()* that is compatible with Collection 2 Level 2 metadata files (*_MTL.txt)
* *05a_env_x_numoff_individuals.R*: 
* *05b_env_x_numsurv_individuals.R*: 
* *06_poplevel_analyses.R*:

### Additional scripts


### Files
* *98_c2l2_scenes_full_list.txt*: list of scenes to be downloaded using the 01_landsat2_dl_aws_s3.sh script