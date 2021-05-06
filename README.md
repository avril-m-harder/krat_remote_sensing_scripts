# krat_remote_sensing_scripts

### Scripts for downloading and analyzing Landsat 5 TM scenes and some mapping
- range_map.R: maps *D. spectabilis* range and extent of Landsat5 scenes covering the study site
- landsat2_dl_aws_s3.sh: downloads scenes of interest (listed in c2l2_scenes_full_list.txt) using AWS s3api ([USGS guide to commercial cloud downloads](https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-2032-Landsat-Commercial-Cloud-Direct-Access-Users-Guide-v2.pdf.pdf))
- c2l2_ls5_scenes_avail.R: examines and summarizes information about the C2L2 scenes available for the study site from Jan. 1 1989 - Dec. 31 2005