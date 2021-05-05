## to get list of URLs, filter index.csv downloaded from Google Earth Engine by list
## of scenes of interest (n=648 in this case: all Landsat 4-5TM scenes overlapping GPS
## coords of mounds, 31.6166667°, -109.2500000°)

# ./gsutils/google-cloud-sdk/bin/gcloud init

cat scenes_to_dl.txt | while read line
 	do
 	gsutil cp -r "${line}" ./ls_downloads/
 	done
 	
