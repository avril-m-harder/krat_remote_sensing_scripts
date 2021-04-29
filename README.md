# krat_remote_sensing_scripts
## Data download
	1. index.csv: contains information on scenes available for download from the Google Cloud Platform (downloaded on 4/28/21)
	2. scene_dl_select.R: reads in index.csv and writes a list of scenes to be downloaded (scenes_to_dl.txt) using gsutils
	3. gsutils_download: downloads Landsat scene data 