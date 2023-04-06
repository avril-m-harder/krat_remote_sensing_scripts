while read -a line
do
	mkdir ${line[3]}
	cd ${line[3]}
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_MTL.txt --request-payer requester ${line[3]}_MTL.txt
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B1.TIF --request-payer requester ${line[3]}_SR_B1.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B2.TIF --request-payer requester ${line[3]}_SR_B2.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B3.TIF --request-payer requester ${line[3]}_SR_B3.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B4.TIF --request-payer requester ${line[3]}_SR_B4.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B5.TIF --request-payer requester ${line[3]}_SR_B5.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_B7.TIF --request-payer requester ${line[3]}_SR_B7.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_SR_CLOUD_QA.TIF --request-payer requester ${line[3]}_SR_CLOUD_QA.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_ST_B6.TIF --request-payer requester ${line[3]}_ST_B6.TIF
	aws s3api get-object --bucket usgs-landsat --key collection02/level-2/standard/tm/${line[0]}/${line[1]}/${line[2]}/${line[3]}/${line[3]}_ST_QA.TIF --request-payer requester ${line[3]}_ST_QA.TIF
	cd ../
done < /Volumes/avril_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/c2l2_scenes_working_list.txt