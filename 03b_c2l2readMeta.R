c2l2readMeta <- function (file, raw = FALSE) 
{
    if (!file.exists(file)) 
        stop("Metadata file does not exist. Looking for: ", file, 
            call. = FALSE)
    if (!grepl("MTL", file) & !grepl("xml", file)) 
        warning("The Landsat metadata file you have specified looks unusual. Typically the filename contains the string 'MTL' or 'xml'. Are you sure you specified the right file? \n I'll try to read it but check the results!")
    format <- if (grepl("xml", file)) 
        "XML"
    else "MTL"
    if (format == "MTL") {
        meta <- read.delim(file, sep = "=", header = FALSE, stringsAsFactors = FALSE, 
            strip.white = TRUE, skip = 1, skipNul = TRUE)
        meta <- meta[-(nrow(meta) - c(1, 0)), ]
        l <- meta[grep("GROUP", meta[, 1]), ]
        meta <- lapply(unique(l[, 2]), FUN = function(x) {
            w <- which(meta[, 2] == x)
            m <- meta[(w[1] + 1):(w[2] - 1), ]
            rownames(m) <- m[, 1]
            m <- m[, 2, drop = FALSE]
            colnames(m) <- "VALUE"
            return(m)
        })
        names(meta) <- unique(l[, 2])
        num <- grep("MIN_MAX|RESCAL|THERMAL", names(meta))
        meta[num] <- lapply(meta[num], function(x) {
            x[, 1] <- as.numeric(x[, 1])
            x
        })
        if (raw) 
            return(meta)
        legacy <- "PROCESSING_SOFTWARE" %in% rownames(meta$LEVEL1_PROCESSING_RECORD)
        if (legacy) 
            message("This scene was processed before August 29, 2012. Using MTL legacy format. Some minor infos such as SCENE_ID will be missing")
        sat <- paste0("LANDSAT", .getNumeric(meta$IMAGE_ATTRIBUTES["SPACECRAFT_ID", 
            ]))
        sen <- meta$IMAGE_ATTRIBUTES["SENSOR_ID", ]
        scene <- meta$LEVEL1_PROCESSING_RECORD["LANDSAT_SCENE_ID", 
            ]
        date <- as.POSIXct(if (!legacy) 
            meta$IMAGE_ATTRIBUTES["DATE_ACQUIRED", ]
        else meta$IMAGE_ATTRIBUTES["DATE_ACQUIRED", ], tz = "GMT")
        date <- strptime(paste0(date, meta$IMAGE_ATTRIBUTES["SCENE_CENTER_TIME", 
            ]), "%Y-%m-%d %H:%M:%S", tz = "GMT")
        pdate <- as.POSIXct(if (!legacy) 
            meta$LEVEL2_PROCESSING_RECORD["DATE_PRODUCT_GENERATED", ]
        else meta$LEVEL2_PROCESSING_RECORD["DATE_PRODUCT_GENERATED", 
            ], tz = "GMT")
        path <- as.numeric(meta$IMAGE_ATTRIBUTES["WRS_PATH", 
            ])
        row <- if (!legacy) 
            as.numeric(meta$IMAGE_ATTRIBUTES["WRS_ROW", ])
        else as.numeric(meta$LEVEL1_PROCESSING_RECORD["STARTING_ROW", 
            ])
        pars <- meta$LEVEL1_PROJECTION_PARAMETERS[c("MAP_PROJECTION", 
            "UTM_ZONE", "DATUM"), ]
        pars[1] <- tolower(pars[1])
        proj <- CRS(paste0(c("+proj=", "+zone=", "+units=m +datum="), 
            pars, collapse = " "))
        files <- row.names(meta[["LEVEL1_PROCESSING_RECORD"]])[grep("^.*BAND", 
            row.names(meta$LEVEL1_PROCESSING_RECORD))]
        files <- files[!grepl("PRESENT", files)]
        files <- meta[["LEVEL1_PROCESSING_RECORD"]][files, ]
        if (grepl(paste0("^", scene, ".*"), files[1])) {
            bands <- gsub(paste0(scene, "_|.TIF"), "", files)
        }
        else {
            bands <- sapply(strsplit(files, "_(T[12]|RT)_|.TIF"), 
                "[[", 2)
        }
        bands <- paste0(bands, "_dn")
        bands <- gsub("BQA", "QA", bands)
        if (trailingZeros <- length(grep("0.TIF", bands)) > 1) 
            stop("Trailing zeros")
        quant <- rep("dn", length(bands))
        cat <- rep("image", length(bands))
        cat[grep("QA", bands)] <- "qa"
        cat[grep("B8", bands)] <- "pan"
        spatRes <- rep(meta$LEVEL1_PROJECTION_PARAMETERS["GRID_CELL_SIZE_REFLECTIVE", 
            ], length(bands))
        if (sen != "MSS") {
            spatRes[grep("B8", bands)] <- meta$LEVEL1_PROJECTION_PARAMETERS["GRID_CELL_SIZE_PANCHROMATIC", 
                ]
            spatRes[grep("B6|B10|B11", bands)] <- meta$LEVEL1_PROJECTION_PARAMETERS["GRID_CELL_SIZE_THERMAL", 
                ]
        }
        spatRes <- as.numeric(spatRes)
        na <- NA
        az <- if (!legacy) 
            as.numeric(meta$IMAGE_ATTRIBUTES["SUN_AZIMUTH", ])
        else as.numeric(meta$PRODUCT_PARAMETERS["SUN_AZIMUTH", 
            ])
        selv <- if (!legacy) 
            as.numeric(meta$IMAGE_ATTRIBUTES["SUN_ELEVATION", 
                ])
        else as.numeric(meta$PRODUCT_PARAMETERS["SUN_ELEVATION", 
            ])
        esd <- meta$IMAGE_ATTRIBUTES["EARTH_SUN_DISTANCE", ]
        if (is.null(esd) || is.na(esd)) 
            esd <- .ESdist(date)
        esd <- as.numeric(esd)
        vsat <- NA
        scal <- 1
        dtyp <- NA
        if (!legacy) {
            r <- meta$LEVEL1_RADIOMETRIC_RESCALING
            rnr <- rownames(r)
            calrad <- data.frame(offset = r[grep("RADIANCE_ADD*", 
                rownames(r)), ], gain = r[grep("RADIANCE_MULT*", 
                rownames(r)), ])
            calref <- data.frame(offset = r[grep("REFLECTANCE_ADD*", 
                rownames(r)), ], gain = r[grep("REFLECTANCE_MULT*", 
                rownames(r)), ])
            rownames(calrad) <- paste0(gsub("^.*BAND_", "B", 
                rnr[grep("RADIANCE_MULT", rnr)]), "_dn")
            if (nrow(calref) != 0) 
                rownames(calref) <- paste0(gsub("^.*BAND_", "B", 
                  rnr[grep("REFLECTANCE_MULT", rnr)]), "_dn")
            else calref <- NA
        }
        else {
            r <- meta$LEVEL1_MIN_MAX_RADIANCE
            rp <- meta$LEVEL1_MIN_MAX_PIXEL_VALUE
            rnr <- rownames(r)
            e2nd <- seq(1, nrow(r), 2)
            L <- diff(r[, 1])[e2nd]
            Q <- diff(rp[, 1])[e2nd]
            radg <- L/Q
            rado <- r[seq(2, nrow(r), 2), 1] - radg
            calrad <- data.frame(offset = rado, gain = radg)
            calref <- NA
            rownames(calrad) <- paste0(gsub("^.*BAND_", "B", 
                rnr[grep("MAX", rnr)]), "_dn")
        }
        if (sat == "LANDSAT8") {
            r <- meta$TIRS_THERMAL_CONSTANTS
            calbt <- data.frame(K1 = r[grep("K1", rownames(r)), 
                ], K2 = r[grep("K2", rownames(r)), ])
            rownames(calbt) <- c("B10_dn", "B11_dn")
        }
        else {
            if (sen != "MSS") {
                TAB7 <- list(LANDSAT4 = data.frame(K1 = 671.62, 
                  K2 = 1284.3), LANDSAT5 = data.frame(K1 = 607.76, 
                  K2 = 1260.56), LANDSAT7 = data.frame(K1 = 666.09, 
                  K2 = 1282.71))
                calbt <- TAB7[[sat]]
                tbds <- bands[grep("B6", bands)]
                if (length(tbds) > 1) 
                  calbt <- rbind(calbt, calbt)
                rownames(calbt) <- tbds
            }
            else {
                calbt <- NULL
                tbds <- NULL
            }
        }
    }
    else {
        meta <- xmlToList(xmlParse(file))
        names(meta$bands) <- gsub(" ", "_", unlist(sapply(meta$bands, 
            "[", "long_name")))
        if (raw) 
            return(meta)
        luv <- c(dn = "dn", toa_rad = "tra", toa_refl = "tre", 
            toa_bt = "bt", sr_refl = "sre", spectral_indices = "idx", 
            cfmask = "tre")
        atts <- sapply(meta$bands, "[", ".attrs")
        sat <- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))
        sen <- meta$global_metadata$instrument
        scene <- gsub("_MTL.txt", "", meta$global_metadata$lpgs_metadata_file)
        date <- strptime(paste(meta$global_metadata$acquisition_date, 
            meta$global_metadata$scene_center_time), format = "%Y-%m-%d %H:%M:%OS", 
            tz = "GMT")
        pdate <- as.POSIXct(meta$bands[[1]]$production_date, 
            tz = "GMT")
        path <- as.numeric(meta$global_metadata$wrs["path"])
        row <- as.numeric(meta$global_metadata$wrs["row"])
        az <- as.numeric(meta$global_metadata$solar_angles["azimuth"])
        selv <- 90 - as.numeric(meta$global_metadata$solar_angles["zenith"])
        proj <- CRS(paste0("+proj=utm +zone=", meta$global_metadata$projection_information$utm_proj_params, 
            " +datum=WGS84 +units=m"))
        esd <- .ESdist(date)
        files <- sapply(meta$bands, "[[", "file_name")
        quant <- luv[sapply(atts, "[", "product")]
        cat <- sapply(atts, "[", "category")
        cat[grep("opacity", names(cat))] <- "qa"
        bands <- gsub(paste0(scene, "_|.tif"), "", files)
        bs <- grepl("_surface_reflectance", names(files))
        bands[bs] <- paste0("B", .getNumeric(bands[bs]), "_", 
            quant[bs])
        bands[cat == "qa"] <- paste0("QA_", gsub("sr_|_qa", "", 
            bands[cat == "qa"]))
        bands[cat == "index"] <- gsub("SR_", "", toupper(bands[cat == 
            "index"]))
        spatRes <- vapply(meta$bands, function(x) x$pixel_size["x"], 
            character(1))
        na <- as.numeric(sapply(atts, "[", "fill_value"))
        vsat <- as.numeric(sapply(atts, "[", "saturate_value"))
        scal <- as.numeric(sapply(atts, "[", "scale_factor"))
        dataTypes <- c(INT16 = "INT4S", UINT8 = "INT1U")
        dtyp <- dataTypes[as.character(sapply(atts, "[", "data_type"))]
        calrad <- calref <- calbt <- NA
    }
    radRes <- if (sat == "LANDSAT8") 
        16
    else if (sen == "MSS") 
        6
    else 8
    ImageMetaData(file = file, format = format, sat = sat, sen = sen, 
        scene = scene, date = date, pdate = pdate, path = path, 
        radRes = radRes, spatRes = spatRes, row = row, az = az, 
        selv = selv, esd = esd, files = files, bands = bands, 
        quant = quant, cat = cat, na = na, vsat = vsat, scal = scal, 
        dtyp = dtyp, calrad = calrad, calref = calref, calbt = calbt, 
        proj = proj)
}
.getNumeric <- function(x, returnNumeric = TRUE) {
    vapply(x, function(xi){
                d <- strsplit(xi, "[^[:digit:]]")[[1]]
                d <- if(returnNumeric) as.numeric(d[d!=""]) else d[d!=""]
                if(length(d)==0) d <- NA_real_
                d[1]
            }, numeric(1))
}