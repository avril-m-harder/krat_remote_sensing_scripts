##### 1. Read in data #####
## read in fitness response variables
fit <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/fitness_variables.csv')
fit <- fit[!duplicated(fit),] ## get rid of duplicates in the fitness data set
## offspring == 0 data come from mounds occupied by adult female in that year;
## identify cells with >1 mound within a year and randomly select one to keep per cell per year;
fit$id <- paste0(fit$cell.num,'.',fit$year)
FIT <- NULL
for(i in unique(fit$id)){
  sub <- fit[fit$id == i,]
  mnd <- sample(sub$database.name, 1, replace = FALSE)
  FIT <- rbind(FIT, sub[sub$database.name == mnd,])
}
fit <- FIT
fit <- fit[,-ncol(fit)]

## get list of unique cell number:year combinations
cells <- fit[,c('cell.num','year')]
cells <- cells[!duplicated(cells),] ## n=1,057

## read in remote sensing predictor variables (written from summarize_cells_with_mounds.R)
cell.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/cells_w_mounds_sumstats.csv')

## separate into mean and median data
mean.cols <- c(1,2, grep('mean', colnames(cell.dat)))
mean.cols <- mean.cols[order(mean.cols)]
mean.data <- cell.dat[,c(mean.cols)]
median.cols <- c(1,2,grep('n.obs', colnames(cell.dat)), grep('median', colnames(cell.dat)))
median.cols <- median.cols[order(median.cols)]
median.data <- cell.dat[,c(median.cols)]

## combine remote sensing and fitness data (use mean data)
all.dat <- merge(fit, mean.data, by=c('cell.num','year')) ## nrow = 1,057

## March 1, 2022: remove data from years 1990-1993 due to missing data issues for some seasons
## in those years (just due to temporal distribution of observations, not the calculation of indices)
all.dat <- all.dat[all.dat$year >= 1994,] ## nrow = 932

##### 2. Run algorithm on all dependent variables #####
colnames(all.dat)[colSums(is.na(all.dat)) > 0] ## double-check that only dependent variables have NA values

## remove weather-based season data, just keep 6-month intervals. overkill otherwise
all.dat <- all.dat[,-grep('weather', colnames(all.dat))]

## Feb. 7, 2022: get rid of some indices that don't make sense to include
## (see notes in rs_index_notes.xlsx)
cut <- NULL
for(i in c('.DVI.','.EVI.','.EVI2.','.GEMI.','.MNDWI.','.NBRI.','.NDWI2.','.SATVI.','.SAVI.','.SLAVI.','.TVI.','.WDVI.')){
  cut <- c(cut, grep(i, colnames(all.dat), fixed=TRUE))
}
all.dat <- all.dat[,-cut]

## num.surv and num.off include zero values for mounds occupied by adult females but with no juveniles
## (n=932)
num.surv <- all.dat[,c(5,9:ncol(all.dat))]
num.off <- all.dat[,c(4,9:ncol(all.dat))]
## avg. age and max.age do not include consideration of juvenile-less mounds occupied by adult females
## (thus, fewer observations; n=695)
avg.age <- all.dat[!is.na(all.dat$avg.age),]
avg.age <- avg.age[,c(6,9:ncol(avg.age))]
max.age <- all.dat[!is.na(all.dat$max.age),]
max.age <- max.age[,c(7,9:ncol(max.age))]

write.table(num.surv, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/numsurv.txt',
            quote = FALSE, row.names = FALSE, sep = '\t')
write.table(num.off, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/numoff.txt',
            quote = FALSE, row.names = FALSE, sep = '\t')
write.table(max.age, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/maxage.txt',
            quote = FALSE, row.names = FALSE, sep = '\t')
write.table(avg.age, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/avgage.txt',
            quote = FALSE, row.names = FALSE, sep = '\t')
