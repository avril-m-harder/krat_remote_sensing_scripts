# install.packages('mgcv')
library(mgcv)
library(scales)

g.col <- 'forestgreen'
w.col <- 'deepskyblue2'
b.col <- 'tan4'
n.col <- 'turquoise4'

random.mnd.nolag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/random_mound_data_nolag.txt', sep='\t')
colnames(random.mnd.nolag) <- c('cell.num','database.name','year','num.off','num.surv','avg.age','num.obs','ann.mean.g','ann.mean.w','ann.mean.b','ann.mean.n','month.szn.1.obs','month.szn.1.g','month.szn.1.w','month.szn.1.b','month.szn.1.n','month.szn.2.obs','month.szn.2.g','month.szn.2.w','month.szn.2.b','month.szn.2.n','month.szn.3.obs','month.szn.3.g','month.szn.3.w','month.szn.3.b','month.szn.3.n','month.szn.4.obs','month.szn.4.g','month.szn.4.w','month.szn.4.b','month.szn.4.n','weather.szn.0.obs','weather.szn.0.g','weather.szn.0.w','weather.szn.0.b','weather.szn.0.n','weather.szn.1.obs','weather.szn.1.g','weather.szn.1.w','weather.szn.1.b','weather.szn.1.n','weather.szn.2.obs','weather.szn.2.g','weather.szn.2.w','weather.szn.2.b','weather.szn.2.n')
random.mnd.6molag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/random_mound_data_6monthlag.txt', sep='\t')
colnames(random.mnd.6molag) <- c('cell.num','database.name','year','num.off','num.surv','avg.age','num.obs','ann.mean.g','ann.mean.w','ann.mean.b','ann.mean.n','month.szn.1.obs','month.szn.1.g','month.szn.1.w','month.szn.1.b','month.szn.1.n','month.szn.2.obs','month.szn.2.g','month.szn.2.w','month.szn.2.b','month.szn.2.n','month.szn.3.obs','month.szn.3.g','month.szn.3.w','month.szn.3.b','month.szn.3.n','month.szn.4.obs','month.szn.4.g','month.szn.4.w','month.szn.4.b','month.szn.4.n','weather.szn.0.obs','weather.szn.0.g','weather.szn.0.w','weather.szn.0.b','weather.szn.0.n','weather.szn.1.obs','weather.szn.1.g','weather.szn.1.w','weather.szn.1.b','weather.szn.1.n','weather.szn.2.obs','weather.szn.2.g','weather.szn.2.w','weather.szn.2.b','weather.szn.2.n') 
random.mnd.1yrlag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/random_mound_data_1yearlag.txt', sep='\t')
colnames(random.mnd.1yrlag) <- c('cell.num','database.name','year','num.off','num.surv','avg.age','num.obs','ann.mean.g','ann.mean.w','ann.mean.b','ann.mean.n','month.szn.1.obs','month.szn.1.g','month.szn.1.w','month.szn.1.b','month.szn.1.n','month.szn.2.obs','month.szn.2.g','month.szn.2.w','month.szn.2.b','month.szn.2.n','month.szn.3.obs','month.szn.3.g','month.szn.3.w','month.szn.3.b','month.szn.3.n','month.szn.4.obs','month.szn.4.g','month.szn.4.w','month.szn.4.b','month.szn.4.n','weather.szn.0.obs','weather.szn.0.g','weather.szn.0.w','weather.szn.0.b','weather.szn.0.n','weather.szn.1.obs','weather.szn.1.g','weather.szn.1.w','weather.szn.1.b','weather.szn.1.n','weather.szn.2.obs','weather.szn.2.g','weather.szn.2.w','weather.szn.2.b','weather.szn.2.n')

## visualize some results - occupied mounds only -- annual means
indeps <- c(8:11,38:41,43:46)        ## independent variables,
colnames(random.mnd.nolag)[indeps]   ## what they are.
deps <- c(4:6)                       ## dependent variables,
colnames(random.mnd.nolag)[deps]     ## what they are.
rands <- 3                           ## 1 random var for now, 
colnames(random.mnd.nolag)[rands]    ## year.
colours <- rep(c(rep(g.col, times=3), rep(w.col, times=3), rep(b.col, times=3), rep(n.col, times=3)), times=3)

pdf('/Users/Avril/Desktop/test.pdf', width=6, height=6)
for(i in indeps){
  for(d in deps){
      gam.mod <- gam(random.mnd.nolag[,d] ~ s(random.mnd.nolag[,i]), method='REML')
      plot(gam.mod, residuals=TRUE, pch=19, col='black', main='No lag')
      gam.mod <- gam(random.mnd.6molag[,d] ~ s(random.mnd.6molag[,i]), method='REML')
      plot(gam.mod, residuals=TRUE, pch=19, col='black', main='6-month lag')
      gam.mod <- gam(random.mnd.1yrlag[,d] ~ s(random.mnd.1yrlag[,i]), method='REML')
      plot(gam.mod, residuals=TRUE, pch=19, col='black', main='1-year lag')
  }
}
dev.off()

for(d in deps){
  gam.mod <- gam(random.mnd.nolag[,d] ~ s(random.mnd.nolag$ann.mean.b) + 
                   s(random.mnd.nolag$ann.mean.g) + s(random.mnd.nolag$ann.mean.n) +
                   s(random.mnd.nolag$ann.mean.w), method='REML')
  plot(gam.mod, residuals=FALSE, pch=19, main='No lag', pages=1)
  gam.mod <- gam(random.mnd.nolag[,d] ~ s(random.mnd.nolag$weather.szn.1.b) + 
                   s(random.mnd.nolag$weather.szn.1.g) + s(random.mnd.nolag$weather.szn.1.n) +
                   s(random.mnd.nolag$weather.szn.1.w), method='REML')
  plot(gam.mod, residuals=FALSE, pch=19, main='No lag', pages=1)
  gam.mod <- gam(random.mnd.nolag[,d] ~ s(random.mnd.nolag$weather.szn.2.b) + 
                   s(random.mnd.nolag$weather.szn.2.g) + s(random.mnd.nolag$weather.szn.2.n) +
                   s(random.mnd.nolag$weather.szn.2.w), method='REML')
  plot(gam.mod, residuals=FALSE, pch=19, main='No lag', pages=1)
}

