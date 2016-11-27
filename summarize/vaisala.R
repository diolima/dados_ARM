# All files must be in the same folder. 
library(data.table)
library(parallel)

files <- grep('.tsv', 
	      dir('~/work/raw_proj/wind_data/springston-met', full.names = T),
	      value = T)

summarize <- function(arquivo){
   nov <- fread(arquivo, skip = 39, data.table = F)
   colnames(nov) <- c('datetime', 'wspeed', 'wdir', 'airtemp', 'rh', 'baro', 'rain')
   # Get dates and character to use as factor to aggregate
   date <- gsub(' .*', '', nov$datetime)
   split_date <- strsplit(date, '-') 
   year <- sapply(split_date, '[', 1)
   month <- sapply(split_date, '[', 2)
   day <- sapply(split_date, '[', 3) 
   hour <- gsub(".* (\\d{2}).*", '\\1', nov$datetime)
   m_d_h <- paste(year, month, day, hour, sep = '_')
   print(sample(m_d_h, 10))
   # Maintain only useful variables
   dat <- nov[,c('wspeed', 'wdir', 'airtemp', 'rh', 'baro', 'rain')]
   aggdata_mean <- aggregate(dat, by = list(m_d_h), FUN = mean, na.rm = T)
   aggdata_min <- aggregate(dat, by = list(m_d_h), FUN = min, na.rm = T)
   aggdata_max <- aggregate(dat, by = list(m_d_h), FUN = max, na.rm = T)
   colnames(aggdata_mean) <- paste0(colnames(aggdata_mean), '_mean')
   colnames(aggdata_min) <- paste0(colnames(aggdata_min), '_min')
   colnames(aggdata_max) <- paste0(colnames(aggdata_max), '_max')
   sum_day <-  gsub('_[0-9]{2}$', '', aggdata_mean[,1])
   sum_hour <- gsub('.*_', '', aggdata_mean[,1]) 
   output <- cbind(day = sum_day, hour = sum_hour, aggdata_mean[,-1], 
		   aggdata_min[,-1], aggdata_max[,-1])
   output
}

# Parallel lapply to read and summarize data
output <- mclapply(files, summarize, mc.cores = 12)

# Retrieving month from file names.
names(output) <- substr(sapply(strsplit(files, '\\.'), '[', 5), 1, 6)
output <- output[order(names(output))]

# Getting NA sum for each month
na_sum <- sapply(output, function(x){
  sum(is.na(x))
})

# Appending full data frame
out_df <- do.call(rbind, output)
out_df$day <- as.Date(out_df$day, format = '%Y_%m_%d')
rownames(out_df) <- NULL

# Manual test 
#    mock_df <- out_df[complete.cases(out_df),]
#    set.seed(42)
#    mock_set <- mock_df[sample(1:nrow(mock_df), 3),]
#    full_df <- fread(files[22], skip = 39, data.table = F)
#    colnames(full_df) <- c('datetime', 'wspeed', 'wdir', 'airtemp', 'rh', 'baro', 'rain')
#    only_selected <- subset(full_df, grepl('^2015-10-03 04:', full_df$datetime))
#    summary(only_selected)
### wdir_mean == 82.42

### Everything is correct. Saving file and moving on ###
write.table(out_df, './../data/vaisala.tsv', quote = F, 
	    sep = '\t', row.names = F)

# Summarize only wdir
summarize_wdir <- function(arquivo){
   nov <- fread(arquivo, skip = 39, data.table = F)
   colnames(nov) <- c('datetime', 'wspeed', 'wdir', 'airtemp', 'rh', 'baro', 'rain')
   nov <- nov[,c('datetime', 'wspeed', 'wdir')]
   nov <- nov[complete.cases(nov),]
   # Get dates and character to use as factor to aggregate
   date <- gsub(' .*', '', nov$datetime)
   split_date <- strsplit(date, '-') 
   year <- sapply(split_date, '[', 1)
   month <- sapply(split_date, '[', 2)
   day <- sapply(split_date, '[', 3) 
   hour <- gsub(".* (\\d{2}).*", '\\1', nov$datetime)
   # vector to aggregate  
   m_d_h <- paste(year, month, day, hour, sep = '_')
   # Submitting m_d_h to data_frame
   nov$facdat <- m_d_h
   split_day <- split(nov, nov$facdat)
   # Iterate
   split_pro <- mclapply(split_day, function(day){
     wt <- day$wspeed/nrow(day)
     xm <- weighted.mean(day$wdir,wt)      
     data.frame(facdat = unique(day$facdat), 
	        wt_mean_wdir = xm,
		nwt_mean_wdir = mean(day$wdir),
                sd_wdir = sd(day$wdir),
	        mean_speed = mean(day$wspeed), 	
		speed_sd = sd(day$wspeed))
   }, mc.cores = 3)
   df_pro <- do.call(rbind, split_pro)
   rownames(df_pro) <- NULL
   sum_day <-  gsub('_[0-9]{2}$', '', df_pro[,1])
   sum_hour <- gsub('.*_', '', df_pro[,1]) 
   output <- cbind(day = sum_day, hour = sum_hour, df_pro[,-1, drop = F])
   output
}
output2 <- mclapply(files, summarize_wdir, mc.cores = 6)

# Assembling data frame
winddf <- do.call(rbind, output2)
winddf$day <- as.Date(winddf$day, format = '%Y_%m_%d')
row.names(winddf) <- NULL

write.table(winddf, './../data/winddata_vaisala.tsv', sep = '\t', quote = F, row.names = F)

# Snippet to plot distributions
# library(ggplot2)
# pdf('~/corr_p.pdf')
# print(ggplot(winddf, aes(wt_mean_wdir, nwt_mean_wdir)) + geom_point(aes(colour = sd_wdir)))
# dev.off()
