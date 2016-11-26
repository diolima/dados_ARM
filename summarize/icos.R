library(data.table)
library(parallel)

# All files must be in dir path
files <- grep('.tsv', dir('~/work/raw_proj/gases/CO', full.name = T), value = T)

reduz_gran <- function(arquivo){
  print(arquivo)
  gases <- fread(arquivo, skip = 39, data.table = F, header = F)
  colnames(gases) <- c('datetime', 'CO', 'N2O', 'H2O') 
  date <- gsub(' .*', '', gases$datetime)
  split_date <- strsplit(date, '-') 
  year <- sapply(split_date, '[', 1)
  month <- sapply(split_date, '[', 2)
  day <- sapply(split_date, '[', 3) 
  hour <- gsub(".* (\\d{2}).*", '\\1', gases$datetime)
  m_d_h <- paste(year, month, day, hour, sep = '_')
  gases <- gases[,-1] 
  aggdata_mean <- aggregate(gases, by = list(m_d_h), FUN = mean, na.rm = T)
  aggdata_min <- aggregate(gases, by = list(m_d_h), FUN = min, na.rm = T)
  aggdata_max <- aggregate(gases, by = list(m_d_h), FUN = max, na.rm = T)
  colnames(aggdata_mean) <- paste0(colnames(aggdata_mean), '_mean')
  colnames(aggdata_min) <- paste0(colnames(aggdata_min), '_min')
  colnames(aggdata_max) <- paste0(colnames(aggdata_max), '_max')
  sum_day <-  gsub('_[0-9]{2}$', '', aggdata_mean[,1])
  sum_hour <- gsub('.*_', '', aggdata_mean[,1]) 
  output <- cbind(day = sum_day, hour = sum_hour, aggdata_mean[,-1], 
		   aggdata_min[,-1], aggdata_max[,-1])
  output
}

output <- mclapply(files, reduz_gran, mc.cores = 12)

# Naming months 
names(output) <- substr(sapply(strsplit(files, '\\.'), '[', 5), 1, 6)
# Ordering months
output <- output[order(names(output))]
# Getting sum of NAs in each month
soma_de_nas <- sapply(output, function(x){
  sum(is.na(x))
})

# Setting up output dataframe
out_df <- do.call(rbind, output)
rownames(out_df) <- NULL
out_df$day <- as.Date(out_df$day, format = '%Y_%m_%d')
out_df[is.na(out_df)] <- NA
out_df[out_df == -Inf] <- NA
out_df[out_df == Inf] <- NA

# Checking manually
#    set.seed(42)
#    mock_df <- out_df[sample(1:nrow(out_df), 3),]
#    mock_out <- fread("/home/diogenes/work/raw_proj/gases/CO/maomaoscs1.co.01s.00.20151001.000000.m02.tsv", data.table = F, skip = 39, header = F)
#    colnames(mock_out) <- c('datetime', 'CO', 'N2O', 'H2O') 
#    mock_out <- subset(mock_out, grepl('2015-10-02 10:', mock_out$datetime))
#    print(sapply(mock_out[,-1], mean, na.rm = T))
# Everything checks out

# Writing output
write.table(out_df, './../data/icos.tsv', quote = F, sep = '\t', row.names = F)
