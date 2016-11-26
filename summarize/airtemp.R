# Todos os arquivos devem estar no mesmo local. Neste caso os arquivos que 
library(data.table)
library(parallel)

files <- grep('.tsv', dir(), value = T)

reduz_gran <- function(arquivo){
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
   # Mantain only useful variables
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

output <- mclapply(files, reduz_gran, mc.cores = 12)

# Pegar o mês do nome dos arquivos.
names(output) <- substr(sapply(strsplit(files, '\\.'), '[', 5), 1, 6)

soma_de_nas <- sapply(output, function(x){
  sum(is.na(x))
})

