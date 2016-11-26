# Function call to merge output from airtemp.R and gas.R
library(data.table)

vaisala <- fread('./../data/vaisala.tsv', data.table = F)
icos <- fread('./../data/icos.tsv', data.table = F)

merged <- merge(vaisala, icos, by = c('day', 'hour'))
# Output was manually checked for consistency in icos and vaisala script
# Be cautious with ordering in hour and day

write.table(merged, './../allvariables.tsv', sep = '\t', quote = F)
