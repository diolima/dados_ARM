library(ggplot2)
library(data.table)
library(splitstackshape)

fulldata <- fread("../data/allvariables.tsv") # Fast read data.table
setnames(fulldata, tolower(colnames(fulldata)))

#fulldata[, hour := paste(hour, ":00:00", sep="")]
#fulldata[, day_hour := as.POSIXct(paste(day, hour, sep=" "), format="%Y-%d-%m %H:%M:%S")]
fulldata[, day_hour := paste(day, hour, sep=" ")]

m.fulldata <- melt(fulldata, id=c('day_hour', 'day', 'hour')) # Melts wide data.table
m.fulldata[, correct_hour := as.POSIXct(strptime(day_hour, "%Y-%m-%d %H"))]
m.fulldata <- cSplit(m.fulldata, "variable", "_")

# variables distribution
#violinplot <- ggplot(m.fulldata, aes(x=, y=value, group=variable, color=variable)) +
#					geom_violin() + facet_wrap(~variable)

# time series profile
#tsplot <- ggplot(m.fulldata[grep('mean', variable)], aes(x=day_hour, y=value, group=variable, color=variable)) +
#				stat_summary(fun.y=mean, geom="line") + 
#				stat_summary(fun.ymax=max, fun.ymin=min, geom="ribbon", fill="grey") +
#				facet_grid(.~variable) + 
#				ylab('Value') + xlab('Datetime Hour')


#pdf('violinplot.pdf')
#print(violinplot)
#dev.off()

#pdf('tsplot.pdf')
#print(tsplot)
#dev.off()

