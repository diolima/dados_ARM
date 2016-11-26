library(data.table)
library(ggplot2)
library(ggthemes)

fulldata <- fread("../data/allvariables.tsv") # Fast read data.table
setnames(fulldata, tolower(colnames(fulldata))) # Lowercase column names

# Date Formatting
fulldata[, day_hour := as.POSIXct(strptime(paste(day, hour, sep=" "), "%Y-%m-%d %H"))] 
fulldata[, day := as.Date(day)]
fulldata[, month := as.numeric(format.Date(day, "%m"))]

# data.table reshaping (wide to long)
m.fulldata <- melt(fulldata[], id=c('day_hour', 'day', 'hour', 'month')) # Melts wide data.table
m.fulldata[, c("measured_var", "stats") := tstrsplit(variable, "_", fixed=T)]
m.fulldata[is.infinite(value), value := NA]

# Exploratory plots by day and month
granularity <- c('day', 'month')
sapply(granularity, function(gran){
	pdf(paste0(gran, '_summary.pdf'), h=10, w=10)
	g <- ggplot(m.fulldata[stats=='mean'], aes(get(gran), value, group=variable)) +
				stat_summary(fun.ymax=max, fun.ymin=min, geom='ribbon', aes(fill=variable)) +
				stat_summary(fun.y=mean, geom='line', color='black') + theme_few() +
				theme_few() + 
				facet_grid(variable~., scales="free") + xlab(gran) 
	if(gran == 'day'){
		g <- g + scale_x_date(date_breaks='1 month', date_labels = "%b %d") +
			     theme(axis.text.x=element_text(angle=45, hjust=1))
	}
	print(g)
	dev.off()
})
# December and November appear to be the months with less outliers and less NA values #


# Variables distribuition
pdf('outliers.pdf', w=10, h=10)
g <- ggplot(m.fulldata[stats=="mean"], aes(x=variable, y=value, fill=variable)) +
						geom_violin() + 
						geom_boxplot(width=0.1, alpha=0.65,
							  fill="white", colour="black", 
							  outlier.color='black', outlier.alpha=0.5) +
						facet_wrap(~variable, scales="free") + theme_few() +
						theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(g)
dev.off()

# You can filter m.fulldata for specific months
pdf('outliers_months.pdf', w=13, h=10)
g <- ggplot(m.fulldata[stats=="mean"], aes(x=variable, y=value, fill=as.factor(month))) +
						geom_violin(position=position_dodge(1)) + 
						geom_boxplot(width=0.1, alpha=0.65,
							  colour="white", aes(fill=as.factor(month)), 
							  outlier.color='black', outlier.alpha=0.5, position=position_dodge(1)) +
						#scale_fill_manual(values=c('steelblue3', 'springgreen2'), labels = c('Nov', 'Dec'), name='Months') +
						facet_wrap(~variable, scales="free") + theme_few() + theme(axis.text.x=element_blank())
print(g)
dev.off()

# Aggregating values by day
daymean <- m.fulldata[stats=="mean", .(mean=mean(value)), by=c("measured_var", "day")][, mean]
fulldt_day <- m.fulldata[, .(max=max(value), min=min(value)), by=c('day', 'measured_var')][, mean := daymean]

# filtering date by month
#m.fulldata[format.Date(day, "%m") == "10"]
