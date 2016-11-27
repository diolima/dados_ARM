library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(animation)

fulldata <- fread("../data/allvariables.tsv") # Fast read data.table
setnames(fulldata, tolower(colnames(fulldata))) # Lowercase column names

## Data Formatting ##
fulldata[, day_hour := as.POSIXct(strptime(paste(day, hour, sep=" "), "%Y-%m-%d %H"))] 
fulldata[, day := as.Date(day)]
fulldata[, month := as.numeric(format.Date(day, "%m"))]
fulldata[, year := as.numeric(format.Date(day, "%Y"))]

m.fulldata <- melt(fulldata[], id=c('day_hour', 'day', 'hour', 'month', 'year')) # Melts wide data.table
m.fulldata[, c("measured_var", "stats") := tstrsplit(variable, "_", fixed=T)]
m.fulldata[is.infinite(value), value := NA]


## Exploratory plots by day and month ##
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
# November appears to be the month with less outliers and less NA values #

# Mean airtemp per hour
pdf('mean_airtemp.pdf')
g <- ggplot(m.fulldata[stats=="mean" & measured_var=="airtemp"], aes(hour, value, group=variable)) +
			stat_summary(fun.ymin=min, fun.ymax=max, geom='ribbon', fill='grey') +
			stat_summary(fun.y=mean, geom='line') + 
			ylab('Temperature') +
			theme_gdocs()
print(g)
dev.off()

# Mean rh per hour
pdf('mean_rh.pdf')
g <- ggplot(m.fulldata[stats=="mean" & measured_var=="rh"], aes(hour, value, group=variable)) +
			stat_summary(fun.ymin=min, fun.ymax=max, geom='ribbon', fill='grey') +
			stat_summary(fun.y=mean, geom='line') + 
			ylab('Relative Humidity') +
			theme_gdocs()
print(g)
dev.off()

# Time series plot all variables in november
sapply(granularity[1], function(gran){
	pdf(paste0(gran, '_nov_summary.pdf'), h=10, w=10)
	g <- ggplot(m.fulldata[stats=='mean' & month==11], aes(get(gran), value, group=variable)) +
				stat_summary(fun.ymax=max, fun.ymin=min, geom='ribbon', aes(fill=variable)) +
				stat_summary(fun.y=mean, geom='line', color='black') + theme_few() +
				theme_few() + 
				facet_grid(variable~year, scales="free") + xlab(gran) 
	if(gran == 'day'){
		g <- g + scale_x_date(date_breaks='1 month', date_labels = "%b %d") +
			     theme(axis.text.x=element_text(angle=45, hjust=1))
	}
	print(g)
	dev.off()
})


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

pdf('correlations.pdf', h=10, w=10)
g <- ggpairs(fulldata[, grep('mean', colnames(fulldata)), with=F],
		lower=list(continuous=wrap('points', size=0.4, alpha=0.3))) + 
		theme_few()
print(g)
dev.off()

# Aggregating values by day
daymean <- m.fulldata[stats=="mean", .(mean=mean(value)), by=c("measured_var", "day")][, mean]
fulldt_day <- m.fulldata[, .(max=max(value), min=min(value)), by=c('day', 'measured_var')][, mean := daymean]

# filtering date by month
#m.fulldata[format.Date(day, "%m") == "10"]


windrose_plot <- function(dt, wspeed, wdir){
	dt <- na.omit(dt[, c(wspeed, wdir), with=F])
	dt[, speed.bin := cut(get(wspeed), breaks=6, dig.lab=1)]
	setattr(dt$speed.bin,"levels", gsub('\\((.*),(.*)\\]', '\\1 - \\2', levels(dt$speed.bin)))
	dt[, dir.bin := cut(get(wdir), breaks=seq(0,360, by=30), dig.lab=2)]
	g <- ggplot(dt, aes(dir.bin)) +
				geom_bar(data=dt, aes(x=dir.bin, fill=speed.bin, y = (..count..)/sum(..count..))) +
				coord_polar(start=-(15/360)* 2*pi) + 
				#ylim(0,0.5)+
				#scale_y_continuous(limits=c(0,100))+
				scale_x_discrete(drop = FALSE,
								labels = c("N","NNE","NE","ENE", "E", 
													"ESE", "SE","SSE", 
													"S","SSW", "SW","WSW", "W", 
													"WNW","NW","NNW")) +	
				scale_fill_discrete(drop=FALSE, name='Wind Speed')
				#scale_x_continuous(breaks=seq(0, 360, by=30), lim=c(0,360))	
	print(g)
}



windrose_gif <- function(dt, wspeed, wdir, by_var='day', filename){
	dt <- na.omit(dt[, c(by_var, wspeed, wdir), with=F])
	dt[, speed.bin := cut(get(wspeed), breaks=6, dig.lab=1)]
	setattr(dt$speed.bin,"levels", gsub('\\((.*),(.*)\\]', '\\1 - \\2', levels(dt$speed.bin)))
	dt[, dir.bin := cut(get(wdir), breaks=seq(0,360, by=30), dig.lab=2)]
	iterator <- unique(as.character(dt[, get(by_var)]))
	saveGIF({
		for (i in iterator){
			g <- ggplot(dt, aes(dir.bin)) +
						geom_bar(data=dt[get(by_var) == i], aes(x=dir.bin, fill=speed.bin, y = (..count..)/sum(..count..))) +
						coord_polar(start=-(15/360)* 2*pi) + 
						ylim(0,0.7)+
						#scale_y_continuous(limits=c(0,100))+
						scale_x_discrete(drop = FALSE,
										labels = c("N","NNE","NE","ENE", "E", 
															"ESE", "SE","SSE", 
															"S","SSW", "SW","WSW", "W", 
															"WNW","NW","NNW")) +	
						scale_fill_discrete(drop=FALSE, name='Wind Speed')+
						ylab('Frequency') + theme_minimal() + ggtitle(paste0('Day ',i)) + xlab('')
						#scale_x_continuous(breaks=seq(0, 360, by=30), lim=c(0,360))	
			print(g)
		}	
	}, interval = 0.1, movie.name = filename, ani.width = 600, ani.height = 600)
}




