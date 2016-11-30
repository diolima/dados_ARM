library(data.table)
library(GGally)
library(ggplot2)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(animation)


value2NA <- function(dtable, condition, substitute){
	#condition example: '>0', '==1', '<=0', '=="tobias"'
	for(j in seq_along(dtable)){
		vectype <- class(dtable[[j]])
		if (vectype=='integer' || vectype=='numeric' || vectype=='character'){
			set(dtable,
			i=which(eval(parse(text=paste0('dtable[[j]]', condition)))),
			j=j, value=substitute)
		}
	}
}

firedata <- fread('../data/fire_archive_M6_3376.csv') 
firedata <- firedata[confidence>=70]
firedata[, c('year', 'month', 'day') := tstrsplit(acq_date, '-')]
firedata[, month := month.abb[as.numeric(month)]]
firedata[, acq_date := as.Date(acq_date)]

fulldata <- fread("../data/allvariables.tsv") # Fast read data.table
setnames(fulldata, tolower(colnames(fulldata))) # Lowercase column names

## Data Formatting ##
fulldata[, day_hour := as.POSIXct(strptime(paste(day, hour, sep=" "), "%Y-%m-%d %H"))] 
fulldata[, day := as.Date(day)]
fulldata[, month_day := as.Date(format.Date(day, "%b-%d"), "%b-%d")]
fulldata[, month := as.numeric(format.Date(day, "%m"))]
fulldata[, year := as.numeric(format.Date(day, "%Y"))]
fulldata[, monthname := factor(month.abb[as.numeric(month)], levels=month.abb)]

value2NA(fulldata, "==-1", NA)

m.fulldata <- melt(fulldata, id=c('day_hour', 'day', 'hour', 'month', 'year', 'monthname', 'month_day')) # Melts wide data.table
m.fulldata[, c("measured_var", "stats") := tstrsplit(variable, "_", fixed=T)]
m.fulldata[is.infinite(value), value := NA]


## Exploratory plots ##
granularity <- c('day', 'monthname')
sapply(granularity, function(gran){
	pdf(paste0(gran, '_summary.pdf'), h=10, w=10)
	g <- ggplot(m.fulldata[stats=='mean'], aes(get(gran), value, group=variable)) +
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

pdf('mean_airtemp_rh.pdf', h=5, w=12)
	g <- ggplot(m.fulldata[stats=="mean" & (measured_var=="airtemp" | measured_var=="rh")], aes(hour, value, group=measured_var)) +
			stat_summary(fun.ymin=min, fun.ymax=max, geom='ribbon', aes(fill=measured_var)) +
			stat_summary(fun.y=mean, geom='line') + 
			ylab('Value') + xlab('Hour of the Day') + 
			facet_grid(measured_var~., scales='free') + scale_fill_manual(name='', values=c('#d87070', '#6d9df2')) +  
			theme_gdocs() + theme(legend.position='none')
print(g)
dev.off()

# Variables distribuition
pdf('outliers.pdf', w=10, h=10)
g <- ggplot(m.fulldata[stats=="mean"], aes(x=variable, y=value, fill=variable)) +
						geom_violin() + 
						geom_boxplot(width=0.1, alpha=0.65,
							  fill="white", colour="black", 
							  outlier.color='black') +
						facet_wrap(~variable, scales="free") + theme_few() +
						theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
print(g)
dev.off()

# You can filter m.fulldata for specific months
pdf('outliers_months.pdf', w=13, h=10)
g <- ggplot(m.fulldata[stats=="mean"], aes(x=variable, y=value, fill=monthname)) +
						geom_violin(position=position_dodge(1)) + 
						geom_boxplot(width=0.1, alpha=0.65,
							  colour="white", aes(fill=monthname), 
							  outlier.color='black',  position=position_dodge(1)) +
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

		
windrose_plot <- function(dt, wspeed, wdir, facet=''){
	if (length(facet) == 1){
		if (facet != ''){
			dt <- na.omit(dt[, c(wspeed, wdir, facet), with=F])
		}else{
			dt <- na.omit(dt[, c(wspeed, wdir), with=F])
		}
	}
	else{
		dt <- na.omit(dt[, c(wspeed, wdir, facet), with=F])
	} 
	dt[, speed.bin := cut(get(wspeed), breaks=6, dig.lab=1)]
	setattr(dt$speed.bin,"levels", gsub('\\((.*),(.*)\\]', '\\1 - \\2', levels(dt$speed.bin)))
	dt[, dir.bin := cut(get(wdir), breaks=seq(0,360, by=30), dig.lab=2)]
	palette <- rev(colorRampPalette(brewer.pal(6,'Blues'))(6))
	g <- ggplot(dt, aes(dir.bin)) +
			geom_bar(data=dt, aes(x=dir.bin, fill=factor(speed.bin, levels=rev(levels(speed.bin))),
												   	y = (..count..)/sum(..count..))) +
			coord_polar(start=-(15/360)* 2*pi) + 
			scale_x_discrete(drop = FALSE,
							labels = c("N","NNE","NE","ENE", "E", 
												"ESE", "SE","SSE", 
												"S","SSW", "SW","WSW", "W", 
												"WNW","NW","NNW")) +	
			scale_fill_manual(values=palette, drop=FALSE, name='Wind Speed') +
			ylab('Frequency') + theme_minimal()  + xlab('')
	if (length(facet) == 1){
		if (facet != ''){
			g <- g + facet_wrap(~get(facet))
		}
	}else if (length(facet) == 2){
		g <- g + facet_grid(get(facet[1])~get(facet[2]))
	}else{
		stop('Maximum length of facet is 2')
	}
	return(g)
}

pdf('winds_months_years.pdf')
print(windrose_plot(fulldata, 'wspeed_mean', 'wdir_mean', 'monthname') + 
	  theme(axis.text.x=element_text(size=5)))
dev.off()



windrose_gif <- function(dt, wspeed, wdir, by_var='day', filename){
	dt <- na.omit(dt[, c(by_var, wspeed, wdir), with=F])
	dt[, speed.bin := cut(get(wspeed), breaks=6, dig.lab=1)]
	setattr(dt$speed.bin,"levels", gsub('\\((.*),(.*)\\]', '\\1 - \\2', levels(dt$speed.bin)))
	dt[, dir.bin := cut(get(wdir), breaks=seq(0,360, by=30), dig.lab=2)]
	iterator <- unique(as.character(dt[, get(by_var)]))
	palette <- rev(colorRampPalette(brewer.pal(6,'Blues'))(6))
	saveGIF({
		for (i in iterator){
			g <- ggplot(dt, aes(dir.bin)) +
						geom_bar(data=dt[get(by_var) == i], aes(x=dir.bin, fill=factor(speed.bin, levels=rev(levels(speed.bin))),
															   	y = (..count..)/sum(..count..))) +
						coord_polar(start=-(15/360)* 2*pi) + 
						ylim(0,0.7)+
						scale_x_discrete(drop = FALSE,
										labels = c("N","NNE","NE","ENE", "E", 
															"ESE", "SE","SSE", 
															"S","SSW", "SW","WSW", "W", 
															"WNW","NW","NNW")) +	
						scale_fill_manual(values=palette, drop=FALSE, name='Wind Speed') +
						ylab('Frequency') + theme_minimal() + ggtitle(paste0('Day ',i)) + xlab('')
			print(g)
		}	
	}, interval = 0.1, movie.name = filename, ani.width = 600, ani.height = 600)
}


pdf('co_anomaly.pdf')
g <- ggplot(m.fulldata[measured_var=='co' & stats=='mean'],
	   aes(month_day, value, group=1)) +
		stat_summary(fun.ymax=max, fun.ymin=min, geom='ribbon',
					 aes(fill=variable)) +
		stat_summary(fun.y='mean', geom='line', color='black') +
		theme_hc() +
		facet_grid(year~monthname, scales="free") +
		theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
		scale_fill_manual(name='', values="steelblue3",
						  labels="Carbon monoxide (CO)") +
		xlab('') + 
		ylab('Gas Concentration (ppmv)')
print(g)
dev.off()
