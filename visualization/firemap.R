library(data.table)
library(ggmap)
library(ggthemes)

firedata <- fread('../data/fire_archive_M6_3376.csv')
fulldata <- fread('../data/allvariables.tsv')
firedata <- firedata[confidence >= 70]
firedata[, c('year', 'month', 'day') := tstrsplit(acq_date, '-')]
firedata[, month := factor(month.abb[as.numeric(month)], levels=month.abb)]
map <- get_map(location = 'Manacapuru, Brazil', zoom = 10)

pdf('roi.pdf')
region <- ggmap(map) + ggtitle('Region of Interest') +
			ylab('') + xlab('') +
			theme_minimal() +
			theme(axis.text=element_blank(),
				 axis.ticks=element_blank())
print(region)
dev.off()


firepoints <- ggmap(map) +
					geom_point(data=firedata[confidence>=70],
							   aes(longitude, latitude),
							   alpha=0.7, color='brown1') +
					ylab('') + xlab('') +
					ggtitle('Fire - Manacapuru, Brazil - NASA') +
					theme_minimal() + theme(axis.text=element_blank(),
											axis.ticks=element_blank())

firepoints_month <- firepoints + facet_wrap(~month)
firepoints_month_year  <- firepoints + facet_grid(year~month)
pdf('fire_month.pdf')
print(firepoints_month)
dev.off()
pdf('fire_month_year.pdf', h=3)
print(firepoints_month_year)
dev.off()



