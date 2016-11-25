library(ggplot)
library(data.table)


fulldata <- fread("allvariables.tsv") # Fast read data.table

m.fulldata <- melt(fulldata, id=1) # Melts wide data.table


# variables distribution
violinplot <- ggplot(m.fulldata, aes(x=, y=value, group=variable, color=variable)) +
					geom_violin() + facet_wrap(~variable)

# time series profile
tsplot <- ggplot(dt, aes(x=, y=value, group=variable, color=variable)) +
				stat_summary(fun.y=mean, geom="line") + 
				stat_summary(fun.ymax=max, fun.ymin=min, geom="ribbon", fill="grey")
				facet_grid(.~variable) + 
				ylab('Value') + xlab('Datetime Hour')


pdf('violinplot.pdf')
print(violinplot)
dev.off()

pdf('tsplot.pdf')
print(tsplot)
dev.off()

