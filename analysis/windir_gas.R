library(data.table)

allvar <- fread('./../data/allvariables.tsv', data.table = F)
wdir_mean <- allvar$wdir_mean
allvar <- allvar[complete.cases(allvar),]
allvar$wamp <- allvar$wdir_max - allvar$wdir_min
allvar <- allvar[,c('CO_mean', 'N2O_mean', 'H2O_mean')]


bin <- cut(, seq(0, 360, by = 30), 
	   labels = paste0(seq(0, 330, by = 30), '-', seq(30, 360, by = 30))) 
allvar$bin <- bin

somevar <- allvar[,c('bin', 'CO_mean', 'N2O_mean', 'H2O_mean')]

pdf('violin_wdir.pdf')
pl <- ggplot(allvar, aes(bin, log10(CO_mean)))
print(pl+geom_violin())
dev.off()


allvar <- allvar


doh <- ggplot()
