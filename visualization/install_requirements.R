required <- c('data.table',
			  'ggplot2',
			  'GGally',
			  'ggmap',
			  'ggthemes',
			  'scales',
			  'RColorBrewer',
			  'animation')

to_install <- required[which(!required %in% installed.packages)]

if (length(to_install) == 0){
	stop('All packages are already installed')
} else {
	cat('Installing the following packages:\n')
	sapply(to_install, function(x) cat(paste0(x, '\n')))
	install.packages(to_install)
}
