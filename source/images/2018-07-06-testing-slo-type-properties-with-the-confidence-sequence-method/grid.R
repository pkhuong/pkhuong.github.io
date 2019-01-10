data <- read.table('data.tab', header=TRUE)

png("grid-small.png", width=800, height=600)
ggplot(data, aes(x=density)) + geom_histogram() + geom_density() + facet_grid(bins ~ bin_size) + labs(title="Distribution of total fill rate until first saturated bin (rows: bin count, 2-1000; columns: bin capacity, 30k-128k; n = 10000)", x="Total fill rate (balls_placed / (bin_count * bin_capacity))")
dev.off()

png("grid.png", width=1600, height=1200)
ggplot(data, aes(x=density)) + geom_histogram() + geom_density() + facet_grid(bins ~ bin_size) + labs(title="Distribution of total fill rate until first saturated bin (rows: bin count, 2-1000; columns: bin capacity, 30k-128k; n = 10000)", x="Total fill rate (balls_placed / (bin_count * bin_capacity))")
dev.off()
