one <- read.table('one_nondigits.tab', header=TRUE)

png('one_nondigits.png', width=800, height=600)
ggplot(data=one, aes(x=Algorithm, y=Cycles, colour=Algorithm)) + geom_jitter(size=1, alpha=0.01) + geom_boxplot(colour='black',outlier.alpha=0, alpha=0.1) + geom_point(y=-1000, alpha=1, size=10) + facet_wrap( ~ Dataset) + theme(axis.text.x = element_blank()) + coord_cartesian(ylim = c(0, 400)) + ggtitle('Distribution of cycles for random integers from a few distribution')
dev.off()

png('one_nondigits_large.png', width=2000, height=1500)
ggplot(data=one, aes(x=Algorithm, y=Cycles, colour=Algorithm)) + geom_jitter(size=2.5, alpha=0.01) + geom_boxplot(colour='black',outlier.alpha=0, alpha=0.1) + geom_point(y=-1000, alpha=1, size=10) + facet_wrap( ~ Dataset) + theme(axis.text.x = element_blank()) + coord_cartesian(ylim = c(0, 250)) + ggtitle('Distribution of cycles for random integers from a few distribution')
dev.off()

one_d <- read.table('one_digits_filtered.tab', header=TRUE)
summary <- one_d %>% group_by(Algorithm, Count) %>% summarise(Cycles_lo = quantile(Cycles, prob=0.1), Cycles_hi = quantile(Cycles, prob=0.9), Cycles=mean(Cycles))

png('one_digits.png', width=800, height=600)
ggplot(summary, aes(x=Count, fill=Algorithm)) + geom_ribbon(aes(ymin=Cycles_lo, ymax=Cycles_hi), alpha=0.25) + geom_line(aes(y=Cycles, colour=Algorithm), alpha=1) + ggtitle('Average cycle count for random integers uniformly chosen in range (+/- 10th/90th percentile)') + xlab('Size of integers (digits)')
dev.off()

png('one_digits_large.png', width=2000, height=1500)
ggplot(summary, aes(x=Count, fill=Algorithm)) + geom_ribbon(aes(ymin=Cycles_lo, ymax=Cycles_hi), alpha=0.25) + geom_line(aes(y=Cycles, colour=Algorithm), alpha=1) + ggtitle('Average cycle count for random integers uniformly chosen in range (+/- 10th/90th percentile)') + xlab('Size of integers (digits)') + coord_cartesian(ylim = c(0, 200))
dev.off()
