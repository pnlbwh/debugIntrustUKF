d <- fread('tractsample.csv')
g <- ggplot(d, aes(x=length, fill=ukftype)) + geom_density(alpha=0.3) + xlim(c(1,200)) + ggtitle("INTRuST: Tract Length: Matlab vs C++")
ggsave(filename="report/tractHist200.png",plot=g)
