library(rpf)

data(science)
dat <- sfpf[,20:44]
rownames(dat) <- sfpf[[19]]
write.csv(file="science1.csv", dat)

df <- read.csv("~/science1.csv", header=TRUE, row.names=1L,
               sep=";", quote='"', stringsAsFactors=FALSE)
