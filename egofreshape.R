install.packages("reshape"); 
library(reshape)
id <- c(1,1,2,2)
time <- c(1,2,1,2)
x1 <- c(5,3,6,2)
x2 <- c(6,5,1,4)
mydata <- data.frame(id,time,x1,x2)
md <- melt(mydata, id=(c("id", "time")))
md
newdata <- cast(md, id~variable, mean)
newdata
