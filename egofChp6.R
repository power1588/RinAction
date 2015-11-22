install.packages("vcd")
library(vcd)
counts <- table(Arthritis$Improved)
counts
barplot(counts,
        main="Simple Bar Plot",
        xlab = "Improvement", ylab = "Frequency")
barplot(counts,
        main="Simple Bar Plot",
        xlab = "Improvement", ylab = "Frequency",
        horiz = TRUE)
plot(Arthritis$Improved, main = "Simple Bar Plot",
     xlab = "Improved", ylab = "Frequency")
plot(Arthritis$Improved,  horiz = TRUE, main = "Simple Bar Plot",
     xlab = "Improved", ylab = "Frequency")
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts,
        main = "Stacked Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = rownames(counts))
barplot(counts,
        main = "Grouped Bar Plot",
        xlab = "Treatment", ylab = "Frequency",
        col = c("red", "yellow", "green"),
        legend = rownames(counts), beside = TRUE)

states <- data.frame(state.region,state.x77)
states
means <- aggregate(states$Illiteracy, by = list(state.region), FUN = mean)
means
means <- means[order(means$x),]
means
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")

par(mar=c(10,8,4,10))
par(las = 1)
counts <- table(Arthritis$Improved)
barplot(counts,
        main = "Treatment Outcome",
        horiz = TRUE, cex.names = 0.8,
        names.arg = c("No Improvement","Some Impro","Marked Impro"))

library(vcd)
attach(Arthritis)
counts <- table(Treatment,Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)

#eg 6.5
par(mfrow = c(2,2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US","UK","AUS","GER","FRA")
pie(slices, labels = lbls,
    main = "Simple Pie Chart")
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls2)),
    main = "Pie Chart with percentages")
library(plotrix)
pie3D(slices, labels = lbls2, explode = 0.1,
      main = "3D Pie Chart")
mytable <- table(state.region)
lbls3 <- paste(names(mytable),"\n", mytable, sep="")
pie(mytable, labels = lbls3,
    main="Pie Chart from a Table\n (with sample sizes)")

#eg 6.6
par(mfrow = c(1,1))
hist(mtcars$mpg)
hist(mtcars$mpg, 
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored histogram with 12 bins")
hist(mtcars$mpg,
     freq = FALSE,
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main=" Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col="blue", lwd=2)
x <- mtcars$mpg
h <- hist(x,
          breaks=12,
          col="red",
          xlab="Miles Per Gallon",
          main="Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean=mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()

#eg 6.8
par(lwd = 2)
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4, 6, 8),
                labels = c("4 cylinder", "6 cylinder",
                           "8 cylinder"))
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")
title(main= "MPG Distribution by Car Cylinders")
colfill <- c(2:(1+length(levels(cyl.f))))
#legend(locator(1), levels(cyl.f),fill=colfill)
detach(mtcars)

#eg 6.11
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex = .7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch = 19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")
