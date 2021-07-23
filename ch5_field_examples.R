#Captain-Price-TF-141

install.packages("pastecs")
install.packages("psych")

library("car")

library("ggplot2")
library(pastecs)
library(psych)
library(reshape)
library(reshape2)

dlf <- read.delim("DownloadFestival(No Outlier).dat", header = TRUE)

hist.day1 <- ggplot(dlf, aes(day1)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 1", y = "Density")

hist.day1

hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)

hist.day2 <- ggplot(dlf, aes(day2)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 1", y = "Density")

hist.day2

hist.day2 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)

hist.day3 <- ggplot(dlf, aes(day3)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 1", y = "Density")

hist.day3

hist.day3 + stat_function(fun = dnorm, args = list(mean = mean(dlf$day1, na.rm = TRUE), sd(dlf$day1, na.rm = TRUE)), colour = "black", size = 1)

qplot.day1 <- qplot(sample = dlf$day1)

qplot.day1

qplot.day2 <- qplot(sample = dlf$day2)

qplot.day2

qplot.day3 <- qplot(sample = dlf$day3)

qplot.day3

describe(dlf$day1)

describe(dlf$day2)

describe(dlf$day3)

stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

describe(cbind(dlf$day1, dlf$day2, dlf$day3))

stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)

describe(dlf[, c("day1", "day2", "day3")])

stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)

round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE), digits = 3)

rexam <- read.delim("RExam.dat", header = TRUE)

rexam

View(rexam)

rexam$uni <- factor(rexam$uni, levels = c(0:1), labels = c("Sussex University"))

round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), digit = 3)

hist.examscores <- ggplot(rexam, aes(exam)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Exam Scores", y = "Density")

hist.examscores

hist.examscores + stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), sd = sd(rexam$exam, na.rm = TRUE)), colour = "black", size = 1)

hist.computer <- ggplot(rexam, aes(computer)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Computer Literacy", y = "Density")

hist.computer

hist.computer + stat_function(fun = dnorm, args = list(mean = mean(rexam$computer, na.rm = TRUE), sd = sd(rexam$computer, na.rm = TRUE)), colour = "black", size = 1)

by(data = rexam$exam, INDICES = rexam$uni, FUN = stat.desc)

by(rexam$exam, rexam$uni, describe)

by(rexam$exam, rexam$uni, stat.desc)

by(rexam$exam, rexam$uni, stat.desc, basic = FALSE, norm = TRUE)

by(cbind(data=rexam$exam, data=rexam$numeracy), rexam$uni, describe)

by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE)

dunceData <- subset(rexam, rexam$uni=="Duncetown University")
sussexData <- subset(rexam, rexam$uni=="Sussex University")

hist.numeracy.duncetown <- ggplot(dunceData, aes(numeracy)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Numeracy Score", y = "Density")

hist.numeracy.duncetown

hist.numeracy.sussex <- ggplot(sussexData, aes(numeracy)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Numeracy Score", y = "Density")

hist.numeracy.sussex

shapiro.test(rexam$exam)

shapiro.test(rexam$numeracy)

by(rexam$exam, rexam$uni, shapiro.test)

by(rexam$numeracy, rexam$uni, shapiro.test)

qplot(sample = rexam$exam)

qplot(sample = rexam$numeracy)

leveneTest(rexam$exam, rexam$uni)

leveneTest(rexam$exam, rexam$uni, center = mean)

leveneTest(rexam$numeracy, rexam$uni)


dlf$day1PlusDay2 <- dlf$day1 + dlf$day2

dlf$logday1 <- log(dlf$day1)

dlf$logday1 <- log(dlf$day1 + 1)

hist.day1log <- ggplot(dlf, aes(logday1)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Log Hygiene score on day 1", y = "Density")

hist.day1log

hist.day1log + stat_function(fun = dnorm, args = list(mean = mean(dlf$logday1, na.rm = TRUE), sd = sd(dlf$logday1, na.rm = TRUE)), colour = "black", size = 1)

dlf$sqrtday1 <- sqrt(dlf$day1)

hist.sqrtday1 <- ggplot(dlf, aes(sqrtday1)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Sqrt Hygiene score on day 1", y = "Density")

hist.sqrtday1

dlf$day1NoOutlier <-ifelse(dlf$day1 > 3, NA, dlf$day1)

View(dlf)

source(file.choose("Rallfun-v35"))

