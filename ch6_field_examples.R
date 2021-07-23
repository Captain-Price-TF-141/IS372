#Captain-Price-TF-141

library("ggm")
library("polycor")
library("boot")
library("ggplot2")
library("Hmisc")


adverts <- c(5,4,4,6,8)
packets <- c(8,9,10,13,15)

advertData <- data.frame(adverts,packets)

scatter <- ggplot(advertData, aes(adverts, packets))

scatter + geom_point()

scatter + geom_point(size = 3) + labs(x = "Adverts", y = "Packets") + scale_y_continuous(limits = c(0, 15),breaks = 0:15) + scale_x_continuous(limits = c(0,9), breaks = 0:9)

examData <- read.delim("Exam ANxiety.dat", header = TRUE)

View(examData)

cor(examData, use = "complete.obs", method = "pearson")

levels(examData$Gender) <- c(0,1)

cor(examData[, sapply(examData, is.numeric)], use = "complete.obs", method = "pearson")

examData

cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "pearson")

cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "kendall")

cor(examData$Exam, examData$Anxiety, use = "pairwise.complete.obs", method = "pearson")

examMatrix <- as.matrix(examData[, c("Exam", "Anxiety", "Revise")])

Hmisc::rcorr(examMatrix)

Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

cor.test(examData$Anxiety, examData$Exam)

cor.test(examData$Revise, examData$Exam)

cor.test(examData$Anxiety, examData$Revise)

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]

cor(examData[, sapply(examData, is.numeric)], use = "complete.obs", method = "pearson")

cor(examData2)

cor(examData[, c("Exam", "Anxiety", "Revise")])

examMatrix <- as.matrix(examData[, c("Exam", "Anxiety", "Revise")])

Hmisc::rcorr(examMatrix)

Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

cor.test(examData$Anxiety, examData$Exam)

cor(examData2)^2

cor(examData2)^2 * 100

#Spearman's correlation coefficent

liarData = read.delim("The Biggest Liar.dat", header = TRUE)

cor(liarData$Position, liarData$Creativity, method = "spearman")

liarMatrix <- as.matrix(liarData[, c("Position", "Creativity")])

rcorr(liarMatrix)

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman", exact = FALSE)

cor.test(liarData$Position, liarData$Creativity, method = "spearman", exact = FALSE)


#Non-parametric,better for more ties
cor(liarData$Position, liarData$Creativity, method = "kendall")

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

#Bootstrapping
bootTau <- function(liarData,i)cor(liarData$Position[i], liarData$Creativity[i], use = "complete.obs", method = "kendall")

boot_kendall <- boot(liarData, bootTau, 2000)

boot_kendall
boot.ci(boot_kendall)

#Biserial and point biserial correlations

catData = read.csv("pbcorr.csv", header = TRUE)

catData

cor(catData$time, catData$gender, method = "pearson")

cor.test(catData$time, catData$gender)

cor.test(catData$time, catData$recode)

catFrequencies <- table(catData$gender)

prop.table(catFrequencies)

polyserial(catData$time, catData$gender)

#partial correlation
examData =read.delim("Exam Anxiety.dat", header = TRUE)

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]

pcor(c("Exam", "Anxiety", "Revise"), var(examData2))

pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2))

pc

pc^2

pcor.test(pc,1,103)

