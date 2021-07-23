#Captain-Price-TF-141

install.packages("ggplot2")
install.packages("pastecs")
install.packages("WRS2")

library(ggplot2)
library(pastecs)
library(WRS2)

spiderLong <- read.delim("SpiderLong.dat", header = TRUE)
spiderWide <- read.delim("SpiderWide.dat", header = TRUE)

View(spiderLong)

View(spiderWide)

bar <- ggplot(spiderLong, aes(Group, Anxiety))

bar + stat_summary(fun = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0) + labs(x = "Group", y = "Anxiety") + theme(legend.position = "none")

spiderWide$pMean <- (spiderWide$picture + spiderWide$real)/2

View(spiderWide)

grandMean <- mean(c(spiderWide$picture, spiderWide$real))

spiderWide$adj <- grandMean - spiderWide$pMean

View(spiderWide)

spiderWide$picture_adj <- spiderWide$picture - spiderWide$adj

spiderWide$real_adj <- spiderWide$real - spiderWide$adj

View(spiderWide)

rmMeanAdjust <- function(dataframe)
{
  varNames <- names(dataframe)
  
  pMean <- (dataframe[,1] + dataframe[,2])/2
  grandmean <- mean(c(dataframe[,1], dataframe[,2]))
  adj <- grandmean - pMean
  varA_adj <-dataframe[,1] + adj
  varB_adj <-dataframe[,2] + adj
  
  output <- data.frame(varA_adj, varB_adj)
  names(output) <- c(paste(varNames[1], "adj", sep = "_"), paste(varNames[2], "_adj", sep = "_"))
  return(output)
}

rmMeanAdjust(spiderWide)

spiderWide$pMean2 <- (spiderWide$picture_adj + spiderWide$real_adj)/2

spiderWide$id <- gl(12, 1, labels = c(paste("P", 1:12, sep = "_")))

adjustedData <- melt(spiderWide, id = c("id", "picture", "real", "pMean", "adj", "pMean2"), measured = c("pictured_adj", "real_adj"))

adjustedData <- adjustedData[, -c(2:6)]

names(adjustedData) <- c("id", "Group", "Anxiety_Adj")

adjustedData$Group <- factor(adjustedData$Group, labels = c("Spider Picture", "Real Spider"))

bar <- ggplot(adjustedData, aes(Group, Anxiety_Adj))

bar + stat_summary(fun = mean, geom = "bar", fill = "white", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +labs(x = "Type of Stimulus", y = "Anxiety") +scale_y_continuous(limits = c(0, 60), breaks = seq(from = 0, to = 60, by = 10))

ind.t.test <- t.test(Anxiety ~ Group, data = spiderLong)

ind.t.test

int.t.test2 <- t.test(spiderWide$real, spiderWide$picture)

regcompar <- lm(Anxiety ~ Group, data = spiderLong)

summary(regcompar)

#Robust methods Yuen

yuen(Anxiety ~ Group, data = spiderLong)

yuenbt(Anxiety ~ Group, data = spiderLong, tr = .2, nboot = 400)

yuenbt(Anxiety ~ Group, data = spiderLong, tr = .2, nboot = 2000)

#Effect sizes

t <- ind.t.test$statistic[[1]]
df <- ind.t.test$parameter[[1]]

r <- sqrt(t^2/(t^2+df))

round(r, 3)

#Dependent T-Test
dep.t.test <- t.test(spiderWide$real, spiderWide$picture, paired = TRUE)

dep.t.test

yuend(spiderWide$real, spiderWide$picture)

#Effect size calculation
t <- dep.t.test$statistic[[1]]

df <- dep.t.test$parameter[[1]]

r <- sqrt(t^2/(t^2+df))

round(r, 3)











