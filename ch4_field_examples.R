#Captain-Price-TF-141

#For installing and loading ggplot2
library(reshape)
library(reshape2)
library(ggplot2)

#load facebook data
facebookData<-read.delim("FacebookNarcissism.dat", header = TRUE)

#create graph object
graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
#add visual
graph + geom_point()
#dots to triangle
graph + geom_point(shape = 17)
#chnage size
graph + geom_point(size = 6)
#change both size and shape
graph + geom_point(shape = 17, size = 6)

graph + geom_point(aes(colour = Rating_Type))

graph + geom_point(aes(colour = Rating_Type), position = "jitter")

graph + geom_point(aes(shape = Rating_Type), position = "jitter")


#scatterplots
examData <- read.delim("Exam Anxiety.dat", header = TRUE)

scatter <- ggplot(examData, aes(Anxiety, Exam))

scatter + geom_point()

scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth(method = "lm", alpha = 0.1, fill = "Blue") + labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))

scatter + geom_point() + geom_smooth(method = "lm")

scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1)

scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")

#Histograms
festivalData<-read.delim("DownloadFestival.dat", header = TRUE)

festivalHistogram<-ggplot(festivalData, aes(day1)) + theme(legend.position = "none")

festivalHistogram + geom_histogram()

festivalHistogram + geom_histogram(binwidth = 0.4)

festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival", y = "Frequency")

festivalBoxplot<-ggplot(festivalData, aes(gender, day1))

festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygene (Day 1 of Festival")

festivalData<-festivalData[order(festivalData$day1),]

festivalData

festivalData<read.delim("DownloadFestival(No Outlier).dat", header = TRUE)

festivalBoxplot<-ggplot(festivalData, aes(gender, day1))

festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygene (Day 1 of Festival")

#Density plot
density<-ggplot(festivalData, aes(day1))

density + geom_density()

density + geom_density() + labs(x = "Hygiene (Day 1 of Festival", y = "Density Estimate")

#Graphing means
chickFlick <- read.delim("ChickFlick.dat", header = TRUE)

bar <- ggplot(chickFlick, aes(film, arousal))

bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black")

bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange")

bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "film", y = "mean Arousal")

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))

bar + stat_summary(fun = mean, geom = "bar", position = "dodge")

bar + stat_summary(fun = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = .90), width = 0.2)

bar + stat_summary(fun = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = .90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")

bar <- ggplot(chickFlick, aes(film, arousal, fill = film))

bar + stat_summary(fun = mean, geom = "bar")

bar + stat_summary(fun = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

bar + stat_summary(fun = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender)

bar + stat_summary(fun = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender) + labs(x = "film, y = mean Arousal") + theme(legend.position = "none")

hiccupsData<-read.delim("Hiccups.dat", header = TRUE)

hiccups <- stack(hiccupsData)

names(hiccups) <- c("Hiccups", "Intervention")

hiccups

hiccups$Intervention_Factor <- factor(hiccups$Intervention, levels = hiccups$Intervention)

line <- ggplot(hiccups, aes(Intervention, Hiccups))

line + stat_summary(fun = mean, geom = "point")

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1))

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed")

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar")

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, colour = "Red")

line + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, colour = "Red") + labs(x = "intervention", y = "Mean Number of Hiccups")

#Graphing several independent variables

textData <-read.delim("TextMessages.dat", header = TRUE)

textData

textData$id = row(textData[1])

textMessages <- melt(textData, id = c("id", "Group"), measured = c("Baseline", "Six_months"))

names(textMessages) <-c("id", "Group", "Time", "Grammar_Score")

print(textMessages)

line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))

line + stat_summary(fun = mean, geom = "point")

line + stat_summary(fun = mean, geom = "point") +stat_summary(fun = mean, geom = "line", aes(group = Group))

line + stat_summary(fun = mean, geom = "point") +stat_summary(fun = mean, geom = "line", aes(group = Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammer Score", colour = "Group")

