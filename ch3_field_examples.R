#Captain-Price-TF-141


#adding simple data
metallicaNames<-c("Lars", "James", "Kirk", "Rob")
metallicaAges<-c(47, 47, 48, 46)


#creating a datafrome from names and ages
metallica<-data.frame(Name = metallicaNames, Age = metallicaAges)


#view dataframe
metallica

#view age varible
metallica$Age

#add new childAge variable
metallica$childAge<-c(12, 12, 4, 6)

#view dataset with childAge added
metallica

#view name of the variables
names(metallica)

#add new variable fatherhoodAge based on age - childAge variables
metallica$fatherhoodAge<-metallica$Age - metallica$childAge

#view with new fatherhoodAge variable
metallica

#enter new data
name<-c("Ben", "Martin", "Andy", "Paul", "Graham", "carina", "Karina", "Doug", "Mark", "Zoe")

birth_date<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1940-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))

job<-c(1,1,1,1,1,2,2,2,2,2)

job<-gl(2, 5, labels = c("Lecturer", "Student"))

levels(job)

friends<-c(5,2,0,4,1,10,12,15,12,17)

alcohol<-c(10,15,20,5,30,25,20,16,17,18)

income<-c(20000, 40000, 35000, 22000, 50000, 5000, 100, 3000, 10000, 10)

neurotic<-c(10,17,14,13,21,7,13,9,14,13)


lecturerData<-data.frame(name, birth_date, job, friends, alcohol, income, neurotic)

lecturerData

View (lecturerData)

lecturerData<-read.delim("Lecturer Data.dat", header = TRUE)

lecturerData

View (lecturerData)

lecturerData<-read.spss("LecturerData.sav", use.value.labels = TRUE, to.data.frame = TRUE)

lecturerData

write.csv(metallica, "Metallica Data.csv")

write.table(metallica, "Metallica Data.txt", sep="\t", row.names = FALSE)

lecturerPersonality <-lecturerData[, c("friends", "alcohol", "neurotic")]

lecturerPersonality

lecturerOnly <- subset(lecturerData, job=="Lecturer")

lecturerOnly

alcoholPersonality <- subset(lecturerData, alcohol > 10, select = c("friends", "alcohol", "neurotic"))

alcoholPersonality

alcoholPersonalityMatrix <- as.matrix(alcoholPersonality)

alcoholPersonalityMatrix <- as.matrix(lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")])

alcoholPersonalityMatrix

satisfactionData = read.delim("Honeymoon Period.dat", header = TRUE)

satisfactionData

satisfactionStacked <- stack(satisfactionData, select = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))

satisfactionStacked


install.packages("reshape")
library(reshape)

install.packages("reshape2")
library(reshape2)

restructureData<-melt(satisfactionData, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))

restructureData
#or
View(restructureData)

wildData<-cast(restructureData, Person + Gender ~ variable, value = "value")


wildData

