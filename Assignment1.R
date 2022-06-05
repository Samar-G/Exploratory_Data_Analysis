#Samar SalahElDin Ghoneim 20188029
#Mostafa Ahmed Mohamed 20188049

library(ggplot2)

# 1
classes <- c("numeric","numeric","factor","character","character","numeric","factor","factor","factor","factor","factor","factor","factor", "factor")

mainData <- read.table("MedicalAppointments.csv", header = TRUE, sep = "," ,  colClasses = classes)

# 2.1
table(mainData$Neighbourhood)

# 2.2
hist(mainData$PatientId,col = "2",xlab = "PatientID")
#hist(log2(mainData$PatientId+1),col = "2",xlab = "PatientID")

# 2.3
# with filtering the 0,-1 value 
filtr <- mainData[mainData$Age >0, "Age"]
plot(density(filtr),col = "6")
#plot(density(log2(filtr+1)))

# without filtering the 0,-1 value
plot(density(mainData$Age),col = "5")
#plot(density(log2(mainData$Age+1)))

# 2.4
sum(is.na(mainData))
sum(rowSums(is.na(mainData)))
colSums(is.na(mainData))

# 2.5
#To get the number of values that are invalid
ageSum <- sum(ifelse(mainData$Age > 0, 0, 1))
genderSum <- sum(ifelse(mainData$Gender %in% c("M", "F"), 0, 1))
showSum <- sum(ifelse(mainData$No.show %in% c("Yes", "No"), 0, 1))
scholarSum <- sum(ifelse(mainData$Scholarship %in% c(0, 1), 0, 1))
hiperSum <- sum(ifelse(mainData$Hipertension %in% c(0, 1), 0, 1))
alcoholSum <- sum(ifelse(mainData$Alcoholism %in% c(0, 1), 0, 1))
diaSum <- sum(ifelse(mainData$Diabetes %in% c(0, 1), 0, 1))
handSum <- sum(ifelse(mainData$Handcap %in% c(0, 1), 0, 1))
smsSum <- sum(ifelse(mainData$SMS_received %in% c(0, 1, 2), 0, 1))
patIdSum <- sum(ifelse(mainData$PatientId > 0, 0, 1))
appIdSum <- sum(ifelse(mainData$AppointmentID > 0, 0, 1))

#To get the invalid values of each column
ageCol <- unique(mainData[mainData$Age <= 0, "Age"])
genderCol <- unique(mainData[!(mainData$Gender %in% c("M", "F")), "Gender"])
showCol <- unique(mainData[!(mainData$No.show %in% c("Yes", "No")), "No.show"])
scholarCol <- unique(mainData[!(mainData$Scholarship %in% c(0, 1)), "Scholarship"])
hiperCol <- unique(mainData[!(mainData$Hipertension %in% c(0, 1)), "Hipertension"])
alcoholCol <- unique(mainData[!(mainData$Alcoholism %in% c(0, 1)), "Alcoholism"])
diaCol <- unique(mainData[!(mainData$Diabetes %in% c(0, 1)), "Diabetes"])
handCol <- unique(mainData[!(mainData$Handcap %in% c(0, 1)), "Handcap"])
smsCol <- unique(mainData[!(mainData$SMS_received %in% c(0, 1, 2)), "SMS_received"])
patIdCol <- unique(mainData[mainData$PatientId <= 0, "PatientId"])
appIdCol <- unique(mainData[mainData$AppointmentID <= 0, "AppointmentID"])

cat(
  "Age:", ageSum, "error values:",ageCol, "\n", "Gender:", genderSum,"error values:",genderCol, "\n",
  "No.show:", showSum, "error values:",showCol, "\n","Scholarship:", scholarSum, "error values:",scholarCol, "\n",
  "Hipertension:", hiperSum, "error values:",hiperCol, "\n","Alcoholism:", alcoholSum, "error values:",alcoholCol, "\n",
  "Diabetes:", diaSum, "error values:",diaCol, "\n","Handcap:", handSum, "error values:",handCol, "\n",
  "SMS_received:", smsSum, "error values:",smsCol, "\n","PatientId:", patIdSum, "error values:",patIdCol, "\n",
  "AppointmentID:", appIdSum, "error values:",appIdCol
)

# 2.6
ageM <- mainData[mainData$Gender=="M", "Age"]
ageF <- mainData[mainData$Gender=="F", "Age"]

par(mfrow = c(1,2))
plot(table(ageF), type = "l", col = "purple",bty = 'n')
plot(table(ageM), type = "l", col = "blue",bty = 'n')
par(mfrow = c(1,1))

# plot(density(log2(ageF+1)), type = "l", col = "purple",pch = 2,bty = 'n')
# lines(density(log2(ageM+1)), col = "black",pch = 2)

# par(mfrow = c(1,2))
# plot(density(ageF), type = "l", col = "purple",bty = 'n')
# plot(density(ageM), type = "l", col = "black",bty = 'n')
# par(mfrow = c(1,1))


# 2.7
yesVisits = subset(mainData, No.show == "Yes")

yesMean <- function(i)
{
  res <- i/sum(x[, "Yes"])
  return(res)
}

patient <- yesVisits[, "PatientId"]
x <-table(patient, yesVisits$No.show)
avgVisits <- sapply(x[,"Yes"], yesMean)

#table(avgVisits) #It collects similar numbers of avg 


# 2.8
ageRange <- cut(mainData$Age,
                breaks = c(1,12,18,40,120),
                include.lowest = TRUE)

levels(ageRange) = c("Children","Teenagers", "Adults", "Elderlies")

tVisits <- table(ageRange, mainData$No.show)
maxVists = which.max(tVisits[, "Yes"])

cat(labels(maxVists), "\n", tVisits[maxVists, "Yes"])

# children = subset(yesVisits, Age %in% c(1:12))
# nrow(children)


# 2.9
maxAbs = which.max(tVisits[,"No"])
cat(labels(maxAbs), "\n", tVisits[maxAbs, "No"])


# 2.10
appDates <- as.Date(yesVisits$AppointmentDay)
yRange <- as.numeric(table(months(appDates)))
monthFact = as.factor(unique(sort(months(appDates))))
ggplot()+
  geom_bar(stat = "identity", fill = "pink",
           mapping = aes(x = monthFact, y= yRange))+
  labs(y = "Frequency", x = "Months")
