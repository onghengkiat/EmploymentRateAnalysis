library("xlsx")
#Set working directory
setwd("C:/Users/user/Documents/R/Employment_Rate_Analysis_WebApp")

#Read the datasets into Rstudio
data <- read.xlsx("data/raw_dataset.xlsx",sheetIndex = 1,header = TRUE)

#Analysis the data
View(data)
class(data) #check typeof class 
dim(data) #check row & column
str(data) #Check the structure of the datasets
summary(data)


###Cleaning process

#Convert Data Type 
data$NA.<- NULL  #remove entire column 1
data$Employed <- as.logical(as.integer(as.character(data$Employed)))
data$Mental.Illness <- as.logical(as.integer(as.character(data$Mental.Illness)))
data$Education <- as.character(data$Education)
data$Have.Computer <- as.logical(as.integer(as.character(data$Have.Computer)))
data$Legally.Disabled <- as.logical(as.integer(as.character(data$Legally.Disabled)))
data$Internet.Access <- as.logical(as.integer(as.character(data$Internet.Access)))
data$Live.With.Parents <- as.logical(as.integer(as.character(data$Live.With.Parents)))
data$Study <- as.logical(as.integer(as.character(data$Study)))
data$Received.Food.Stamps <- as.logical(data$Received.Food.Stamps)
data$Section.8.Housing <- as.logical(data$Section.8.Housing)
data$Lack.of.concentration <- as.logical(data$Lack.of.concentration)
data$Anxiety <- as.logical(as.integer(as.character(data$Anxiety)))
data$Depression <- as.logical(data$Depression)
data$Obsessive.thinking <- as.logical(data$Obsessive.thinking)
data$Mood.swings <- as.logical(data$Mood.swings)
data$Panic.attacks <- as.logical(data$Panic.attacks)
data$Compulsive.behavior <- as.logical(data$Compulsive.behavior)
data$Tiredness <- as.logical(as.integer(as.character(data$Tiredness)))

#convert data type
data$Age <- as.character(data$Age)
data$Gender <- as.character(data$Gender)
data$Household.Income <- as.character(data$Household.Income)
data$Region <- as.character(data$Region)
data$Device.Type <- as.character(data$Device.Type)


# Remove duplicate data rows
data <- unique(data)

#Make all lowercase in variable Education
data$Education <- tolower(data$Education)


###Replacing Na / Null value

#Check number of null data before removing
sum(is.na(data))

#Method 1
#Replace all null value with Mode by refering to summary()
summary(data)
data$Have.Computer[is.na(data$Have.Computer)] <- TRUE
data$Legally.Disabled[is.na(data$Legally.Disabled)] <- FALSE
data$Internet.Access[is.na(data$Internet.Access)] <- TRUE
data$Live.With.Parents[is.na(data$Live.With.Parents)] <- FALSE
data$Anxiety[is.na(data$Anxiety)]<-FALSE
data$Tiredness[is.na(data$Tiredness)] <- FALSE

#Method 2
#Simply delete column with null value 
#data <- na.omit(data) 

#Check number of null data after removing
sum(is.na(data))


#Analysis the data
str(data)
dim(data) 
summary(data)


#List the name of column
colnames(data) 


#Renaming coloum
names(data)[names(data) == "Mental.Illness" ] <- "Mental Illness"
names(data)[names(data) == "Education" ] <- "Education"
names(data)[names(data) == "Have.Computer" ] <- "Own Computer"
names(data)[names(data) == "I.have.been.hospitalized.before.for.my.mental.illness" ] <- "Hospitalized"
names(data)[names(data) == "Days.Hopitalized.for.Mental.illeness" ] <- "Days Hospitalized for Mental Illness"
names(data)[names(data) == "Legally.Disabled" ] <- "Disabled"
names(data)[names(data) == "Internet.Access" ] <- "Internet Access"
names(data)[names(data) == "Live.With.Parents" ] <- "Live With Parents"
names(data)[names(data) == "I.have.a.gap.in.my.resume" ] <- "GapInResume"
names(data)[names(data) == "Gap.In.Resume..Months." ] <- "Gaps In Resume(in a Month)"
names(data)[names(data) == "Total.Annual.Income..USD." ] <- "Annual Income (USD)"
names(data)[names(data) == "Annual.income.from.social.welfare.programs" ] <- "Annual Income from Social Welfare programs (USD)"
names(data)[names(data) == "Received.Food.Stamps" ] <- "Receive Food Stamps"
names(data)[names(data) == "Section.8.Housing" ] <- "Section 8 Housing"
names(data)[names(data) == "Frequency.Hospitalized" ] <- "Frequency Hospitalized"
names(data)[names(data) == "Lack.of.concentration" ] <- "Lack of Concentration"
names(data)[names(data) == "Obsessive.thinking" ] <- "Obsessive Thinking"
names(data)[names(data) == "Mood.swings" ] <- "Mood Swings"
names(data)[names(data) == "Panic.attacks" ] <- "Panic Attacks"
names(data)[names(data) == "Compulsive.behavior" ] <- "Compulsive Behavior"
names(data)[names(data) == "Household.Income" ] <- "Household Income"
names(data)[names(data) == "Device.Type" ] <- "Device Type"

#Write new datasets (after cleaning) to new xlsx file
write.xlsx(data,"CleanedData.xlsx",sheetName = "Sheet1" , col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)







































