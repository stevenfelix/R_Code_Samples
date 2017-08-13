# Study 1 Data cleaning and preparation
# Steven Felix
# July 2016
#
#
# Description:  These scripts are to be run on the mostly raw data downloaded from qualtrics. The majority
#               of the code is devoted to matching participants across two different data sets -- the screeners
#               that MTurkers compelted to establish eligibiligy and the study questionnaires that the eligible ones
#               completed. It's a messy process because we did not collect unique identifiers are not present in both
#               data sets.
#               The only modification of the raw data is the 1) the copy and pasting of variable names for 
#               the first 7 or 8 variables, which have their variable names on the second line instead
#               of the first, and 2) deletion of the second row, which contains question text, no data.
#               The script outputs two Rdata files: the first is a "complete" data set that has 1) the screening data merged in
#               2) the empty data rows removed (ie people who were ineligible and did not answer any questions), and
#               3) unnecessary data columns removed.  The second file is for data analysis purposes and includes
#               only scale composites, not individual items. It also has formatted and labeled factors.
# Important:    Lines for saving or writing new data sets are commented out to prevent accidental overwriting.
#               search for "save" and "write" to uncomment them (3 instances total).
#
#

rm(list = ls())
search()
library(car)
library(xlsx)

########### Opening data ------------------------------------------------------------



setwd("~/Dropbox/Research/Dissertation Project/Study 1 - PC Correlates/Data/CSVs")
data_raw <- read.csv(file = "partclean_PC MTurk Survey Data_7-05-2016.csv", header = TRUE)
#data_raw <- read.xlsx(file = "partclean_PC MTurk Survey Data_7-05-2016.xlsx", sheetIndex = 1)
names(data_raw)
dim(data_raw)
table(data_raw$PC_Mother_1) # need to make sure this gets fixed

# delete unneccessary variables
data_raw$ResponseID <- NULL
data_raw$ResponseSet <- NULL
data_raw$Name <- NULL
data_raw$ExternalDataReference <- NULL
data_raw$EmailAddress <- NULL
data_raw$Q352 <- NULL

# remove Status = 0
data_raw <- data_raw[data_raw$Status == 0,]
data_raw$Status <- NULL
dim(data_raw) # 979: removes 21 rows

# remove unfinished data
table(data_raw$Finished)
unfinished <- data_raw %>% filter(Finished == 0)
data_raw <- data_raw[data_raw$Finished ==1,]
data_raw$Finished <- NULL
dim(data_raw) # 852: removes 127 rows

# rename login variables
names(data_raw)[1:20]
names(data_raw)[which(names(data_raw) == "Login_12_TEXT")] <- "Mturk_WorkerID"
data_raw$Mturk_WorkerID <- as.character(data_raw$Mturk_WorkerID)

names(data_raw)[which(names(data_raw) == "Login_14_TEXT")] <- "EligibilityCode2"
names(data_raw)[which(names(data_raw) == "Login_6_TEXT")] <- "EligibilityCode"

# remove captcha failures, empty eligibility codes
data_raw2 <- data_raw[complete.cases(data_raw[,c("captcha1","Mturk_WorkerID","EligibilityCode","EligibilityCode2")]),]
dim(data_raw2) # removes 627 rows

# convert Eligibility Code to numeric
data_raw2$EligibilityCode <-as.numeric(levels(data_raw2$EligibilityCode))[data_raw2$EligibilityCode]

# these are the ones with NAs -- could go back and figure these out if you wanted
data_raw2[is.na(data_raw2$EligibilityCode),5:15]

# check to see that the original matches the new version
#cbind(data_raw2[,c("EligibilityCode","EligibilityCode2","Mturk_WorkerID")], data_raw2$EligibilityCode == data_raw2$EligibilityCode2)

# remove EligibilityCode with NAs (ie Codes that origininally had characters in it)
data_raw2 <- data_raw2[complete.cases(data_raw2[,c("EligibilityCode")]),]
dim(data_raw2) # removes 3

# remove invalid eligibility codes
eligible <- data_raw2[data_raw2$EligibilityCode >= 70000 & data_raw2$EligibilityCode < 80000,]
# eligible[,c("EligibilityCode","Mturk_WorkerID")]
dim(eligible) # removes 25

# by doing the 2 steps above, i am automatically taking a bunch of people out
# it is possible that some of these people actually have valid data, but did not enter
# a correct code (maybe didn't get it in time). Could merge in Eligiblity codes based on worker ID
# and then decide which to include / which are eligible

# change row names
rownames(eligible) <- 1:nrow(eligible)
dim(eligible)

# check how long it took people to complete the survey
eligible$Duration_minutes[order(eligible$Duration_minutes)]
# lots of peole with high values, consider excluding

########### Open and prepare screening data -----------------------------------------

setwd("~/Dropbox/Research/Dissertation Project/Study 1 - PC Correlates/Data/CSVs")
screener <- read.csv(file = "partclean_PC MTurk Screening Data_7-05-2016.csv", header = TRUE)
#screener <- read.xlsx(file = "partclean_PC MTurk Screening Data_7-05-2016.xlsx", sheetIndex = 1)
names(screener)

screener$ResponseID <- NULL
screener$ResponseSet <- NULL
screener$Name <- NULL
screener$ExternalDataReference <- NULL
screener$EmailAddress <- NULL
screener$Finished <- NULL
screener$LocationAccuracy <- NULL
screener$LocationLongitude <- NULL
screener$LocationLatitude <- NULL

# remove any rows with NA for participan code or relationship status
# we can't do ANYTHING with screening data without the participant code
screener <- screener[complete.cases(screener[,c("Participant.Code","rel_status")]),]

names(screener)
dim(screener)

# remove all status == 1
screener <- screener[screener$Status == 0,]
dim(screener)

########### Merging MTurk Worker IDs to Screening Data ------------------------------

# In order to accurately merge screener data, need to check for duplicates of assigned participant codes
dupIDs <- unique(screener$Participant.Code[duplicated(screener$Participant.Code)])
dupDF <- screener[screener$Participant.Code %in% dupIDs,]
dupDF <- dupDF[order(dupDF$Participant.Code),]
dim(dupDF)

# load HIT data
setwd("~/Dropbox/Research/Dissertation Project/Study 1 - PC Correlates/Data/Mturk Screening HIT data/")
hit <- read.csv("screeningHITdata.csv")
hit$X <- NULL
names(hit)
hit$WorkerId <- as.character(hit$WorkerId)
hit$Answer.surveycode <-as.numeric(levels(hit$Answer.surveycode))[hit$Answer.surveycode]

NAs <- hit[which(is.na(hit$Answer.surveycode)),]  # 3 with NA
NAs

# Check survey data to see if it contains screener survey code for these worker IDs
code1 <- eligible[eligible$Mturk_WorkerID == NAs[1,"WorkerId"],"EligibilityCode"]
code1
hit[hit$WorkerId == NAs[1,"WorkerId"],"Answer.surveycode"] <- code1 # code found in final survey data
hit[hit$WorkerId == NAs[1,"WorkerId"],]

# Worker ID in second row of NA doesn't have an eligibility code in final data
eligible[eligible$Mturk_WorkerID == NAs[2,"WorkerId"],"EligibilityCode"]

# deals with  third line of NAs DF
code2 <- eligible[eligible$Mturk_WorkerID == NAs[3,"WorkerId"],"EligibilityCode"]
code2
hit[hit$WorkerId == NAs[3,"WorkerId"],"Answer.surveycode"] <- code2 # code found in final survey data
hit[hit$WorkerId == NAs[3,"WorkerId"],]

hit[which(is.na(hit$Answer.surveycode)),]  # now only 1 with NA, rejected, not in final data
hit <- hit[complete.cases(hit[,"Answer.surveycode"]),]

# Check for duplicates  mtruk IDs in HIT data of
IDdups <- hit$WorkerId[which(duplicated(hit$WorkerId))]
IDdups # only 1:  A3J2UG22S8BIW4 
IDdupsDF <- hit[hit$WorkerId %in% IDdups,]

IDdupsDF #A3J2UG22S8BIW4 shows up twice in HIT data, providing survey codes 75273 and 76440

screener[screener$Participant.Code == 75273,] # in screener, only one person provides 75273
screener[screener$Participant.Code == 76440,] # interestingly, two people got the code 76440 in the screen
hit[hit$Answer.surveycode == 76440,] # and this echoes true in the HIT data -- two different worker ids report same eligibility code
eligible[eligible$EligibilityCode == 76440, 1:10] # in survey data, 76440 also shows up twice [good, consistent]
eligible[eligible$EligibilityCode == 75273, 1:10] # but 75273 does not, 
### suggesting that the person who shows up twice in HIT data only did study once

# Conclusions: 
# 1 person completed the screener twice (A3J2UG22S8BIW4), and got paid twice, but
# he/she didn't complete the survey twice, just once, using 76440
# 76440 also happens to be a duplicate -- someone else was assinged that during the screener
# What to do? nothing really, most of these issues will solve themselves, except
# the duplicate code 76440.  In the screening data we need
# to make sure the correct Worker ID gets assigned to the two responses using that code.

# transfer Mtruk IDs from hit data to screener
WorkerIDs <- hit$WorkerId # vcector of Worker IDs

# Note: even though there are duplicated Answer.surveycode, the following line still works
#       there end up being duplicated names for different worker IDs
names(WorkerIDs) <- hit$Answer.surveycode # assign names to IDs
names(hit)
# Note!
WorkerIDs[which(names(WorkerIDs) == "48229")] # see -- two
WorkerIDs["48229"]  # but beware, when accessing it just by subsetting, only one of the duplicated names shows up


screener$WorkerID <- WorkerIDs[as.character(screener$Participant.Code)]


########### Handling Duplicate IDs --------------------------------------------------

## because we merged based on survey codes (for which there are duplicates)
## there will now be incorrect duplicate WorkerIDs in the screening data (because it matches the first survey code)
## the next step is to go through the duplicate eligibility codes one by one
## and assign the correct worker IDs to the different worker


dups <- hit$Answer.surveycode[duplicated(hit$Answer.surveycode)]
dups <- unique(dups)
dups

# Dealing with duplicate #1
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups


# duplicate number 2
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 3
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 4
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 5
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 6
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

# this one is different from the others! they are in opposite order
screener$WorkerID[row.names(screener) == row.names(temp)[1]] <- hitdup$WorkerId[2] 

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 7 -- this is also different from others (same as #6)
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[1]] <- hitdup$WorkerId[2]

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 8
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]
screener$WorkerID[row.names(screener) == row.names(temp)[3]] <- hitdup$WorkerId[3]

### this one is a big guess, could use some more thought
## here i swap the IDs in rows 2 and 3, i could choose to leave them in recorded order.

#check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups
dups <- dups[-1]  # the last one was double?
dups

# duplicate number 9
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

# check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 10  -- THIS ONE IS WEIRD pay attention
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")] # only 1 45006
temp[,c("EndDate","Participant.Code","WorkerID")] # but 2 here, so lets just get rid of the one wiht the wrong time
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- NA

# check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 11
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[1]] <- hitdup$WorkerId[2]

# check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

# duplicate number 12
hitdup <- hit[hit$Answer.surveycode == dups[1],]
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]
screener$WorkerID[row.names(screener) == row.names(temp)[2]] <- hitdup$WorkerId[2]

# check to make sure it worked right
temp <- screener[screener$Participant.Code == dups[1],]
hitdup[,c("SubmitTime","Answer.surveycode","WorkerId")]
temp[,c("EndDate","Participant.Code","WorkerID")]

dups
dups <- dups[-1]
dups

## ok, now the screening data should have the correct worker IDs, but let's check for worker ID duplicates, just in case
## check for duplicates now of MTurk Worker IDs in screening Data
x <- screener[duplicated(screener$WorkerID),]
x <- x$WorkerID
screener[screener$WorkerID %in% x,]  
# case 18 and 86 are the same person (detected previously...case 86 is the one 
# that is included in final survey data)

# remove case 18
screener <- screener[-which(row.names(screener) == "18"),]

# Now let's deal with all the NAs for worker IDs
sum(is.na(screener$WorkerID)) # 22

# view all of the Participant codes without Worker IDs
x <- screener[is.na(screener$WorkerID),]
x[,c("WorkerID","Participant.Code")]
codes <- x$Participant.Code

# Check to see if full survey data contain any EligibilityCodes linked with missing worker IDs in screening data
y <- x[x$Participant.Code %in% eligible$EligibilityCode,c("WorkerID","Participant.Code")]
IDs <- eligible[eligible$EligibilityCode %in% y$Participant.Code,c("Mturk_WorkerID","EligibilityCode")]
IDs
# YES!!

screener$WorkerID[which(screener$Participant.Code == IDs$EligibilityCode[1])] <- IDs$Mturk_WorkerID[1]
screener$WorkerID[which(screener$Participant.Code == IDs$EligibilityCode[2])] <- IDs$Mturk_WorkerID[2]
screener$WorkerID[which(screener$Participant.Code == IDs$EligibilityCode[3])] <- IDs$Mturk_WorkerID[3]
screener$WorkerID[which(screener$Participant.Code == IDs$EligibilityCode[4])] <- IDs$Mturk_WorkerID[4]

# check to see if the above worked
x <- screener[is.na(screener$WorkerID),]
x[,c("WorkerID","Participant.Code")] # list should be shorter
y <- x[x$Participant.Code %in% eligible$EligibilityCode,c("WorkerID","Participant.Code")]
y  # should be 0
IDs <- eligible[eligible$EligibilityCode %in% y$Participant.Code,c("Mturk_WorkerID","EligibilityCode")]
IDs # should be 0

sum(is.na(screener$WorkerID)) # still 18 missing Worker IDs

# before worrying about these, let's see how many people in our survey data
# we can't match screening data for

########### Merging screening and survey data, using Worker IDs ---------------------

# may need to merge each variable from the screener individually
names(screener)

# consider removing all the cases with NA in the worker ID

sum(x$Participant.Code %in% eligible$EligibilityCode)
# none of the eligibility codes with missing IDs can be found in survey data
# so.... lets just delete them

# since all duplicates are just NAs, remove all NAs
screener <- screener[!is.na(screener$WorkerID),]

# give screener row names (which we can do since we just removed all worker ID duplicates)
row.names(screener) <- screener$WorkerID

# rename participant code in screener for merging
names(screener)[which(names(screener) == "Participant.Code")] <- "Participant.Code2"

# this person has a mistyped worker ID in survey data, this copies it from screening data
eligible[eligible$EligibilityCode == 74091, "Mturk_WorkerID"] <- screener[screener$Participant.Code == 74091, "WorkerID"]

data <- cbind(eligible,screener[as.character(eligible$Mturk_WorkerID),c("Participant.Code2","Eligible","age","rel_status","language","occupation","gender","race","occupation",
                                                                        "education","WorkerID")])

sum(!data$WorkerID == data$Mturk_WorkerID) # all worker ID's match!!
data[!data$EligibilityCode == data$Participant.Code2, c("EligibilityCode","Participant.Code2","WorkerID","Mturk_WorkerID")]
# there appears to be one unequal eligibility code, but likely mistyped

########### saving full data set ----------------------------------------------------

data[data == 99] <- NA

table(data$PC_Mother_1)
data$PC_Mother_1[data$PC_Mother_1 == 11] <- NA

table(data$PC_Other_1)
data$PC_Other_1[data$PC_Other_1 == 11] <- NA

table(data$PC_Father_1)
data$PC_Father_1[data$PC_Father_1 == 11] <- NA

table(data$PC_Friend_1)
data$PC_Friend_1[data$PC_Friend_1 == 11] <- NA


setwd("~/Dropbox/Research/Dissertation Project/Study 1 - PC Correlates/Data/Rdata")
#save(x = data, file = "completeSurveyData.Rdata")
