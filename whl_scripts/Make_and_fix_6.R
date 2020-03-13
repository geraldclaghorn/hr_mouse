# fix resistances + make clone
# Made by Gerald Claghorn 3 February 2015
# version 3 by Jarren, Zoe and Marcell 18 August 2015 (picking breeders 78)
# 3 batches, by Layla 28 Oct 2016

# To run this file
# 1. [no longer true, just keep .xls file] Make the g__wn.csv file from the g__wn.xls file
# 2. Find and replance generation (10 replacements)
# 3. Run the whole script
# 4. Identify problems from output and fix them, using this syntax or editing manually in the wean file
#    A. Check the data book to look for any typos from data book to excel file (wean.xls and each batch)
#    B. When correcting data files, highlight the cell and leave a comment (with your name and date)
#    C. If you're uncertain about problems, consult someone with experience on how to do exclusions

# This file takes g__run.csv from the merge_whl_running syntax and the g__res.csv from the merge_resistances syntax
# as well as the g__wn.csv file which is converted from an .xls file using excel
# and combines them to yield one file with all of the resistance measurements together and labled.

# The resulting file (g__ALL.csv) should be the same length and one column wider than the
# initial dimensions that result from merging g__run, g__res, g__wn

library("xlsx")

# Read in run and resistance
setwd("D:/SELECT/Sel85/Analyze")
RUN <- read.xlsx("g85run.xlsx",1)
RES <- read.csv("g85res.CSV")
RUNRESNA <- merge(RUN, RES, all=TRUE)
# Delete wheels with no mouse
RUNRES <- RUNRESNA[!is.na(RUNRESNA$MouseID),]

# Read in wean data
setwd("d:/SELECT/Sel85/wean/")
WEAN <- read.xlsx("g85wn.xls",1,startRow=10)
View(WEAN)
ALL <- merge(WEAN, RUNRES, all=TRUE)

#### THIS SECTION CHECKS FOR MISTAKES
#### IF ANY LISTS ARE PRINTED IN THIS SECTION, IT COULD INDICATE A TYPO OR OTHER MISTAKE


# List any duplicate MouseIDs (Values of duplicate ID will print here, and you will have an error if they are not fixed)
if(length(ALL[duplicated(ALL$MouseID) & ALL$MouseID != -9, 1]) >0 ) 
  ALL[duplicated(ALL$MouseID) & ALL$MouseID != -9, 1] else "No duplicate MouseIDs"

# Convert user missing values (-9) to r-compatible missing values (NA)
ALL[ALL==-9] <- NA
# Delete NA mouseIDs from ALL file
#ALL <- ALL[!is.na(ALL$MouseID),]

# Break the file into smaller chunks that will be easier to work with 
NOTWEAN <- ALL[is.na(ALL$MouseID), ]  #Keep the -9 mice aside for later, we don't need to do anything more for them
ALLWEAN <- ALL[!is.na(ALL$MouseID), ]
# Assign MouseID to row names (will not work if duplicates exist)
rownames(ALLWEAN) <- ALLWEAN[, "MouseID"]
WEANNOTRAN <- ALLWEAN[is.na(ALLWEAN$MassOn),]
ALLRAN <- ALLWEAN[!is.na(ALLWEAN$MassOn),]

# List out your exclusions
excludeall <- c("87896", "87076","87430", "87675", "871956", "87794", "87804", "87639", "87678", "87887", "87066", "87915", "87391", "87610", "87101", "87405", "87780",
                "87228","87725","87157","87678","87249","87841","87532","871152","87579","87563","87789","87405", "87363")
exclude1 <- c("87169", "87210", "87676", "87828", "87253","87075","87076","87893","87194","87422","87676",
              "87062","87011",
              "87992","87988","87179","87804","87059")
exclude2 <- c("87636", "87828", "87253","87075","87076","87893","87422","87676","87824","87893","87321","87422",
              "87059","87603")
exclude3 <- c("87828", "87253","87075","87076","87893","87896",
              "87179","87804")
exclude4 <- c("87828","87075","87076","87896",
              "87804","87979","87804")
exclude5 <- c("87075","87824", "87136","87216","87459","87548",
              "87780","87804")
exclude6 <- c("87075",
              "87915","87780")

# Make lists of variables associated with days.
day1 <- c("Run1", "Int1", "Max1", "Min1", "RPM1")
day2 <- c("Run2", "Int2", "Max2", "Min2", "RPM2")
day3 <- c("Run3", "Int3", "Max3", "Min3", "RPM3")
day4 <- c("Run4", "Int4", "Max4", "Min4", "RPM4")
day5 <- c("Run5", "Int5", "Max5", "Min5", "RPM5")
day6 <- c("Run6", "Int6", "Max6", "Min6", "RPM6")

# Uses the exclusions list to take out the bad values
for (i in excludeall) ALLRAN[i, c(day1, day2, day3, day4, day5, day6)] <- NA
for (i in exclude1) ALLRAN[i, day1] <- NA
for (i in exclude2) ALLRAN[i, day2] <- NA
for (i in exclude3) ALLRAN[i, day3] <- NA
for (i in exclude4) ALLRAN[i, day4] <- NA
for (i in exclude5) ALLRAN[i, day5] <- NA
for (i in exclude6) ALLRAN[i, day6] <- NA



# Look for disagreement between data from weaning and from wheels

#  Mismatched sex (Values of mismatches will print here, and you will be warned if they stil exist at the bottom of the file)
MISMATCHSEX <- ALLRAN[(ALLRAN$Sexwh != ALLRAN$Sex),]
if (nrow(MISMATCHSEX) > 0) subset(MISMATCHSEX, select=c("MouseID", "Sex", "Sexwh")) else "No mismatched sex"

#  Mismatched line (Values of mismatches will print here, and you will be warned if they stil exist at the bottom of the file)
MISMATCHLINE <- ALLRAN[(ALLRAN$Linewh != ALLRAN$Line),]
if (nrow(MISMATCHLINE) > 0) subset(MISMATCHLINE, select=c("MouseID", "Line", "Linewh")) else "No mismatched line"

#  Extraordinary mass gain or loss (Values of mass checks will print here, and you will be warned if they stil exist at the bottom of the file)
## THIS IS GIVING EXTRA NA's AND I CANT FIGURE OUT WHERE THEY ARE COMING FROM
MASSCHANGE <- ALLRAN[((ALLRAN$MassOff/ALLRAN$MassOn) > 1.25 | (ALLRAN$MassOff/ALLRAN$MassOn) < 0.75),]
if (nrow(MASSCHANGE[!is.na(MASSCHANGE$MouseID),]) > 0) 
    subset(MASSCHANGE[!is.na(MASSCHANGE$MouseID),], 
    select=c("MouseID", "MassOn", "MassOff","Batch","Wheel","Line","Sex")) else "No extraordinary mass change"

# These were outliers from Masson:massoff
ALLRAN[which(ALLRAN$MouseID==87083),"MassOff"] <- NA #dehydrated
ALLRAN[which(ALLRAN$MouseID==87405),"MassOff"] <- NA #dehydrated
ALLRAN[which(ALLRAN$MouseID==87748),"MassOff"] <- NA #dehydrated
ALLRAN[which(ALLRAN$MouseID==87788),"MassOff"] <- NA #dehydrated
ALLRAN[which(ALLRAN$MouseID==87824),"MassOff"] <- NA #dehydrated

# Look for outliers visually
plot(ALLRAN$RES1, ALLRAN$RES2)
plot(ALLRAN$RES3, ALLRAN$RES4)
plot(ALLRAN$RES5, ALLRAN$RES6)
plot(ALLRAN$RES7, ALLRAN$RES8)

# Look for outliers by ratios
RESCK12 <- ALLRAN[((ALLRAN$RES1/ALLRAN$RES2) > 2 | (ALLRAN$RES1/ALLRAN$RES2) < .5) & ALLRAN$Batch == 1,]
RESCK34 <- ALLRAN[((ALLRAN$RES3/ALLRAN$RES4) > 2 | (ALLRAN$RES3/ALLRAN$RES4) < .5) & (ALLRAN$Batch == 1 | ALLRAN$Batch == 2),]
RESCK56 <- ALLRAN[((ALLRAN$RES5/ALLRAN$RES6) > 2 | (ALLRAN$RES5/ALLRAN$RES6) < .5) & (ALLRAN$Batch == 2 | ALLRAN$Batch == 3),]
RESCK78 <- ALLRAN[((ALLRAN$RES7/ALLRAN$RES8) > 2 | (ALLRAN$RES7/ALLRAN$RES8) < .5) & (ALLRAN$Batch == 3 | ALLRAN$Batch == 4),]

# Values of resistance checks will print here, and you will be warned if they stil exist at the bottom of the file
# RES1/RES2  > 2 or RES1/RES2  < 0.5
if (nrow(RESCK12[!is.na(RESCK12$MouseID),]) > 0) subset(RESCK12[!is.na(RESCK12$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES1", "RES2", "RES3", "RES4")) else "No RESCK12 disagreement"
# RES3/RES4  > 2 or RES3/RES4  < 0.5
if (nrow(RESCK34[!is.na(RESCK34$MouseID),]) > 0) subset(RESCK34[!is.na(RESCK34$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES1", "RES2", "RES3", "RES4", "RES5", "RES6")) else "No RESCK34 disagreement"
# RES5/RES6  > 2 or RES5/RES6  < 0.5
if (nrow(RESCK56[!is.na(RESCK56$MouseID),]) > 0) subset(RESCK56[!is.na(RESCK56$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES3", "RES4", "RES5", "RES6", "RES7", "RES8")) else "No RESCK56 disagreement"
# RES7/RES8  > 2 or RES7/RES8  < 0.5
if (nrow(RESCK78[!is.na(RESCK78$MouseID),]) > 0) subset(RESCK78[!is.na(RESCK78$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES5", "RES6", "RES7", "RES8")) else "No RESCK78 disagreement"

# List out your exclusions for resistance
excludeRES1 <- c("87091","87121","87252")
excludeRES2 <- c()
excludeRES3 <- c("87241","87328","87460","87569","87688","87780","87099")
excludeRES4 <- c("87075","87204","87841","871152")
excludeRES5 <- c("87066","87219","87341","87533","87887","87847")
excludeRES6 <- c("87099","87532","87568","87695")
excludeRES7 <- c("87053","87799","87830","87849")
excludeRES8 <- c("87219","87881")



# Uses the exclusions list to take out the bad values
for (i in excludeRES1) ALLRAN[i, "RES1"] <- NA
for (i in excludeRES2) ALLRAN[i, "RES2"] <- NA
for (i in excludeRES3) ALLRAN[i, "RES3"] <- NA
for (i in excludeRES4) ALLRAN[i, "RES4"] <- NA
for (i in excludeRES5) ALLRAN[i, "RES5"] <- NA
for (i in excludeRES6) ALLRAN[i, "RES6"] <- NA
for (i in excludeRES7) ALLRAN[i, "RES7"] <- NA
for (i in excludeRES8) ALLRAN[i, "RES8"] <- NA

# ALL OF THIS IS REPEATED FROM ABOVE, BUT SHOULD RESULT IN NO VALID CASES IF RES HAS BEEN FIXED
RESCK12 <- ALLRAN[((ALLRAN$RES1/ALLRAN$RES2) > 2 | (ALLRAN$RES1/ALLRAN$RES2) < .5) & ALLRAN$Batch == 1,]
RESCK34 <- ALLRAN[((ALLRAN$RES3/ALLRAN$RES4) > 2 | (ALLRAN$RES3/ALLRAN$RES4) < .5) & (ALLRAN$Batch == 1 | ALLRAN$Batch == 2),]
RESCK56 <- ALLRAN[((ALLRAN$RES5/ALLRAN$RES6) > 2 | (ALLRAN$RES5/ALLRAN$RES6) < .5) & (ALLRAN$Batch == 2 | ALLRAN$Batch == 3),]
RESCK78 <- ALLRAN[((ALLRAN$RES7/ALLRAN$RES8) > 2 | (ALLRAN$RES7/ALLRAN$RES8) < .5) & (ALLRAN$Batch == 3 | ALLRAN$Batch == 4),]
if (nrow(RESCK12[!is.na(RESCK12$MouseID),]) > 0) subset(RESCK12[!is.na(RESCK12$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES1", "RES2", "RES3", "RES4")) else "No RESCK12 disagreement"
if (nrow(RESCK34[!is.na(RESCK34$MouseID),]) > 0) subset(RESCK34[!is.na(RESCK34$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES1", "RES2", "RES3", "RES4", "RES5", "RES6")) else "No RESCK34 disagreement"
if (nrow(RESCK56[!is.na(RESCK56$MouseID),]) > 0) subset(RESCK56[!is.na(RESCK56$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES3", "RES4", "RES5", "RES6", "RES7", "RES8")) else "No RESCK56 disagreement"
if (nrow(RESCK78[!is.na(RESCK78$MouseID),]) > 0) subset(RESCK78[!is.na(RESCK78$MouseID),], select=c("MouseID", "Wheel", "Batch", "RES5", "RES6", "RES7", "RES8", "RES9", "RES10")) else "No RESCK78 disagreement"

## Calculate resistance for each mouse according to batch
RES <- ALLRAN$RES8*NA
ALLRANRES <- cbind(ALLRAN, RES)

b1IDs <- as.character(ALLRANRES[ALLRANRES$Batch == 1, "MouseID"])
b2IDs <- as.character(ALLRANRES[ALLRANRES$Batch == 2, "MouseID"])
b3IDs <- as.character(ALLRANRES[ALLRANRES$Batch == 3, "MouseID"])

b1IDs <- b1IDs[!is.na(b1IDs)]
b2IDs <- b2IDs[!is.na(b2IDs)]
b3IDs <- b3IDs[!is.na(b3IDs)]

# This results in NA for all that were made to have exclusions
for (i in b1IDs) ALLRANRES[i, "RES"] <- (sum(ALLRANRES[i, "RES1"], ALLRANRES[i, "RES2"], ALLRANRES[i, "RES3"], ALLRANRES[i, "RES4"], na.rm = TRUE)/
  (1*(!is.na(ALLRANRES[i, "RES1"])) + 1*(!is.na(ALLRANRES[i, "RES2"])) + 1*(!is.na(ALLRANRES[i, "RES3"])) + 1*(!is.na(ALLRANRES[i, "RES4"]))))
for (i in b2IDs) ALLRANRES[i, "RES"] <- (sum(ALLRANRES[i, "RES3"], ALLRANRES[i, "RES4"], ALLRANRES[i, "RES5"], ALLRANRES[i, "RES6"], na.rm = TRUE)/
  (1*(!is.na(ALLRANRES[i, "RES3"])) + 1*(!is.na(ALLRANRES[i, "RES4"])) + 1*(!is.na(ALLRANRES[i, "RES5"])) + 1*(!is.na(ALLRANRES[i, "RES6"]))))
for (i in b3IDs) ALLRANRES[i, "RES"] <- (sum(ALLRANRES[i, "RES5"], ALLRANRES[i, "RES6"], ALLRANRES[i, "RES7"], ALLRANRES[i, "RES8"], na.rm = TRUE)/
  (1*(!is.na(ALLRANRES[i, "RES5"])) + 1*(!is.na(ALLRANRES[i, "RES6"])) + 1*(!is.na(ALLRANRES[i, "RES7"])) + 1*(!is.na(ALLRANRES[i, "RES8"]))))

# Make a list of MouseIDs within each batch and room
b1ABIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 1 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel <= 100, "MouseID"])
b2ABIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 2 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel <= 100, "MouseID"])
b3ABIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 3 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel <= 100, "MouseID"])
b1CDIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 1 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel > 100, "MouseID"])
b2CDIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 2 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel > 100, "MouseID"])
b3CDIDs <- as.character(ALLRANRES[ALLRANRES$Batch == 3 & !is.na(ALLRAN$MouseID) & ALLRAN$Wheel > 100, "MouseID"])

# Calculate RES based on batch

for (i in b1ABIDs) ALLRANRES[i, "BATROOM"] <- 1
for (i in b2ABIDs) ALLRANRES[i, "BATROOM"] <- 1
for (i in b3ABIDs) ALLRANRES[i, "BATROOM"] <- 1
for (i in b1CDIDs) ALLRANRES[i, "BATROOM"] <- 2
for (i in b2CDIDs) ALLRANRES[i, "BATROOM"] <- 2
for (i in b3CDIDs) ALLRANRES[i, "BATROOM"] <- 2

# Calculate wheel start year
ALLRANRES[, "whlyear"] <- 2018

# Calculate wheel start month
ALLRANRES[,"whlmo"] <- 03

# Calculate wheel start day
for (i in b1ABIDs) ALLRANRES[i, "whlday"] <- 05
for (i in b2ABIDs) ALLRANRES[i, "whlday"] <- 12
for (i in b3ABIDs) ALLRANRES[i, "whlday"] <- 19
for (i in b1CDIDs) ALLRANRES[i, "whlday"] <- 05
for (i in b2CDIDs) ALLRANRES[i, "whlday"] <- 12
for (i in b3CDIDs) ALLRANRES[i, "whlday"] <- 19

# Put the new variables "RES" and "BATCHROOM with the rest of the variables
RESwithIDs <- subset(ALLRANRES, select=c("MouseID", "RES", "BATROOM","whlyear","whlmo","whlday"))
ALLNORES <- rbind(ALLRAN, WEANNOTRAN, NOTWEAN)
ALL2 <- merge(ALLNORES, RESwithIDs, all = TRUE)


ALL3 <- ALL2[,c("Wnorder","MouseID","Sex","WnMass","DamID","DamMass","SireID","Line","Linetype",
                "WnDay","WnMonth","WnYear","Bday","Bmonth","Byear","SepDay","SepMonth","SepYear","Breeder","Fate","Observ",
                "Wheel","Batch","Sexwh","Linewh","MassOn","MassOff","WhlNotes",
                "Run1","Run2","Run3","Run4","Run5","Run6",
                "Int1","Int2","Int3","Int4","Int5","Int6",
                "Max1","Max2","Max3","Max4","Max5","Max6",
                "Min1","Min2","Min3","Min4","Min5","Min6",
                "RPM1","RPM2","RPM3","RPM4","RPM5","RPM6",
                "RES1","RES2","RES3","RES4","RES5","RES6","RES7","RES8","RES",
                "BATROOM","whlyear","whlmo","whlday")]

### Exclusions based on outliers
# these checks are from breed syntax (to make breeder list and look document)
# but we are dealing with them here to make a corrected all file.
# These were outliers based on run5:run6 graphs

ALLRAN <- ALL3[!is.na(ALL3$MassOff) & (!is.na(ALL3$Run5 | ALL3$Run6)),]
rownames(ALLRAN) <- ALLRAN[, "MouseID"]

# Check for mice that ran much more or much less on night 6 thanon night 5
# Not every mouse on this list should be excluded, this is just a check
Bigchange5to6 <- ALLRAN[(ALLRAN$Run6/ALLRAN$Run5) > 2.5 | (ALLRAN$Run6/ALLRAN$Run5) < 0.4,]
subset(Bigchange5to6[!is.na(Bigchange5to6$MouseID),], select=c("MouseID","Line", "Wheel", "Batch", "Run1", "Run2", "Run3", "Run4", "Run5", "Run6"))

# Exclude values based on large changes from day 5 to day 6 (according to Careau et al 2013)
# And we have reason to think that it is not due to natural variation
exclude5 <- c("87988")
exclude6 <- c("87363","87684","87792")
# More exclusions based on Breed syntax; MC - Go through Breed.R first, then come back if you have "INF" problem with ratios dividing by zeros. Find the mouseid for the offenders and list them below. 
exclude5 <- c("87850")
exclude6 <- c()

# Uses the exclusions list to take out the bad values
for (i in exclude5) ALL3[which(ALL3$MouseID==i), day5] <- NA
for (i in exclude6) ALL3[which(ALL3$MouseID==i), day6] <- NA


# ALERT USER TO UNFIXED PROBLEMS
resstatus <- if (nrow(RESCK12[!is.na(RESCK12$MouseID),]) > 0){"WARNING: RESCK disagreement still exists"
}else if (nrow(RESCK34[!is.na(RESCK34$MouseID),]) > 0){"WARNING: RESCK disagreement still exists"
}else if (nrow(RESCK56[!is.na(RESCK56$MouseID),]) > 0){"WARNING: RESCK disagreement still exists"
}else if (nrow(RESCK78[!is.na(RESCK78$MouseID),]) > 0){"WARNING: RESCK disagreement still exists"
}else "Resistance is ok"
duplicatestatus <- if (nrow(ALLWEAN[ALLWEAN$MouseID == ALLWEAN[duplicated(ALLWEAN$MouseID) & ALLWEAN$MouseID != -9, 1], 1:9]) > 0) "WARNING: Duplicate MouseIDs" else "No duplicate MouseIDs"
sexmatchstatus <- if (nrow(MISMATCHSEX) > 0) "WARNING: Mismatched sex" else "No mismatched sex"
linematchstatus <- if (nrow(MISMATCHLINE) > 0) "WARNING: Mismatched line" else "No mismatched line"
MASSCHANGE <- ALL3[((ALL3$MassOff/ALL3$MassOn) > 1.25 | (ALL3$MassOff/ALL3$MassOn) < 0.75),]
if (nrow(MASSCHANGE[!is.na(MASSCHANGE$MouseID),]) > 0) 
  subset(MASSCHANGE[!is.na(MASSCHANGE$MouseID),], 
         select=c("MouseID", "MassOn", "MassOff")) else "No extraordinary mass change"
masschangestatus <- if (nrow(MASSCHANGE[!is.na(MASSCHANGE$MouseID),]) > 0) "WARNING: Extraordinary mass change" else "No extraordinary mass change"
rbind(resstatus, duplicatestatus, sexmatchstatus, linematchstatus, masschangestatus)

all_wnorder <- ALL3[order(ALL3$Wnorder),]

#write file
all_wnorder<- write.table(all_wnorder, file="D:/SELECT/Sel85/Analyze/g85ALL2.csv",sep=",",na="-9", row.names=FALSE)
#Added CSV read in and XLS output to fix problem while reading in variables when importing into SPSS (some variables would be
#recognized as string and despite clearly being numeric) - NS 11/06/2018
all_wnorder2 <- read.csv("D:/SELECT/Sel85/Analyze/g85ALL2.csv")
write.xlsx(all_wnorder2, file="D:/SELECT/Sel85/Analyze/g85ALL2.xls", sheetName = "Sheet1", 
          col.names = TRUE, row.names = FALSE, append = FALSE)

#Check all file for extra -9's at bottom. If -9's present, look at observations of various files in global environment
# to see if they match realistic numbers. -NS 11/06/2018