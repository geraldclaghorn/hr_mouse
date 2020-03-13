# Breed.run clone 
# Originally made by Gerald Claghorn on 3 February, 2015
# version 6 by Layla Hiramatsu 14 July 2016
# took out Line 9, by Layla 28 Oct 2016
# version 7 by Layla Hiramatsu, added prompt for directory 16 May 2017

# To run this file
# 1. Find and replance generation gXX
# 2. Run to Pass 1, change lm() run and res variables according to best transformations
# 3. Run to pass 2, exclude outliers if necessary
# 4. Run the rest, re-format Look file and print to give to Ted

# Load packages
#install.packages("plyr")
#install.packages("arm")
#install.packages("moments")
#install.packages("ggplot2")
library(plyr)
library(MASS)
library(arm)
library(moments)
library(ggplot2)
library(xlsx)


# Read in run and resistance
wd <- "D:/SELECT/Sel85/Analyze/"
setwd(wd)
ALL <- read.csv("g85ALL2.csv")

ALL[ALL==-9] <- NA

# Keep only the mice that had real MassOff values and at least one of Run5 or Run6
ALLRAN <- ALL[!is.na(ALL$MassOff) & (!is.na(ALL$Run5 | ALL$Run6)),]
rownames(ALLRAN) <- ALLRAN[, "MouseID"]

# MC - Once you have found "INF" values for ratios dividing by zeros, you will want to run those mice back into make and fix to be exluded in the appropriate line.
# Moved this block of code to Make_and_fix 14 July 2016 - Layla
# Check for mice that ran much more or much less on night 6 thanon night 5
# Not every mouse on this list should be excluded, this is just a check
# Bigchange5to6 <- ALLRAN[(ALLRAN$Run6/ALLRAN$Run5) > 2.5 | (ALLRAN$Run6/ALLRAN$Run5) < 0.4,]
# subset(Bigchange5to6[!is.na(Bigchange5to6$MouseID),], select=c("MouseID","Line", "Wheel", "Batch", "Run1", "Run2", "Run3", "Run4", "Run5", "Run6"))
# Exclude values based on large changes from day 5 to day 6 (according to Careau et al 2013)
# And we have reason to think that it is not due to natural variation
# Exclude values of 0 from specified mice
# exclude5 <- 0 #c("87850",)
# exclude6 <- 0 #c("80023","80384","80626","80672","80724","80771","80866","801139") 
# # Did above exclusions in make_and_fix
# # Make lists of variables associated with days.
# day5 <- c("Run5", "Int5", "Max5", "Min5", "RPM5")
# day6 <- c("Run6", "Int6", "Max6", "Min6", "RPM6")
# # Uses the exclusions list to take out the bad values
# for (i in exclude5) ALLRAN[i, day5] <- NA
# for (i in exclude6) ALLRAN[i, day6] <- NA

# Compute average changes in all running variables from night 5 to 6 (skip Min because it should always be 0)
RUN6DB5 <- ALLRAN$Run6/ALLRAN$Run5
mean(RUN6DB5, na.rm=TRUE)
Int6DB5 <- ALLRAN$Int6/ALLRAN$Int5
mean(Int6DB5, na.rm=TRUE)
Max6DB5 <- ALLRAN$Max6/ALLRAN$Max5
mean(Max6DB5, na.rm=TRUE)
RPM6DB5 <- ALLRAN$RPM6/ALLRAN$RPM5
mean(RPM6DB5, na.rm=TRUE)

# List MouseIDs of mice that are missing data for either night 5 or 6
missingrun5 <- as.character(ALLRAN[is.na(ALLRAN$Run5) & !is.na(ALLRAN$Run6), "MouseID"])
missingrun6 <- as.character(ALLRAN[!is.na(ALLRAN$Run5) & is.na(ALLRAN$Run6), "MouseID"])

# Calculate projected values from 5:6 ratios and available data (skip Min because it should always be 0)
for (i in missingrun5) ALLRAN[i, "Run5"] <- (ALLRAN[i, "Run6"])/mean(RUN6DB5, na.rm=TRUE)
for (i in missingrun6) ALLRAN[i, "Run6"] <- (ALLRAN[i, "Run5"])*mean(RUN6DB5, na.rm=TRUE)
for (i in missingrun5) ALLRAN[i, "Int5"] <- (ALLRAN[i, "Int6"])/mean(Int6DB5, na.rm=TRUE)
for (i in missingrun6) ALLRAN[i, "Int6"] <- (ALLRAN[i, "Int5"])*mean(Int6DB5, na.rm=TRUE)
for (i in missingrun5) ALLRAN[i, "Max5"] <- (ALLRAN[i, "Max6"])/mean(Max6DB5, na.rm=TRUE)
for (i in missingrun6) ALLRAN[i, "Max6"] <- (ALLRAN[i, "Max5"])*mean(Max6DB5, na.rm=TRUE)
for (i in missingrun5) ALLRAN[i, "RPM5"] <- (ALLRAN[i, "RPM6"])/mean(RPM6DB5, na.rm=TRUE)
for (i in missingrun6) ALLRAN[i, "RPM6"] <- (ALLRAN[i, "RPM5"])*mean(RPM6DB5, na.rm=TRUE)

# Calculate Run56 from 
ALLRAN["XRun56"] <- (ALLRAN$Run5 + ALLRAN$Run6)/2
ALLRAN["XInt56"] <- (ALLRAN$Int5 + ALLRAN$Int6)/2
ALLRAN["XMax56"] <- (ALLRAN$Max5 + ALLRAN$Max6)/2
ALLRAN["XRPM56"] <- (ALLRAN$RPM5 + ALLRAN$RPM6)/2

# MC - Once you have found "INF" values for ratios dividing by zeros, you will want to plug those mice back into make and fix to be exluded in the appropriate line.
#Plots for reality checks
plot(ALLRAN$Run5, ALLRAN$Run6) + abline(0, 1)
plot(ALLRAN$Int5, ALLRAN$Int6) + abline(0, 1)
plot(jitter(ALLRAN$Max5, factor = 10), jitter(ALLRAN$Max6, factor = 10)) + abline(0, 1)
plot(ALLRAN$RPM5, ALLRAN$RPM6) + abline(0, 1)
bins <- seq(0, (max(ALLRAN$XRun56, na.rm=TRUE)+1000), 1000)
HRXRun56 <- hist(ALLRAN[ALLRAN$Linetype == 1, "XRun56"], breaks=bins)
ControlXRun56 <- hist(ALLRAN[ALLRAN$Linetype == 0, "XRun56"], breaks=bins)
plot(HRXRun56, col=rgb(1,0,0,0.6), xlim=c(0,ALLRAN$XRun56), add=T)  # first histogram
plot(ControlXRun56, col=rgb(0,0,1,0.6), xlim=c(0,ALLRAN$XRun56), add=T)  # second


# Try a bunch of transformations of XRun56
ALLRAN["QXRun56"] <- ALLRAN$XRun56**0.75
ALLRAN["SXRun56"] <- sqrt(ALLRAN$XRun56)
ALLRAN["QTXRun56"] <- ALLRAN$XRun56**0.4
ALLRAN["CXRun56"] <- ALLRAN$XRun56**0.333333333333333333
ALLRAN["FXRun56"] <- ALLRAN$XRun56**0.2
ALLRAN["TXRun56"] <- ALLRAN$XRun56**0.1
ALLRAN["LOGRUN56"] <- log10(ALLRAN$XRun56)
ALLRAN["XMASS"] <- (ALLRAN$MassOn + ALLRAN$MassOff)/2

# Check skewness for all XRun56 transformations
XRun56skew <- skewness(ALLRAN$XRun56, na.rm=TRUE)
QXRun56skew <- skewness(ALLRAN$QXRun56, na.rm=TRUE)
SXRun56skew <- skewness(ALLRAN$SXRun56, na.rm=TRUE)
QTXRun56skew <- skewness(ALLRAN$QTXRun56, na.rm=TRUE)
CXRun56skew <- skewness(ALLRAN$CXRun56, na.rm=TRUE)
FXRun56skew <- skewness(ALLRAN$FXRun56, na.rm=TRUE)
TXRun56skew <- skewness(ALLRAN$TXRun56, na.rm=TRUE)
#LOGRun56skew <- skewness(ALLRAN$LOGRUN56, na.rm=TRUE) #Removed due to inf value
Allskew <- cbind(c("XRun56", "QXRun56", "SXRun56", "QTXRun56", "CXRun56", "FXRun56", "TXRun56"),
            as.data.frame(rbind(XRun56skew, QXRun56skew, SXRun56skew, QTXRun56skew, CXRun56skew, FXRun56skew, TXRun56skew)))
colnames(Allskew) <- c("Transformation", "Skewness")
Allskew[abs(Allskew$Skewness) == min(abs(Allskew$Skewness)),]
Chosen_Run_transformation <- as.character(Allskew[abs(Allskew$Skewness) == min(abs(Allskew$Skewness)), "Transformation"])

# Transformations of RES
ALLRAN["SQRTRES"] <- sqrt(ALLRAN$RES)
ALLRAN["CURTRES"] <- ALLRAN$RES**0.333333333333333333
ALLRAN["LOGRES"] <- log10(ALLRAN$RES)

# Check skewness for all RES transformations
RESskew <- skewness(ALLRAN$RES, na.rm=TRUE)
SQRTRESskew <- skewness(ALLRAN$SQRTRES, na.rm=TRUE)
CURTRESskew <- skewness(ALLRAN$CURTRES, na.rm=TRUE)
#LOGRESskew <- skewness(ALLRAN$LOGRES, na.rm=TRUE)
Allskewres <- cbind(c("RES", "SQRTRES", "CURTRES"),
                 as.data.frame(rbind(RESskew, SQRTRESskew, CURTRESskew)))
colnames(Allskewres) <- c("Transformation", "Skewness")
Allskewres[abs(Allskewres$Skewness) == min(abs(Allskewres$Skewness)),]
Chosen_res_transformation <- as.character(Allskewres[abs(Allskewres$Skewness) == min(abs(Allskewres$Skewness)), "Transformation"])

# Print the best fit transformations 
rbind(Chosen_Run_transformation, Chosen_res_transformation)

#### RUN HERE TO FIRST PASS  - CHANGE RESPONSE VARIABLE AND RES VARIABLE TO MATCH WHAT IS PRINTED ABOVE ######

# Get rid of mice used in special experiments here
# G75, we had Jarren Kay's mice, but they were excluded above because they didn't have massoff


# Linear regression
# Layla changed this 14 July 2016 to add BATCh as a factor and make BATROOM only 1 or 2 (in make syntax).
# as.factor(BATROOM) were taken out for Generation 82 and 83 because we only used one room, 
# In future, if you need to use only 1 room again, delete "+ as.factor(BATROOM)" from the below line

# xxWhy doesn't the result include Linetype as an effect in this model???
LM <- lm(XRun56 ~ as.factor(DamID) + as.factor(Linetype) + as.factor(Sex) + as.factor(Batch) 
         + CURTRES + as.factor(BATROOM), data=ALLRAN)
display(LM)
anova(LM)

# Extract the residuals
residualLM <- as.data.frame(resid(LM))
standardizedresidualLM <- as.data.frame(studres(LM))
residualLM["MouseID"] <- as.numeric(row.names(residualLM))
colnames(residualLM) <- c("Residual", "MouseID")
standardizedresidualLM["MouseID"] <- as.numeric(row.names(standardizedresidualLM))
colnames(standardizedresidualLM) <- c("StdResid", "MouseID")
hist(residualLM$Residual)
hist(standardizedresidualLM$StdResid)

# Add residual values to the mice
Breed <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ALLRAN, residualLM, standardizedresidualLM))
rownames(Breed) <- Breed[, "MouseID"]

# Look for statistical outliers
HighOuliers <- Breed[Breed$StdResid > 3, ]
LowOuliers <- Breed[Breed$StdResid < -3, ]
if (nrow(HighOuliers[!is.na(HighOuliers$MouseID),]) > 0) 
  subset(HighOuliers[!is.na(HighOuliers$MouseID),], 
         select=c("MouseID", "Sex", "Line", "Batch","Wheel", "Run1", "Run2", "Run3", "Run4", "Run5", "Run6", "StdResid")) else "No high outliers"
if (nrow(LowOuliers[!is.na(LowOuliers$MouseID),]) > 0) 
  subset(LowOuliers[!is.na(LowOuliers$MouseID),], 
         select=c("MouseID", "Sex", "Line", "Batch", "Wheel", "Run1", "Run2", "Run3", "Run4", "Run5", "Run6","StdResid")) else "No low outliers"
# Exclude these outliers (if it seems like not natural variation) 

#### RUN HERE TO ON SECOND PASS  - DECIDE IF ANY MICE NEED TO BE EXCLUDED ######

 # Exclude mice based on above printout
 Breed["Outlier"] <- 0
 exclude_outliers <- c("87136","87169","87179","87988","87992","87173")
 for (i in exclude_outliers) Breed[i, "Outlier"] <- 1
 Breeders <- Breed[Breed$Outlier != 1,]

Breeders <- Breeders[order(Breeders$DamID),]

#write Breeders file
#Also, make a breed folder
write.table(Breeders, file="D:/SELECT/Sel85/Breed/g85Breeders.csv",sep=",", row.names=FALSE)

# Generate subsets of Breedersers split by sex and line
Ccolumns <- c("MouseID", "Sex", "DamID", "Line", "Batch", "Wheel", "XRun56")
HRcolumns <- c("MouseID", "Sex", "DamID", "Line", "Batch", "Wheel", "XRun56", "Residual", "StdResid")

Breeders1M <- subset(Breeders[(Breeders$Line == 1 & Breeders$Sex == 1),], select=Ccolumns)
Breeders2M <- subset(Breeders[(Breeders$Line == 2 & Breeders$Sex == 1),], select=Ccolumns)
Breeders3M <- subset(Breeders[(Breeders$Line == 3 & Breeders$Sex == 1),], select=HRcolumns)
Breeders4M <- subset(Breeders[(Breeders$Line == 4 & Breeders$Sex == 1),], select=Ccolumns)
Breeders5M <- subset(Breeders[(Breeders$Line == 5 & Breeders$Sex == 1),], select=Ccolumns)
Breeders6M <- subset(Breeders[(Breeders$Line == 6 & Breeders$Sex == 1),], select=HRcolumns)
Breeders7M <- subset(Breeders[(Breeders$Line == 7 & Breeders$Sex == 1),], select=HRcolumns)
Breeders8M <- subset(Breeders[(Breeders$Line == 8 & Breeders$Sex == 1),], select=HRcolumns)

Breeders1F <- subset(Breeders[(Breeders$Line == 1 & Breeders$Sex == 0),], select=Ccolumns)
Breeders2F <- subset(Breeders[(Breeders$Line == 2 & Breeders$Sex == 0),], select=Ccolumns)
Breeders3F <- subset(Breeders[(Breeders$Line == 3 & Breeders$Sex == 0),], select=HRcolumns)
Breeders4F <- subset(Breeders[(Breeders$Line == 4 & Breeders$Sex == 0),], select=Ccolumns)
Breeders5F <- subset(Breeders[(Breeders$Line == 5 & Breeders$Sex == 0),], select=Ccolumns)
Breeders6F <- subset(Breeders[(Breeders$Line == 6 & Breeders$Sex == 0),], select=HRcolumns)
Breeders7F <- subset(Breeders[(Breeders$Line == 7 & Breeders$Sex == 0),], select=HRcolumns)
Breeders8F <- subset(Breeders[(Breeders$Line == 8 & Breeders$Sex == 0),], select=HRcolumns)

# Write Breederser files
write.table(Breeders1M, file="D:/SELECT/Sel85/Breed/g85L1M.csv",sep=",", row.names=FALSE)
write.table(Breeders2M, file="D:/SELECT/Sel85/Breed/g85L2M.csv",sep=",", row.names=FALSE)
write.table(Breeders3M, file="D:/SELECT/Sel85/Breed/g85L3M.csv",sep=",", row.names=FALSE)
write.table(Breeders4M, file="D:/SELECT/Sel85/Breed/g85L4M.csv",sep=",", row.names=FALSE)
write.table(Breeders5M, file="D:/SELECT/Sel85/Breed/g85L5M.csv",sep=",", row.names=FALSE)
write.table(Breeders6M, file="D:/SELECT/Sel85/Breed/g85L6M.csv",sep=",", row.names=FALSE)
write.table(Breeders7M, file="D:/SELECT/Sel85/Breed/g85L7M.csv",sep=",", row.names=FALSE)
write.table(Breeders8M, file="D:/SELECT/Sel85/Breed/g85L8M.csv",sep=",", row.names=FALSE)

write.table(Breeders1F, file="D:/SELECT/Sel85/Breed/g85L1F.csv",sep=",", row.names=FALSE)
write.table(Breeders2F, file="D:/SELECT/Sel85/Breed/g85L2F.csv",sep=",", row.names=FALSE)
write.table(Breeders3F, file="D:/SELECT/Sel85/Breed/g85L3F.csv",sep=",", row.names=FALSE)
write.table(Breeders4F, file="D:/SELECT/Sel85/Breed/g85L4F.csv",sep=",", row.names=FALSE)
write.table(Breeders5F, file="D:/SELECT/Sel85/Breed/g85L5F.csv",sep=",", row.names=FALSE)
write.table(Breeders6F, file="D:/SELECT/Sel85/Breed/g85L6F.csv",sep=",", row.names=FALSE)
write.table(Breeders7F, file="D:/SELECT/Sel85/Breed/g85L7F.csv",sep=",", row.names=FALSE)
write.table(Breeders8F, file="D:/SELECT/Sel85/Breed/g85L8F.csv",sep=",", row.names=FALSE)


#Generate means for look file
ALLRAN$Run56 <- ALLRAN$XRun56
ALLRAN$Int56 <- ALLRAN$XInt56
ALLRAN$Max56 <- ALLRAN$XMax56
ALLRAN$RPM56 <- ALLRAN$XRPM56

Look_Sex <- ddply(ALLRAN, .(Sex), summarise,
                  N = sum(!is.na(Run56)),
                  mean_Run56 = mean(Run56, na.rm=TRUE),
                  sd_Run56   = sd(Run56, na.rm=TRUE),
                  se_Run56   = sd_Run56 / sqrt(N),
                  mean_Int56 = mean(Int56, na.rm=TRUE),
                  sd_Int56   = sd(Int56, na.rm=TRUE),
                  se_Int56   = sd_Int56 / sqrt(N),
                  mean_Max56 = mean(Max56, na.rm=TRUE),
                  sd_Max56   = sd(Max56, na.rm=TRUE),
                  se_Max56   = sd_Max56 / sqrt(N),
                  mean_RPM56 = mean(RPM56, na.rm=TRUE),
                  sd_RPM56   = sd(RPM56, na.rm=TRUE),
                  se_RPM56   = sd_RPM56 / sqrt(N))
Look_Sex["Line"] <- "All"
Look_Sex["Linetype"] <- "All"

Look_Linetype <- ddply(ALLRAN, .(Linetype, Sex), summarise,
                       N = sum(!is.na(Run56)),
                       mean_Run56 = mean(Run56, na.rm=TRUE),
                       sd_Run56   = sd(Run56, na.rm=TRUE),
                       se_Run56   = sd_Run56 / sqrt(N),
                       mean_Int56 = mean(Int56, na.rm=TRUE),
                       sd_Int56   = sd(Int56, na.rm=TRUE),
                       se_Int56   = sd_Int56 / sqrt(N),
                       mean_Max56 = mean(Max56, na.rm=TRUE),
                       sd_Max56   = sd(Max56, na.rm=TRUE),
                       se_Max56   = sd_Max56 / sqrt(N),
                       mean_RPM56 = mean(RPM56, na.rm=TRUE),
                       sd_RPM56   = sd(RPM56, na.rm=TRUE),
                       se_RPM56   = sd_RPM56 / sqrt(N))
Look_Linetype["Line"] <- "All"

Look_Line <- ddply(ALLRAN, .(Linetype, Line, Sex), summarise,
                   N = sum(!is.na(Run56)),
                   mean_Run56 = mean(Run56, na.rm=TRUE),
                   sd_Run56   = sd(Run56, na.rm=TRUE),
                   se_Run56   = sd_Run56 / sqrt(N),
                   mean_Int56 = mean(Int56, na.rm=TRUE),
                   sd_Int56   = sd(Int56, na.rm=TRUE),
                   se_Int56   = sd_Int56 / sqrt(N),
                   mean_Max56 = mean(Max56, na.rm=TRUE),
                   sd_Max56   = sd(Max56, na.rm=TRUE),
                   se_Max56   = sd_Max56 / sqrt(N),
                   mean_RPM56 = mean(RPM56, na.rm=TRUE),
                   sd_RPM56   = sd(RPM56, na.rm=TRUE),
                   se_RPM56   = sd_RPM56 / sqrt(N))

ggplot(Look_Linetype, aes(x=as.factor(Sex), y=mean_Run56, fill=as.factor(Linetype))) +      
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + 
  geom_bar(stat="identity", position=position_dodge()) +  
  scale_fill_manual(values = c("#1f497c", "#C00000")) +
  geom_errorbar(aes(ymin = mean_Run56-se_Run56, ymax = mean_Run56+se_Run56), width=.25, size = 1, position=position_dodge(.9), color="black")

Look <- rbind(Look_Sex, Look_Linetype, Look_Line)
LookSorted <- Look[order(Look$Sex,Look$Linetype),]

#write Look file
write.csv(LookSorted, "D:/SELECT/Sel85/Analyze/g85Look.csv", row.names=FALSE)
#write.xlsx(LookSorted, file=paste(wd, "g85Look.xls",sep=""), row.names=FALSE)
