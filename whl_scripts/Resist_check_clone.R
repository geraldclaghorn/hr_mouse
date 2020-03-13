# Check resistances file clone
# Gerald Claghorn, 9 October 2014

# THIS FILE RUNS IN TWO PASSES
# IF PROCESSING MULTIPLE DAYS IN ONE SITTING, CLEAR OBJECTS FROM ENVIRONMENT BETWEEN DAYS

# To run this file
# 1. Change the wheel directory (very first command)
# 2. Find and replace the date (REMMDD) (There should be 18 replacements)
# 3. Run to the large comment and check the dimensions of the objects
# 4. Run the rest of the file

# This file takes .dat resistance files from each of four computers and puts them together,
# Input files named according to the resistance convention MMDD__.dat
# where the blank is the computer ID (A, B, C, or D), and the pass # (1 or 2)

# Get into our resistance directory
setwd("D:/SELECT/Sel85/Resist/")

# Read in data from .dat file 
# If the file .dat has been opened and re-saved, you might get hung up here.
# Make sure all matricies are exactly 1 observations of exactly 157 Variables
# Some may need to be read as tabbed ("delim") instead of csv
# The if statements should take care of those
A1 <- read.csv("re0326A1.DAT")
if (ncol(A1) == 1) A1 <- read.delim("re0326A1.DAT")
A2 <- read.csv("re0326A2.DAT")
if (ncol(A2) == 1) A2 <- read.delim("re0326A2.DAT")
B1 <- read.csv("re0326B1.DAT")
if (ncol(B1) == 1) B1 <- read.delim("re0326B1.DAT")
B2 <- read.csv("re0326B2.DAT")
if (ncol(B2) == 1) B2 <- read.delim("re0326B2.DAT")
C1 <- read.csv("re0326C1.DAT")
if (ncol(C1) == 1) C1 <- read.delim("re0326C1.DAT")
C2 <- read.csv("re0326C2.DAT")
if (ncol(C2) == 1) C2 <- read.delim("re0326C2.DAT")
D1 <- read.csv("re0326D1.DAT")
if (ncol(D1) == 1) D1 <- read.delim("re0326D1.DAT")
D2 <- read.csv("re0326D2.DAT")
if (ncol(D2) == 1) D2 <- read.delim("re0326D2.DAT")
rbind(dim(A1), dim(A2), dim(B1), dim(B2), dim(C1), dim(C2), dim(D1), dim(D2))

####################################################################################################################
###########      RUN TO HERE FIRST, CHECK THE DIMENSIONS OF THE NEW MATRICIES                            ###########
###########      If they are exactly 1 observations of exactly 157 variables, proceed                    ###########
###########      If not, use dropdown menu to open the problem file(s) and store as "A" "B" "C" or "D"   ###########
####################################################################################################################

#create column names
timecol <- c("hour", "mintue", "second", "msecond", "month", "day", "year")
Acols <- c(timecol, paste(c("f", "r", "t"), sep = "", collapse = NULL,round(((1:150 + 1)/3), 0)))
Bcols <- c(timecol, paste(c("f", "r", "t"), sep = "", collapse = NULL,round((((1:150 + 1)/3)+50), 0)))
Ccols <- c(timecol, paste(c("f", "r", "t"), sep = "", collapse = NULL,round((((1:150 + 1)/3)+100), 0)))
Dcols <- c(timecol, paste(c("f", "r", "t"), sep = "", collapse = NULL,round((((1:150 + 1)/3)+150), 0)))

#Assign column names
colnames(A1) <- Acols
colnames(B1) <- Bcols
colnames(C1) <- Ccols
colnames(D1) <- Dcols
colnames(A2) <- Acols
colnames(B2) <- Bcols
colnames(C2) <- Ccols
colnames(D2) <- Dcols

#Seperate forward, backward, and total columns
ARES1 <- A1[, grep("^t", names(A1))]
BRES1 <- B1[, grep("^t", names(B1))]
CRES1 <- C1[, grep("^t", names(C1))]
DRES1 <- D1[, grep("^t", names(D1))]
ARES2 <- A2[, grep("^t", names(A2))]
BRES2 <- B2[, grep("^t", names(B2))]
CRES2 <- C2[, grep("^t", names(C2))]
DRES2 <- D2[, grep("^t", names(D2))]

#Put the files from the four computers together.
RESABCD1 <- cbind(ARES1, BRES1, CRES1, DRES1)
RESABCD2 <- cbind(ARES2, BRES2, CRES2, DRES2)

#Give the new data sets column names
colnames(RESABCD1) <- 1:200
colnames(RESABCD2) <- 1:200

#Put the above together
Wheel<- (1:200)
RES1Flip <- rbind(Wheel, RESABCD1)
RES2Flip <- rbind(Wheel, RESABCD2)
RES1 <- as.data.frame(t(RES1Flip), row.names = NULL, optional = FALSE)
RES2 <- as.data.frame(t(RES2Flip), row.names = NULL, optional = FALSE)
colnames(RES1) <- c("Wheel", "RES")
colnames(RES2) <- c("Wheel", "RES")

#write file
write.table(RES1, file="re0326_1.csv",sep=",", row.names=FALSE)
write.table(RES2, file="re0326_2.csv",sep=",", row.names=FALSE)