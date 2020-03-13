# Check file clone
# Gerald Claghorn, 9 October 2014 (Last updated 21 April 2015)

# Instructions:
# You can run the whole script right now! Just hit "Source" (in RStudio)
# OR from the console enter (for example) source(~/check_file_name.r)
# You will be prompted to enter the generation, batch, and file date

# Disclaimers:
# THIS IS FOR FOUR COMPUTERS (A, B, C, D) NAMED BY LAB CONVENTION (ex. 150105A.DAT)
# AND STORED IN A DIRECTORY BY LAB CONVENTION (ex. c:/SELECT/g74/whldata/batch4)
# IT WILL NOT WORK CORRECTLY FOR ANY OTHER FORMAT

# Description:
# This script takes .dat files from each of four computers and puts them together,
# while computing total revolutions, active intervals, and wheel speed (average and max).
# It also checks for doubling, as well as wheels with fewer than 1000 revs.
# Results will be opened at the end of the file.
{
# Get into our wheel directory
# R prompts user for generation and batch which are joined with text to locate the directory
# Then, user specifies the YYMMDD code specific to the data for this day
gen <- readline("Generation:")
batch <- readline("Batch:")
setwd(paste("D:/SELECT/SEL",gen,"/whldata/batch",batch, sep=""))
today <- readline("File Date? (YYMMDD format, ex. 150105): ")

# Read in data from .dat file
# R uses the previous information to specify the exact names (*A.DAT, *B.DAT, etc.).
# If the file .dat has been opened and re-saved, it will need to be opened as a delimited file.
for (i in paste(today,toupper(letters)[1:4],".DAT", sep="")) assign(substr(i,7,7), read.csv(i))
if (length(A) == 1) 
  A <- read.delim(paste(today, "A.DAT", sep=""), header=FALSE, stringsAsFactors=FALSE)
if (length(B) == 1) 
  B <- read.delim(paste(today, "B.DAT", sep=""), header=FALSE, stringsAsFactors=FALSE)
if (length(C) == 1) 
  C <- read.delim(paste(today, "C.DAT", sep=""), header=FALSE, stringsAsFactors=FALSE)
if (length(D) == 1) 
  D <- read.delim(paste(today, "D.DAT", sep=""), header=FALSE, stringsAsFactors=FALSE)

# Store the original file dimensions, should be exactly 157 columns and about 1400 rows
File_Dimensions <-rbind(dim(A),dim(B),dim(C),dim(D))

# R needs to join each computer file together, but only AFTER they each have the same number of rows.
# Add NA to the end of each file to make them all the same length
# At the end of this step all files will have the number of observations from the longest file
if (nrow(A) < max(nrow(A), nrow(B),nrow(C),nrow(D)))
  A[(nrow(A)+1):max(nrow(A), nrow(B),nrow(C),nrow(D)), 1:length(A)] <- NA
if (nrow(B) < max(nrow(A), nrow(B),nrow(C),nrow(D)))
  B[(nrow(B)+1):max(nrow(A), nrow(B),nrow(C),nrow(D)), 1:length(B)] <- NA
if (nrow(C) < max(nrow(A), nrow(B),nrow(C),nrow(D)))
  C[(nrow(C)+1):max(nrow(A), nrow(B),nrow(C),nrow(D)), 1:length(C)] <- NA
if (nrow(D) < max(nrow(A), nrow(B),nrow(C),nrow(D)))
  D[(nrow(D)+1):max(nrow(A), nrow(B),nrow(C),nrow(D)), 1:length(D)] <- NA

# Extract Total, Forward, and Backward columns and then bind them
Total <- cbind(
  A[,c(seq(10,ncol(A),3))], # Starting with column 10, take every third column up to end of file
  B[,c(seq(10,ncol(B),3))], 
  C[,c(seq(10,ncol(C),3))], 
  D[,c(seq(10,ncol(D),3))])
Forward <- cbind(
  A[,c(seq(8,ncol(A)-2,3))], # Starting with column 8, take every third column up to end of file -2
  B[,c(seq(8,ncol(B)-2,3))], 
  C[,c(seq(8,ncol(C)-2,3))], 
  D[,c(seq(8,ncol(D)-2,3))]) 
Backward <- cbind(
  A[,c(seq(9,ncol(A)-1,3))], # Starting with column 9, take every third column up to end of file -1
  B[,c(seq(9,ncol(B)-1,3))], 
  C[,c(seq(9,ncol(C)-1,3))], 
  D[,c(seq(9,ncol(D)-1,3))])

#Assign column names (wheels)
colnames(Total) <- c(1:200); colnames(Forward) <- c(1:200); colnames(Backward) <- c(1:200)

# Find cases of doubling, where forward and backward revs are greater than 20 AND total revs are greater than 40
# Checking only for forward and backward > 20 gives a different answer for mysterious reasons.
# Then count positive results for each wheel
dbl.ck <- as.data.frame(cbind(1:200, sapply((as.data.frame(Forward > 20 & Backward > 20 & Total > 40)*1), sum, na.rm=TRUE)))
colnames(dbl.ck) <- c("Wheel", "Cases of Doubling")

#Store all doubling wheels as a new data frame
if(nrow(dbl.ck[dbl.ck$Cases>0,])==0) 
  Doubling <- "No Doubling" else Doubling <- dbl.ck[dbl.ck$Cases>0,]

#Compute rev, int, and max
rev <- cbind(1:200, sapply(Total, sum, na.rm=TRUE)); colnames(rev) <- c("Wheel", "Run")
int <- cbind(1:200, colSums(Total != 0, na.rm=TRUE)); colnames(int) <- c("Wheel", "Int")
max <- cbind(1:200, sapply(Total, max, na.rm=TRUE)); colnames(max) <- c("Wheel", "Max")
min <- cbind(1:200, sapply(Total, min, na.rm=TRUE)); colnames(min) <- c("Wheel", "Min")

#Put the files from the four computers together.
ABCD <- Reduce(function(x, y) merge(x, y, all=TRUE), list(rev, int, max, min))
ABCD["rpm"] <- ABCD$Run/ABCD$Int

#write file
write.table(ABCD, file=paste("ABCD", today,".csv", sep=""),sep=",", row.names=FALSE)

# Remove unneccesary objects that might cause problems if they were "recycled" from previous day
rm(A);rm(B);rm(C);rm(D);rm(int);rm(max);rm(min);rm(rev);rm(batch);rm(gen);rm(i);rm(today)

# Output
Under_1000_Revs <- ABCD[ABCD$Run <1000,] # Print revs > 1000
Mean_Running <- sapply(ABCD, mean, na.rm = TRUE) # Means, ignores NA
Range_Running <- sapply(ABCD, range, na.rm = TRUE) #ranges, ignores NA
colnames(File_Dimensions) <- c("Minutes", "Columns (usually 157)"); rownames(File_Dimensions) <- c("A","B","C","D")
View(Under_1000_Revs);View(Mean_Running);View(Range_Running);View(Doubling);View(File_Dimensions);View(ABCD)
}