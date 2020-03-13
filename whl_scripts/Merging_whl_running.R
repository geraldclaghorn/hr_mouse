# whl.run clone
# Made by Gerald Claghorn 19 January 2015
# 3 batches, by Layla 28 October 2016

# To run this file
# 1. Make an analyze folder if there isn't one already
# 2. Change dates to match your batches (at the read.csv commands)
# 3. Find and replance generation [g85 and Sel85] (8 replacements for four batches)

# This file takes the ABCDYYMMDD.csv files that are generated from the check file
# and combines them to yield one file with all of the batches together and labled.

# The dates should only show up in one place only, as ABCDYYMMDD.csv
# In a typical generation, you should end up with one out file "Run"
# That contains 600 observations of 38 variables:
# (Wheel, MouseID, Batch, Sexwh, Linewh, Masson, Massoff, Whlnotes, Run1-6, Int1-6, Max1-6, Min1-6, RPM1-6)

library(xlsx)

setwd("D:/SELECT/sel85/whldata/batch1/")
B1D1 <- read.csv("ABCD180306.csv")
B1D2 <- read.csv("ABCD180307.csv")
B1D3 <- read.csv("ABCD180308.csv")
B1D4 <- read.csv("ABCD180309.csv")
B1D5 <- read.csv("ABCD180310.csv")
B1D6 <- read.csv("ABCD180311.csv")
#Import masson/massoff, etc.
B1whl <- read.xlsx("g85B1whl.xls",1, stringsAsFactors=FALSE,startRow=6)

setwd("D:/SELECT/Sel85/whldata/batch2/")
B2D1 <- read.csv("ABCD180313.csv")
B2D2 <- read.csv("ABCD180314.csv")
B2D3 <- read.csv("ABCD180315.csv")
B2D4 <- read.csv("ABCD180316.csv")
B2D5 <- read.csv("ABCD180317.csv")
B2D6 <- read.csv("ABCD180318.csv")
#Import masson/massoff, etc.
B2whl <- read.xlsx("g85B2whl.xls",1, stringsAsFactors=FALSE,startRow=6)

setwd("D:/SELECT/Sel85/whldata/batch3/")
# do we need this? -> b3date <- 160628
B3D1 <- read.csv("ABCD180320.csv")
B3D2 <- read.csv("ABCD180321.csv")
B3D3 <- read.csv("ABCD180322.csv")
B3D4 <- read.csv("ABCD180323.csv")
B3D5 <- read.csv("ABCD180324.csv")
B3D6 <- read.csv("ABCD180325.csv")
#Import masson/massoff, etc.
B3whl <- read.xlsx("g85B3whl.xls", 1, stringsAsFactors=FALSE,startRow=6)

#Make some more descriptive column names
D1cols <- c("Wheel", "Run1", "Int1", "Max1", "Min1", "RPM1")
D2cols <- c("Wheel", "Run2", "Int2", "Max2", "Min2", "RPM2")
D3cols <- c("Wheel", "Run3", "Int3", "Max3", "Min3", "RPM3")
D4cols <- c("Wheel", "Run4", "Int4", "Max4", "Min4", "RPM4")
D5cols <- c("Wheel", "Run5", "Int5", "Max5", "Min5", "RPM5")
D6cols <- c("Wheel", "Run6", "Int6", "Max6", "Min6", "RPM6")
# Assign the column names as appropriate, there HAS to be a better way of doing all of this - JC
# Batch 1
colnames(B1D1) <- D1cols
colnames(B1D2) <- D2cols
colnames(B1D3) <- D3cols
colnames(B1D4) <- D4cols
colnames(B1D5) <- D5cols
colnames(B1D6) <- D6cols
# Batch 2
colnames(B2D1) <- D1cols
colnames(B2D2) <- D2cols
colnames(B2D3) <- D3cols
colnames(B2D4) <- D4cols
colnames(B2D5) <- D5cols
colnames(B2D6) <- D6cols
# Batch 3
colnames(B3D1) <- D1cols
colnames(B3D2) <- D2cols
colnames(B3D3) <- D3cols
colnames(B3D4) <- D4cols
colnames(B3D5) <- D5cols
colnames(B3D6) <- D6cols

#merge all days for each batch
B1 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
             list(B1whl, B1D1, B1D2, B1D3, B1D4, B1D5, B1D6))
B2 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
             list(B2whl, B2D1, B2D2, B2D3, B2D4, B2D5, B2D6))
B3 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
             list(B3whl, B3D1, B3D2, B3D3, B3D4, B3D5, B3D6))

#put the batches together
run <- rbind(B1, B2, B3)
#delete wheels with no mice
run <- run[!is.na(run$MouseID),]

# List any duplicate MouseIDs (Values of duplicate ID will print here, and you will have an error if they are not fixed)
if(length(run[duplicated(run$MouseID) & run$MouseID != -9, 1]) >0 ) 
  run[duplicated(run$MouseID) & run$MouseID != -9, c("MouseID","Batch")] else "No duplicate MouseIDs"

# This code locates mouseid duplicates:
# run[run$MouseID==,]

#write file
write.xlsx(run, file="D:/select/Sel85/analyze/g85run.xlsx", row.names=FALSE)

# Left to do:
# 1. merge in wean data
# 2. Merge in resistances
# 3. Fix resistances

