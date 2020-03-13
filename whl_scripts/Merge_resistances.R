# res.run clone
# Made by Gerald Claghorn 19 January 2015
# 3 batches, by Layla 28 October 2016

# To run this file
# 1. Make an analyze folder if there isn't one already
# 2. Change dates to match your batches (at the read.csv commands)
# 3. Find and replance generation (14 replacements for four batches)

# This file takes the REMMDD__.csv files that are generated from the resistance check file
# and combines them to yield one file with all of the resistance measurements together and labled.

# The dates should only show up in one place only, as REMMDD__.csv
# In a typical generation, you should end up with one out file "G__Run"
# That contains 800 observations of 38 variables:
# (Wheel, MouseID, Batch, Sexwh, Linewh, Masson, Massoff, Whlnotes, Run1-6, Int1-6, Max1-6, Min1-6, RPM1-6)

# Read in resistances
setwd("D:/SELECT/Sel85/resist/")
RES1 <- read.csv("RE0305_1.csv")
RES2 <- read.csv("RE0305_2.csv")
RES3 <- read.csv("RE0312_1.csv")
RES4 <- read.csv("RE0312_2.csv")
RES5 <- read.csv("RE0319_1.csv")
RES6 <- read.csv("RE0319_2.csv")
RES7 <- read.csv("RE0326_1.csv")
RES8 <- read.csv("RE0326_2.csv")

# name the columns
colnames(RES1) <- c("Wheel", "RES1")
colnames(RES2) <- c("Wheel", "RES2")
colnames(RES3) <- c("Wheel", "RES3")
colnames(RES4) <- c("Wheel", "RES4")
colnames(RES5) <- c("Wheel", "RES5")
colnames(RES6) <- c("Wheel", "RES6")
colnames(RES7) <- c("Wheel", "RES7")
colnames(RES8) <- c("Wheel", "RES8")

#Put resistances together
resistance <- Reduce(function(x, y) merge(x, y, all=TRUE), 
             list(RES1, RES2, RES3, RES4, RES5, RES6, RES7, RES8))

#write file
write.table(resistance, file="D:/SELECT/SEL85/analyze/g85res.csv",sep=",", row.names=FALSE)
