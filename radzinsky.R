setwd("~/Projects/Occupation/France-ivrep")
library(foreign)
library(nnet)
library(readstata13)
library(dplyr)
library(stargazer)
library(lattice)
library(ggplot2)

#read in original data
pool <- read.dta13("France-ivrep/rollcallpool.dta")

#factorize the north indicator (north =1 if department is mostly north of the line)
as.factor(pool$north)

#subset just those provinces mostly on either side
line <- subset(pool, ireg %in% c("Loir et Cher",
                                     "Nievre",
                                     "Landes",
                                     "Gironde",
                                     "Charente",
                                     "Dordogne",
                                     "Allier",
                                     "Haute-Vienne",
                                      "Creuse"))
#--party id in territories---#
n <- subset(pool, ireg %in% c("Loir et Cher",
                                  "Nievre",
                                  "Landes",
                                  "Gironde",
                                  "Charente"))
s <- subset(pool, ireg %in% c("Allier",
                                  "Dordogne",
                                  "Haute-Vienne",
                                  "Creuse"))

ftable(n$PARTY1NE)
ftable(s$PARTY1NE)
ftable(n$PARTY2NE)
ftable(s$PARTY2NE)
ftable(n$PARTY3NE)
ftable(s$PARTY3NE)

#--Vote on whether to join NATO, 03/1950, V2321----------------------------#
#subset the vote on NATO
natoline <- subset(line, line$V2321 %in% c(
    "yes",
    "no",
    "not present"))

#bar graph of vote on whether to join NATO
ggplot(natoline, aes(natoline$V2321, fill=factor(natoline$PARTY2NE), alpha=as.factor(natoline$north))) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), stat="bin", position="dodge") +
  labs(x="Whether to join NATO",y="Number of votes") +
  ggtitle("NATO Vote Distribution by Bloc and Side of Dem. Line") +
  scale_fill_brewer(name="Party bloc") +
  scale_alpha_discrete(name="Is North", range=c(0.6, 1)) +
  theme(
    plot.title=element_text(size = 12),
    axis.title.x=element_text(size=10),
    axis.title.y=element_text(size=10))


#--Vote on whether to develop atomic energy, 07/1957, V3131----------------#

nukeline <- subset(line, line$V3131 %in% c(
    "yes",
    "no",
    "not present"))

#bar graph of vote on whether to develop atomic energy
ggplot(nukeline, aes(nukeline$V3131, fill=factor(nukeline$PARTY3NE), alpha=as.factor(nukeline$north))) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), stat="bin", position="dodge") +
  labs(x="Whether to develop nuclear energy",y="Number of votes") +
  ggtitle("Nuclear Energy Vote Distribution by Bloc and Side of Dem. Line") +
  scale_fill_brewer(name="Party bloc") +
  scale_alpha_discrete(name="Is North", range=c(0.6, 1)) +
  theme(
      plot.title=element_text(size = 12),
      axis.title.x=element_text(size=10),
      axis.title.y=element_text(size=10))


