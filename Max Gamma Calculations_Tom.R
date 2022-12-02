
ls()
rm(list = ls())

#setwd("C:/Users/twbc201/OneDrive - University of Exeter")
setwd("/Users/emilielindqvist/Dropbox/BYS2/R Inequlity and the Biospere/from Tom/")

library(tidyverse)     

x <- read.csv("topic_papers_DOI_fromJuan.csv")
pause
names(x)

summary(x)
str(x)
names(x)
x$topic <- as.factor(x$topic)
str(x)


# this works 
test <- (
x %>%
  group_by(document) %>%
  filter(gamma == max(gamma))
)
head(test)
write.csv(test, "maxgamma_DOI_fromJuan.csv")
