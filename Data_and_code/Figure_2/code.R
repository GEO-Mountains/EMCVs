################
### Code to reproduce Figure 2 of Thornton et al. (2021) - Towards a Definition of Essential Mountain Climate Variables
### ############

#Set the working directory and install packages:

setwd("X:/XX/XX")

install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 

#Import data and set colour palette:

data_in <- read.csv("word_cloud_input_data.csv", colClasses=c("character", "numeric"))
pal2 <- brewer.pal(8,"Dark2")

#Generate word cloud:

wordcloud(data_in$term,data_in$freq, scale=c(8,.4),min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

#Note, one must ensure that the "plots" window is sufficiently large to display all terms. 

#Save the output using the interactive options (e.g. in RStudio). 

#Alternatively one can pregenerate an empty PDF file with the required dimensions and write the output plot to it.

################ 