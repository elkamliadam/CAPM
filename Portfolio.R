#In this file we are gonna create a hypothetical portfolio that 
#we are gonna use in the CAPM project

#Our portfolio will be consisted of 3 Moroccan exchanged stocke
library(quantmod)
library(readxl)
library(xts)
library(tidyverse)
library(janitor)

#The imported data contains some charachters that interfere in the conversion to numerical
#This function replace those charachters with " "
clean_and_convert <- function(x) {
  x <- gsub("[^0-9.-]", "", x)  
  as.numeric(x)
}
#Label-vie historic data
label <- read_excel('Label_vie.xlsx')
label <- clean_names(label)
label$seance <- as.Date(label$seance, format = "%d/%m/%Y") #Format the date
label <- label[, c(1, 4, 5, 6, 7, 9, 12)] #Select the colomns that interest us
label[,-1] <- lapply(label[,-1], clean_and_convert) #We clean the data
label <- as.xts(label[, -1], order.by = label$seance)#Convert to an xts object
colnames(label) <- c("LABEL.Open", "LABEL.Close", "LABEL.High", "LABEL.Low", "LABEL.Volume", "LABEL.Adjusted") #Standardise the names 
label <- label[, c(1, 3, 4, 2, 5, 6)]#permute the order so it followd the OHLC standard
label.monthly <- to.monthly(label) #Change to monthly
label.monthly <- label.monthly[,6]#Keep the adjusted price only
head(label.monthly)
label.ret <- Delt(label.monthly$label.Adjusted) #Calculate the returns
names(label.ret) <- paste("LABEL.ret")#name the colomn
head(label.ret)

# Alliances data
Alliances <- read_excel("Alliances.xlsx")
Alliances <- clean_names(Alliances)
Alliances$seance <- as.Date(Alliances$seance, format = "%d/%m/%Y")
Alliances <- Alliances[, c(1, 4, 5, 6, 7, 9, 12)]
Alliances[,-1] <- lapply(Alliances[,-1], clean_and_convert)
Alliances <- as.xts(Alliances[, -1], order.by = Alliances$seance)
colnames(Alliances) <- c("ALLIANCES.Open", "ALLIANCES.Close", "ALLIANCES.High", "ALLIANCES.Low", "ALLIANCES.Volume", "ALLIANCES.Adjusted")
Alliances <- Alliances[, c(1, 3, 4, 2, 5, 6)]
Alliances.monthly <- to.monthly(Alliances)
Alliances.monthly <- Alliances.monthly[,6]
head(Alliances.monthly)
Alliances.ret <- Delt(Alliances.monthly$Alliances.Adjusted)
names(Alliances.ret) <- paste("ALLIANCES.ret")
head(Alliances.ret)

#HPS data

Hps <- read_excel('HPS.xlsx')
Hps <- clean_names(Hps)
Hps$seance <- as.Date(Hps$seance, format = "%d/%m/%Y")
Hps <- Hps[, c(1, 4, 5, 6, 7, 9, 12)]
Hps[,-1] <- lapply(Hps[,-1], clean_and_convert)
Hps <- as.xts(Hps[, -1], order.by = Hps$seance)
colnames(Hps) <- c("HPS.Open", "HPS.Close", "HPS.High", "HPS.Low", "HPS.Volume", "HPS.Adjusted")
Hps <- Hps[, c(1, 3, 4, 2, 5, 6)]
Hps.monthly <- to.monthly(Hps)
Hps.monthly <- Hps.monthly[,6]
head(Hps.monthly)
Hps.ret <- Delt(Hps.monthly$Hps.Adjusted)
names(Hps.ret) <- paste("HPS.ret")
head(Hps.ret)

#Now we combined them in one data set 
port <- cbind(label.ret, Alliances.ret, Hps.ret)
#We calculate the Equally Weighted return
port$port.ret <- rowMeans(port)
port <- port[-1, 4] #Remove the firts row since its NA and keep the port.ret only
head(port)

#Now we save our data into a csv file
csv.port <- cbind(index(port), data.frame(port))
names(csv.port)[1] <- paste(c("date")) #change the index name to date since the idnex in xts is the date
head(csv.port)
#Now we change the real data index 
row.names(csv.port) <- seq(1,nrow(csv.port), by = 1)
head(csv.port)
write.csv(csv.port, "Portfolio(monthly).csv")




