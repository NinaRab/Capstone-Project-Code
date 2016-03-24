#load ggplot2, set dataFolder
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
dataFolder <- "C:/Users/Nina/Desktop/datafoundations/Capstone Project/"

#load all the data gathered related to my capstone project
allprices <- read_excel(paste0(dataFolder, "all_prices.xlsx"))
leafly1 <- read.csv(paste0(dataFolder, "leafly_1.csv"))
leafly1761 <- read.csv(paste0(dataFolder, "leafly_1761.csv"))
leaflyeffects <- read.csv(paste0(dataFolder, "leafly_effects.csv"))
conditions <- read.csv(paste0(dataFolder, "conditions_strains.csv"))
conditions1 <- read.csv(paste0(dataFolder, "conditions1.csv"))

#look at structure of each dataset
str(allprices)
str(leafly1)
str(leafly1761)
str(leaflyeffects)
str(conditions)
str(conditions1)

#join leafly1 & leafly1761 and conditions & conditions1 by rows
leaflyStrains <- as.data.frame(bind_rows(leafly1, leafly1761))
allconditions <- as.data.frame(bind_rows(conditions, conditions1))

#add effects to leafly_strains
leafly <- full_join(leaflyStrains, leaflyeffects, by = "name")

#clean leafly and allconditions datasets of extraneous data
leafly$parents_link <- NULL
leafly$parents_link._source <- NULL
leafly$parents._source <- NULL
leafly$parents <- NULL
allconditions$pageUrl <- NULL
allconditions$strains <- NULL

#remove duplicate rows
leafly <- distinct(leafly)
allconditions <- distinct(allconditions)

#parse leafly data to contain succinct, relevant data
leafly$number_reviews <- gsub("[0-9](\\.*?)([0-9]*?\\|)|Reviews", "", leafly$number_reviews)
leafly$pageUrl.x <- gsub("https://www.leafly.com/|(/[a-z0-9-]+)", "", leafly$pageUrl.x)

#parse allconditions data to contain succinct, relevant data
allconditions$condition <- gsub("Cannabis Strains That Help With|Best Cannabis Strains to Relieve", "", allconditions$condition)

#reorder leafly data so strain name is first
leafly <- select(leafly, name, everything())

#rename pageUrl.x to strain.type
leafly <- rename(leafly, strain.type = pageUrl.x)

#separate allconditions strains (each strain gets its own column) 
allconditions <- separate(allconditions, strains._source, c(1:56), sep = "; ")
allconditions <- as.data.frame(t(allconditions))

#save transposed allconditions data to allconditions_tran
allconditions_tran <- as.data.frame(t(allconditions))

#create a vector of condition names from allconditions_tran to use as labels for allconditions data
names(allconditions) <- as.vector(allconditions_tran$condition)

#remove first row from allconditions (since it is really the label for each column)
allconditions <- allconditions[-1,]

#create a vector of 1, and a vector of the number of strains for each condition
numStrains <- 1
strains_per_condition <- as.vector(summarise_each(allconditions, funs(n_distinct)))

#create a vector of strain names for strains in allconditions and remove duplicates

#####one hot encoding for conditions for each strain####

#execute these steps for each condition 
strains <- list() #create an empty list
for (i in 1:nrow(allconditions_tran)){
  this_strains <- unique(allconditions[,i]) #remove repeat strains for each condition
  this_strains <- this_strains[!is.na(this_strains)] #get rid of NAs
  temp <- rep.int(numStrains, length(this_strains)) #create a vector of 1's of length = number of strains for that condition
  names(temp) <- as.character(unique(as.character(this_strains))) #set the names of the temp vector as the names of the strains
  temp <- t(data.frame(temp)) #transpose the temp data frame
  strains[[allconditions_tran$condition[i]]] <- temp #save the transposed temp data frame to strains, with the column header of what condition it is
}

#at the end of the for-loop
#1. smartbind each strain (combine the strains list to the strains_final data frame and put NA's where data is missing)
library(gtools)
strains_final <- data.frame(do.call(smartbind, strains)) 

#############

#transpose strains_final to be able to combine with leafly data and apply condition names
strains_final <- data.frame(t(data.frame(strains_final)))
names(strains_final) <- as.character(allconditions_tran$condition)

###remove numbers from leafly$flavors and then separate each
leafly_f_num <- as.data.frame(leafly$flavors)
leafly$flavors <- gsub("[0-9]\\.", "", leafly$flavors)
leafly$flavors <- sub(" ", "", leafly$flavors)
leafly$flavors <- gsub("  ", ";", leafly$flavors)
leafly_flav <- separate(leafly, flavors, c("flavor_1", "flavor_2", "flavor_3"), sep = (";"), fill = "right")

#create a variable in leafly which counts how many flavors that strain has
leafly$num_flavs <- str_count(leafly_f_num, pattern = "[0-9]\\.")

#create a data frame of all the flavors
flavs <- list()
flavs <- as.vector(levels(names_fla$flavors))
flavs <- as.data.frame(flavs[-1])

####one-hot encoding for leafly$flavors####
#execute these steps for each flavor 
flavors <- list() #create an empty list
numflav <- 1
for (i in 1:nrow(flavs)){
  this_strain <- allconditions[,i]
  this_strains <- this_strains[!is.na(this_strains)]
  temp <-  rep.int(numflav, leafly$num_flavs[i])
  print(temp)
  names(temp) <- as.vector.factor(leafly$flavors[i])
  print(names(temp))
  flavors[[leafly$flavors[i]]] <- temp
  print(flavors)
}      
flavors_final <- do.call(smartbind, as.list(flavs))
flavors_final$strains <- as.character(leafly$name)
  