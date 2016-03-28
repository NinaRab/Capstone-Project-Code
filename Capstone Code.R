#load libraries, set dataFolder
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)
library(gtools)
dataFolder <- "C:/Users/Nina/Desktop/datafoundations/Capstone Project/"

#load data gathered related to my capstone project
allprices <- read_excel(paste0(dataFolder, "all_prices.xlsx"))
leafly1 <- read.csv(paste0(dataFolder, "leafly_1.csv"))
leafly1761 <- read.csv(paste0(dataFolder, "leafly_1761.csv"))
conditions <- read.csv(paste0(dataFolder, "conditions_strains.csv"))
conditions1 <- read.csv(paste0(dataFolder, "conditions1.csv"))

#join leafly1 & leafly1761 and conditions & conditions1 by rows
leafly <- as.data.frame(bind_rows(leafly1, leafly1761))
allconditions <- as.data.frame(bind_rows(conditions, conditions1))

#clean leafly and allconditions datasets of extraneous data
leafly$parents_link <- NULL
leafly$parents_link._source <- NULL
leafly$parents._source <- NULL
leafly$parents <- NULL
leafly$recommendations_notes <- NULL
leafly$grow_info <- NULL
allconditions$pageUrl <- NULL
allconditions$strains <- NULL

#remove duplicate rows
leafly <- distinct(leafly)
allconditions <- distinct(allconditions)

#parse leafly data to contain succinct, relevant data
leafly$number_reviews <- gsub("[0-9](\\.*?)([0-9]*?\\|)|Reviews", "", leafly$number_reviews)


#parse allconditions data to contain succinct, relevant data
allconditions$condition <- gsub("Cannabis Strains That Help With|Best Cannabis Strains to Relieve", "", allconditions$condition)
allconditions$strains._source <- gsub("hybrid", "", allconditions$strains._source)
allconditions$strains._source <- gsub("sativa", "", allconditions$strains._source)
allconditions$strains._source <- gsub("indica", "", allconditions$strains._source)
allconditions$strains._source <- gsub("edible", "", allconditions$strains._source)

#reorder leafly data so strain name is first
leafly <- select(leafly, name, everything())

#rename pageUrl to strain.type
leafly <- rename(leafly, replace = c("pageUrl" = "strain.type"))

#save leafly$pageUrl to use for combining leafly with allconditions
leafly$pageUrl <- leafly$strain.type
leafly$pageUrl <- gsub("https://www.leafly.com/[a-z0-9-]+/", "", leafly$pageUrl)

#parse leafly$strain.type to get the strain type
leafly$strain.type <- gsub("https://www.leafly.com/|(/[a-z0-9-]+)", "", leafly$strain.type)

###get allconditions data in order for one-hot encoding###

#separate allconditions strains (each strain gets its own column) 
allconditions <- separate(allconditions, strains._source, c(1:56), sep = "; //")
allconditions$`1` <- gsub("//", "", allconditions$`1`)
allconditions <- as.data.frame(t(allconditions))

#save transposed allconditions data to allconditions_tran
allconditions_tran <- as.data.frame(t(allconditions))

#create a vector of condition names from allconditions_tran to use as labels for allconditions data
names(allconditions) <- as.vector(allconditions_tran$condition)

#remove first row from allconditions (since it is really the label for each column)
allconditions <- allconditions[-1,]

numStrains <- 1
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
#smartbind each strain (combine the strains list to the strains_final data frame and put NA's where data is missing)
strains_final <- data.frame(do.call(smartbind, strains)) 

#transpose strains_final to be able to combine with leafly data and apply condition names
strains_final <- data.frame(t(data.frame(strains_final)))
names(strains_final) <- as.character(allconditions_tran$condition)
strains_final$pageUrl <- row.names(strains_final)#add strain names to new column in strains_final

######get leafly$flavors data in order for one-hot encoding###
###remove numbers from leafly$flavors and then separate each
leafly$flavors <- gsub("[0-9]\\.", "", leafly$flavors)
leafly$flavors <- sub(" ", "", leafly$flavors)
leafly$flavors <- gsub("  ", ";", leafly$flavors)
leafly_flav <- select(leafly, 1:2) %>% separate(flavors, c("flavor_1", "flavor_2", "flavor_3"), sep = (";"))
leafly_flav$flavor_1[leafly_flav$flavor_1==""] <- NA
t_leafly_flav <- as.data.frame(t(leafly_flav))
names(t_leafly_flav) <- as.character(leafly$name)
t_leafly_flav <- t_leafly_flav[-1,]

####one-hot encoding for leafly$flavors####
#execute these steps for each flavor 
flav_list <- list() #create an empty list
numflav <- 1
for (i in 1:nrow(leafly)){
  this_flavor <- t_leafly_flav[,i]
  temp <-  rep.int(numflav, length(this_flavor))
  names(temp) <- as.character(this_flavor)
  flav_list[[leafly$name[i]]] <- temp
}     
flavors_final <- data.frame(do.call(smartbind, flav_list))

#removes columnsnamed NA
flavors_final$NA. <- NULL
flavors_final[is.na(flavors_final)] <- 0

#merge flavors_final$minty and flavors_final$mint (since it's the same thing)
flavors_final$mint <- flavors_final$Minty + flavors_final$Mint
flavors_final$Mint <- NULL
flavors_final$Minty <- NULL

#add flavors_final$name to full join it with leafly
flavors_final$name <- row.names(flavors_final)
colnames(flavors_final) <- paste("flvr", colnames(flavors_final), sep = "_")
flavors_final <- rename(flavors_final, replace = c("flvr_name" = "name"))
leafly <- full_join(leafly, flavors_final, by = "name")

#####

######get negatives data in order for one-hot encoding#####
negatives <- read.csv(paste0(dataFolder, "negatives.csv"))

#select relevant columns (instead of parsing based on words)
negatives <- select(negatives, 1:2)

#combine negatives with leafly data 
#(to make sure strain names match up with their own negatives)
leafly <- full_join(leafly, negatives, by = "name")
leafly <- distinct(leafly)

#separate and parse leafly$negatives so each negative is in its own column (in a new data frame)
leafly$negatives <- gsub(" ", "", leafly$negatives)
leafly_neg <- select(leafly, 1,57) %>% separate(negatives, c("neg_1", "neg_2", "neg_3", "neg_4", "neg_5"), sep = (";"))

#encode blank values as NA
leafly_neg$neg_1[leafly_neg$neg_1==""] <- NA

#create a transposed data frame (t_leafly_neg) to use for encoding
t_leafly_neg <- as.data.frame(t(leafly_neg))
names(t_leafly_neg) <- as.character(leafly$name)
t_leafly_neg <- t_leafly_neg[-1,]

####one-hot encoding for negatives####
negative_list <- list()
numNeg <- 1
for (i in 1:nrow(leafly)){
  this_neg <- t_leafly_neg[,i]
  temp <- rep.int(numNeg, length(this_neg))
  names(temp) <- as.character(this_neg)
  temp <- data.frame(t(temp))
  negative_list[[leafly$name[i]]] <- temp
}

negs_final <- data.frame(do.call(smartbind, negative_list))
negs_final$name <- row.names(negs_final)

#change NA's in negs_final to O
negs_final[is.na(negs_final)] <- 0

#remove columns named NA
negs_final$NA. <- NULL
negs_final$NA..1 <- NULL
negs_final$NA..2 <- NULL
negs_final$NA..3 <- NULL
negs_final$NA..4 <- NULL

#add neg_ to each column name in negs_final 
#(to be able to differentiate between negatives and other categories)
colnames(negs_final) <- paste("neg", colnames(negs_final), sep = "_")

#full join negs_final with leafly again
negs_final <- rename(negs_final, replace = c("neg_name" = "name"))
leafly <- full_join(leafly, negs_final, by = "name")

#remove leafly columns which have already been encoded
leafly$flavors <- NULL
leafly$negatives <- NULL
leafly <- distinct(leafly) 

#######

effects <- read.csv(paste0(dataFolder, "effects_only.csv"))

#select relevant columns (instead of parsing based on words)
effects <- select(effects, 1:2)

#combine effects with leafly data 
#(to make sure strain names match up with their own effects)
leafly <- full_join(leafly, effects, by = "name")
leafly <- distinct(leafly)

#separate and parse leafly$effects so each effect is in its own column (in a new data frame)
leafly$effects <- gsub(" ", ".", leafly$effects)
leafly_fx <- select(leafly, 1,62) %>% separate(effects, c("fx_1", "fx_2", "fx_3", "fx_4", "fx_5"), sep = (";."))

#encode blank values as NA
leafly_fx$fx_1[leafly_fx$fx_1==""] <- NA

#create a transposed data frame (t_leafly_fx) to use for encoding
t_leafly_fx <- as.data.frame(t(leafly_fx))
names(t_leafly_fx) <- as.character(leafly$name)
t_leafly_fx <- t_leafly_fx[-1,]

####one-hot encoding for effects####
fx_list <- list()
numfx <- 1
for (i in 1:nrow(leafly)){
  this_fx <- t_leafly_fx[,i]
  temp <- rep.int(numfx, length(this_fx))
  names(temp) <- as.character(this_fx)
  temp <- data.frame(t(temp))
  fx_list[[leafly$name[i]]] <- temp
}

fx_final <- data.frame(do.call(smartbind, fx_list))
fx_final$name <- row.names(fx_final)

#change NA's in fx_final to 0
fx_final[is.na(fx_final)] <- 0

#remove columns named NA
fx_final$NA. <- NULL
fx_final$NA..1 <- NULL
fx_final$NA..2 <- NULL
fx_final$NA..3 <- NULL
fx_final$NA..4 <- NULL

#add fx_ to each column name in fx_final 
#(to be able to differentiate between negatives and other categories)
colnames(fx_final) <- paste("fx", colnames(fx_final), sep = "_")

#full join fx_final with leafly again
fx_final <- rename(fx_final, replace = c("fx_name" = "name"))
leafly <- full_join(leafly, fx_final, by = "name")

#remove leafly columns which have already been encoded
leafly$effects <- NULL
leafly <- distinct(leafly) 
#####

#####get medical data ready for one-hot encoding
medical <- read.csv(paste0(dataFolder, "medical.csv"))

#select relevant columns (instead of parsing based on words)
medical <- select(medical, 1:2)

#combine medical with leafly data 
#(to make sure strain names match up with their own medical conditions)
leafly <- full_join(leafly, medical, by = "name")
leafly <- distinct(leafly)

#separate and parse leafly$medical so each medical condition is in its own column (in a new data frame)
leafly$medical <- gsub(" ", "", leafly$medical)
leafly_med <- select(leafly, 1,89) %>% separate(medical, c("med_1", "med_2", "med_3", "med_4", "med_5"), sep = (";"))

#encode blank values as NA
leafly_med$med_1[leafly_med$med_1==""] <- NA

#create a transposed data frame (t_leafly_med) to use for encoding
t_leafly_med <- as.data.frame(t(leafly_med))
names(t_leafly_med) <- as.character(leafly$name)
t_leafly_med <- t_leafly_med[-1,]

####one-hot encoding for medical####
med_list <- list()
nummed <- 1
for (i in 1:nrow(leafly)){
  this_med <- t_leafly_med[,i]
  temp <- rep.int(nummed, length(this_med))
  names(temp) <- as.character(this_med)
  temp <- data.frame(t(temp))
  med_list[[leafly$name[i]]] <- temp
}

med_final <- data.frame(do.call(smartbind, med_list))
med_final$name <- row.names(med_final)

#change NA's in med_final to O
med_final[is.na(med_final)] <- 0

#remove columns named NA
med_final$NA. <- NULL
med_final$NA..1 <- NULL
med_final$NA..2 <- NULL
med_final$NA..3 <- NULL
med_final$NA..4 <- NULL

#add med_ to each column name in med_final 
#(to be able to differentiate between negatives and other categories)
colnames(med_final) <- paste("med", colnames(med_final), sep = "_")

med_final <- rename(med_final, replace = c("med_name" = "name"))
#full join med_final with leafly again
leafly <- full_join(leafly, med_final, by = "name")

#remove leafly columns which have already been encoded
leafly$medical <- NULL
leafly <- distinct(leafly) 

######

#### get leafly$parents data ready for one-hot encoding####
parents <- select(leafly, 1, 6) %>% separate(parents._alt, c("p1", "p2", "p3", "p4", "p5", "p6", "p7"), sep = "; ")
#encode blank values as NA
parents$p1[parents$p1==""] <- NA

#create a transposed data frame (t_parents) to use for encoding
t_parents <- as.data.frame(t(parents))
names(t_parents) <- as.character(leafly$name)
t_parents <- t_parents[-1,]

####one-hot encoding for parents####
parent_list <- list()
numpar <- 1
for (i in 1:nrow(leafly)){
  this_parent <- t_parents[,i]
  temp <- rep.int(numpar, length(this_parent))
  names(temp) <- as.character(this_parent)
  temp <- data.frame(t(temp))
  parent_list[[leafly$name[i]]] <- temp
}

parent_final <- data.frame(do.call(smartbind, parent_list))
parent_final$name <- row.names(parent_final)

#change NA's in parent_final to O
parent_final[is.na(parent_final)] <- 0

#remove columns named NA
parent_final$NA. <- NULL
parent_final$NA..1 <- NULL
parent_final$NA..2 <- NULL
parent_final$NA..3 <- NULL
parent_final$NA..4 <- NULL
parent_final$NA..5 <- NULL
parent_final$NA..6 <- NULL

#add prnts_ to each column name in parent_final 
#(to be able to differentiate between negatives and other categories)
colnames(parent_final) <- paste("prnts", colnames(parent_final), sep = "_")

parent_final <- rename(parent_final, replace = c("prnt_name" = "name"))
#full join parent_final with leafly again
leafly <- full_join(leafly, parent_final, by = "name")

#remove leafly columns which have already been encoded
leafly$parents._alt <- NULL
leafly <- distinct(leafly) 
######

####get leafly$strain.type ready for one-hot encoding and encode
strain_type <- as.data.frame(select(leafly, 1, 6))
strain_type$strain.type <- tolower(as.character(strain_type$strain.type))
type_list <- list()
numtype <- 1
for (i in 1:nrow(leafly)){
  this_type <- strain_type$strain.type[i]
  temp <- rep.int(numtype, length(this_type))
  names(temp) <- as.character(this_type)
  type_list[[leafly$name[i]]] <- temp
}
type_final <- data.frame(do.call(smartbind, type_list))
type_final$name <- row.names(type_final)

#remove columns named NA
type_final$NA. <- NULL

#add type_ to each column name in type_final 
#(to be able to differentiate between negatives and other categories)
colnames(type_final) <- paste("type", colnames(type_final), sep = "_")

type_final <- rename(type_final, replace = c("type_name" = "name"))
#full join type_final with leafly again
leafly <- full_join(leafly, type_final, by = "name")

#remove leafly columns which have already been encoded
leafly$strain.type <- NULL
leafly <- distinct(leafly)

####get leafly$most_popular_in data ready for one-hot encoding####
pop_locations <- select(leafly, 1, 5)
pop_locations$most_popular_in <- gsub(",Spain", ",SP", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub(",Netherlands", ",NL", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("[A-Z]*(\\s)", "--", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("Spain", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("Netherlands", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("[A-Z]{2}", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("\\;\\-", "\\,\\-", pop_locations$most_popular_in)
pop_locations <- separate(pop_locations, most_popular_in, c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11", "L12"), sep = "(\\,(\\-)+)+")
pop_locations[pop_locations==""] <- NA
pop_locations[['L12']] <- NULL
pop_locations[['L11']] <- NULL

#create a transposed data frame (t_poploc) to use for encoding
t_poploc <- as.data.frame(t(pop_locations))
names(t_poploc) <- as.character(leafly$name)
t_poploc<- t_poploc[-1,]

####one-hot encoding for parents####
poploc_list <- list()
numloc <- 1
for (i in 1:nrow(leafly)){
  this_location <- t_poploc[,i]
  temp <- rep.int(numloc, length(this_location))
  names(temp) <- as.character(this_location)
  temp <- data.frame(t(temp))
  poploc_list[[leafly$name[i]]] <- temp
}

locations_final <- data.frame(do.call(smartbind, poploc_list))
locations_final$name <- row.names(locations_final)

#change NA's in locations_final to O
locations_final[is.na(locations_final)] <- 0

#remove columns named NA
locations_final$NA. <- NULL
locations_final$NA..1 <- NULL
locations_final$NA..2 <- NULL
locations_final$NA..3 <- NULL
locations_final$NA..4 <- NULL
locations_final$NA..5 <- NULL
locations_final$NA..6 <- NULL
locations_final$NA..7 <- NULL
locations_final$NA..8 <- NULL
locations_final$NA..9 <- NULL

#remove duplicates (entries that had the same location more than once, which contain a digit in the name)
locations_final <- select(locations_final, -matches("[0-9]"))

#add loc_ to each column name in locations_final 
#(to be able to differentiate between locations and other categories)
colnames(locations_final) <- paste("loc", colnames(locations_final), sep = "_")

locations_final <- rename(locations_final, replace = c("loc_name" = "name"))
#full join locations_final with leafly 
leafly <- full_join(leafly, locations_final, by = "name")

#remove leafly columns which have already been encoded
leafly$most_popular_in <- NULL
leafly <- distinct(leafly)

#########
#paste cnd_ (for condition) to the beginning of each variable name in strains_final
colnames(strains_final) <- paste("cnd", colnames(strains_final), sep = "_")

#full join strains_final with leafly
strains_final <- rename(strains_final, replace = c("cnd_pageUrl" = "pageUrl"))
strains_final[is.na(strains_final)] <- 0 
leafly <- left_join(leafly, strains_final, by = "pageUrl")

#remove encoded columns
leafly$pageUrl <- NULL

#remove NA's
leafly[is.na(leafly)] <- 0

#keep NA's for missing number_reviews
leafly$number_reviews[leafly$number_reviews==0] <- NA

#remove non-unique rows
leafly <- distinct(leafly)

#create a popularity variable from number_reviews and review
leafly$popularity <- as.numeric(leafly$number_reviews)*as.numeric(leafly$review)
leafly$number_reviews <- NULL
leafly$review <- NULL
