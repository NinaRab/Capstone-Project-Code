#load ggplot2, set dataFolder
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(gtools)
dataFolder <- "C:/Users/Nina/Desktop/datafoundations/Capstone Project/"

#load all the data gathered related to my capstone project
allprices <- read_excel(paste0(dataFolder, "all_prices.xlsx"))
leafly1 <- read.csv(paste0(dataFolder, "leafly_1.csv"))
leafly1761 <- read.csv(paste0(dataFolder, "leafly_1761.csv"))
conditions <- read.csv(paste0(dataFolder, "conditions_strains.csv"))
conditions1 <- read.csv(paste0(dataFolder, "conditions1.csv"))

#look at structure of each dataset
str(allprices)
str(leafly1)
str(leafly1761)
str(conditions)
str(conditions1)

#join leafly1 & leafly1761 and conditions & conditions1 by rows
leafly <- as.data.frame(bind_rows(leafly1, leafly1761))
allconditions <- as.data.frame(bind_rows(conditions, conditions1))


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


#parse allconditions data to contain succinct, relevant data
allconditions$condition <- gsub("Cannabis Strains That Help With|Best Cannabis Strains to Relieve", "", allconditions$condition)
allconditions$strains._source <- gsub("hybrid", "", allconditions$strains._source)
allconditions$strains._source <- gsub("sativa", "", allconditions$strains._source)
allconditions$strains._source <- gsub("indica", "", allconditions$strains._source)
allconditions$strains._source <- gsub("edible", "", allconditions$strains._source)

#reorder leafly data so strain name is first
leafly <- select(leafly, name, everything())

#rename pageUrl to strain.type
leafly <- rename(leafly, strain.type = pageUrl)

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

#create a vector of 1, and a vector of the number of strains for each condition
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
flavors_final <- do.call(smartbind, flav_list)
flavors_final$name <- row.names(flavors_final)

######get negatives data in order for one-hot encoding#####
negatives <- read.csv(paste0(dataFolder, "negatives.csv"))

#separate negatives into separate columns (instead of parsing based on words)
negatives <- select(negatives, 1:2) %>% separate(negatives, c("neg_1", "neg_2", "neg_3", "neg_4", "neg_5"), sep = ";")

#combine negatives with leafly data
leafly <- full_join(leafly, negatives, by = "name")

#encode blank values as NA
leafly$neg_1[leafly$neg_1==""] <- NA

#create a transposed data frame (t_negatives) to use for encoding
S_negatives <- select(leafly, 1, 11:15)
t_negatives <- as.data.frame(t(S_negatives))
names(t_negatives) <- as.character(leafly$name)
t_negatives <- t_negatives[-1,]

####one-hot encoding for negatives####
negative_list <- list()
numNeg <- 1
for (i in 1:nrow(leafly)){
  this_neg <- unique(t_negatives[,i])
  this_neg <- this_neg[!is.na(this_neg)]
  temp <- rep.int(numNeg, length(this_neg))
  names(temp) <- as.character(this_neg)
  negative_list[[leafly$name[i]]] <- temp
}

negs_final <- do.call(smartbind, negative_list)
negs_final$name <- row.names(negs_final)

#####get effects_only data ready for one-hot encoding####
effects_only <- read.csv(paste0(dataFolder, "effects_only.csv"))
effects_only <- select(effects_only, 1:2) %>% separate(effects, c("e1", "e2", "e3", "e4", "e5"), sep = ";")
leafly <- full_join(leafly, effects_only, by = "name")
leafly$e1[leafly$e1==""] <- NA
s_effect <- select (leafly, 1, 16:20)
t_effects <- as.data.frame(t(s_effect))
names(t_effects) <- as.character(s_effect$name)
t_effects <- t_effects[-1,]

####one-hot encoding for effects_only####
fx_list <- list() #create an empty list
numfx <- 1
for (i in 1:nrow(leafly)){
  this_effect <- unique(t_effects[ ,i])
  temp <-  rep.int(numfx, length(this_effect))
  names(temp) <- as.character(this_effect)
  fx_list[[leafly$name[i]]] <- temp
}     
fx_final <- do.call(smartbind, fx_list)
fx_final$name <- row.names(fx_final)

######

#####get medical data ready for one-hot encoding
medical <- read.csv(paste0(dataFolder, "medical.csv"))
medical <- select(medical, 1:2) %>% separate(medical, c("m1", "m2", "m3", "m4", "m5"), sep = ";")
leafly <- full_join(leafly, medical, by = "name")
leafly$m1[leafly$m1==""] <- NA
S_medical <- select(leafly, 1, 21:25)
t_medical <- as.data.frame(t(S_medical))
names(t_medical) <- as.character(leafly$name)
t_medical <- t_medical[-1,]

####one-hot encoding for effects_only####
med_list <- list() #create an empty list
nummed <- 1
for (i in 1:nrow(leafly)){
  this_med <- t_medical[,i]
  temp <-  rep.int(nummed, length(this_med))
  names(temp) <- as.character(this_med)
  med_list[[leafly$name[i]]] <- temp
}     
med_final <- do.call(smartbind, med_list)
med_final$name <- row.names(med_final)

######

#### get leafly$parents data ready for one-hot encoding####
parents <- select(leafly, 1, 7) %>% separate(parents._alt, c("p1", "p2", "p3", "p4", "p5", "p6", "p7"), sep = ";")
leafly <- full_join(leafly, parents, by = "name")
leafly$p1[leafly$p1==""] <- NA
leafly <- distinct(leafly)
s_parents <- select(leafly, 1, 26:32)
t_parents <- as.data.frame(t(s_parents))
names(t_parents) <- as.character(leafly$name)
t_parents <- t_parents[-1, ]

####one-hot encoding for leafly$parents data####
par_list <- list()
numpar <- 1
for (i in 1:nrow(leafly)){
  this_par <- t_parents[,i]
  temp <- rep.int(numpar, length(this_par))
  names(temp) <- as.character(this_par)
  par_list[[leafly$name[i]]] <- temp
}
par_final <- do.call(smartbind, par_list)
par_final$name <- row.names(par_final)
######

####get leafly$strain.type ready for one-hot encoding and encode
strain_type <- as.data.frame(select(leafly, 1, 8))
strain_type$strain.type <- tolower(as.character(strain_type$strain.type))
type_list <- list()
numtype <- 1
for (i in 1:nrow(leafly)){
  this_type <- strain_type$strain.type[i]
  temp <- rep.int(numtype, length(this_type))
  names(temp) <- as.character(this_type)
  type_list[[leafly$name[i]]] <- temp
}
type_final <- do.call(smartbind, type_list)
type_final$name <- row.names(type_final)

####get leafly$most_popular_in data ready for one-hot encoding####
pop_locations <- select(leafly, 1, 6)
pop_locations$most_popular_in <- gsub(",Spain", ",SP", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub(",Netherlands", ",NL", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("[A-Z]*(\\s)", "---", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("Spain", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("Netherlands", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("[A-Z]{2}", "-", pop_locations$most_popular_in)
pop_locations$most_popular_in <- gsub("\\;\\-", "\\,\\-", pop_locations$most_popular_in)
pop_locations <- separate(pop_locations, most_popular_in, c("l1", "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9", "l10", "l11", "l12"), sep = "(\\,(\\-)+)+")
pop_locations[pop_locations==""] <- NA
pop_locations[['l12']] <- NULL
pop_locations[['l11']] <- NULL

leafly <- full_join(leafly, pop_locations, by = "name")
leafly <- distinct(leafly)
s_locations <- select(leafly, 1, 33:42)
t_locations <- as.data.frame(t(s_locations))
names(t_locations) <- as.character(leafly$name)
t_locations <- t_locations[-1, ]

####one-hot encoding for pop_locations data####
loc_list <- list()
numloc <- 1
for (i in 1:nrow(leafly)){
  this_loc <- unique(t_locations[,i])
  temp <- rep.int(numloc, length(this_loc))
  names(temp) <- as.character(this_loc)
  loc_list[[leafly$name[i]]] <- temp
}
loc_final <- do.call(smartbind, loc_list)
loc_final$name <- row.names(loc_final)

#########
####combine all the one-hot encoded data
leafly_OHE <- select(leafly, name, number_reviews, review, pageUrl)
leafly_OHE <- full_join(leafly_OHE, strains_final, by = "pageUrl")
leafly_OHE <- full_join(leafly_OHE, flavors_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, negs_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, fx_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, med_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, par_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, type_final, by = "name")
leafly_OHE <- full_join(leafly_OHE, loc_final, by = "name")
