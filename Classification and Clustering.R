#load libraries, set dataFolder
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)
library(gtools)
dataFolder <- "<your file path here>"

#load data gathered related to my capstone project
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

#####one hot encoding for conditions for each strain####
#execute these steps for each condition 
strains <- list() #create an empty list
for (i in 1:nrow(allconditions_tran)){
  this_strains <- unique(allconditions[,i]) #remove repeat strains for each condition
  this_strains <- this_strains[!is.na(this_strains)] #get rid of NAs
  temp <- rep.int(1, length(this_strains)) #create a vector of 1's of length = number of strains for that condition
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
for (i in 1:nrow(leafly)){
  this_flavor <- t_leafly_flav[,i]
  temp <-  rep.int(1, length(this_flavor))
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
for (i in 1:nrow(leafly)){
  this_neg <- t_leafly_neg[,i]
  temp <- rep.int(1, length(this_neg))
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
for (i in 1:nrow(leafly)){
  this_fx <- t_leafly_fx[,i]
  temp <- rep.int(1, length(this_fx))
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
for (i in 1:nrow(leafly)){
  this_med <- t_leafly_med[,i]
  temp <- rep.int(1, length(this_med))
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
for (i in 1:nrow(leafly)){
  this_parent <- t_parents[,i]
  temp <- rep.int(1, length(this_parent))
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
#(to be able to differentiate between parents and other categories)
colnames(parent_final) <- paste("prnts", colnames(parent_final), sep = "_")

parent_final <- rename(parent_final, replace = c("prnts_name" = "name"))
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
for (i in 1:nrow(leafly)){
  this_type <- strain_type$strain.type[i]
  temp <- rep.int(1, length(this_type))
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

####one-hot encoding for popular locations####
poploc_list <- list()
for (i in 1:nrow(leafly)){
  this_location <- t_poploc[,i]
  temp <- rep.int(1, length(this_location))
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

#look at column names to make sure there are no duplicates in each category
#and to merge 2 columns if necessary
names(leafly[,order(colnames(leafly),decreasing=TRUE)])
#since fx_Headache and fx_Headaches are similar, merge both into fx_Headaches
leafly$fx_Headaches <- leafly$fx_Headache + leafly$fx_Headaches
leafly$fx_Headache <- NULL

#####EDA#####
#get summary data on leafly and save for later
leafly_summary <- data.frame(summary(leafly))
leafly_means <- select(leafly, -name, -strain_highlights) %>% summarise_each(funs(mean))

###univariate EDA by category###

#separate each category of qualities into its own data frame & get a mean value for each quality
cnds <- select(leafly, starts_with("cnd_")) %>% summarise_each(funs(mean))
meds <- select(leafly, starts_with("med_")) %>% summarise_each(funs(mean))
fx <- select(leafly, starts_with("fx_")) %>% summarise_each(funs(mean))
negs <- select(leafly, starts_with("neg_")) %>% summarise_each(funs(mean))
locs <- select(leafly, starts_with("loc_")) %>% summarise_each(funs(mean))
flvrs <- select(leafly, starts_with("flvr_")) %>% summarise_each(funs(mean))
prnts <- select(leafly, starts_with("prnts_")) %>% summarise_each(funs(mean))

#transpose means to be able to see more clearly
t_negs <- data.frame(t(negs))
names(t_negs) <- ("mean")
t_cnds <- data.frame(t(cnds))
names(t_cnds) <- ("mean")
t_meds <- data.frame(t(meds))
names(t_meds) <- ("mean")
t_fx <- data.frame(t(fx))
names(t_fx) <- ("mean")
t_locs <- data.frame(t(locs))
names(t_locs) <- ("mean")
t_flvrs <- data.frame(t(flvrs))
names(t_flvrs) <- ("mean")
t_prnts <- data.frame(t(prnts))
names(t_prnts) <- ("mean")

#create data frame of leafly unencoded to be able to do graphical EDA# 
#for a better sense of the data#

#create data frame of leafly unencoded
leafly_unenc <- as.data.frame(bind_rows(leafly1, leafly1761))

leafly_unenc$parents_link <- NULL
leafly_unenc$parents_link._source <- NULL
leafly_unenc$parents._source <- NULL
leafly_unenc$parents <- NULL
leafly_unenc$recommendations_notes <- NULL
leafly_unenc$grow_info <- NULL

leafly_unenc <- distinct(leafly_unenc)

leafly_unenc$number_reviews <- gsub("[0-9](\\.*?)([0-9]*?\\|)|Reviews", "", leafly_unenc$number_reviews)

#reorder leafly_unenc data so strain name is first
leafly_unenc <- select(leafly_unenc, name, everything())

#rename pageUrl to strain.type
leafly_unenc <- rename(leafly_unenc, replace = c("pageUrl" = "strain.type"))

#save leafly_unenc$pageUrl to use for combining leafly with allconditions
leafly_unenc$pageUrl <- leafly_unenc$strain.type
leafly_unenc$pageUrl <- gsub("https://www.leafly.com/[a-z0-9-]+/", "", leafly_unenc$pageUrl)

#parse leafly_unenc$strain.type to get the strain type
leafly_unenc$strain.type <- gsub("https://www.leafly.com/|(/[a-z0-9-]+)", "", leafly_unenc$strain.type)

###remove numbers from leafly_unenc$flavors and then separate each
leafly_unenc$flavors <- gsub("[0-9]\\.", "", leafly_unenc$flavors)
leafly_unenc$flavors <- sub(" ", "", leafly_unenc$flavors)
leafly_unenc$flavors <- gsub("  ", ";", leafly_unenc$flavors)
leafly__unenc_flav <- select(leafly_unenc, 1:2) %>% separate(flavors, c("flavor_1", "flavor_2", "flavor_3"), sep = (";"))
leafly__unenc_flav$flavor_1[leafly__unenc_flav$flavor_1==""] <- NA

negatives <- read.csv(paste0(dataFolder, "negatives.csv"))

#select relevant columns (instead of parsing based on words)
negatives <- select(negatives, 1:2)

#separate and parse leafly$negatives so each negative is in its own column (in a new data frame)
negatives$negatives <- gsub(" ", "", negatives$negatives)
unenc_neg <- select(negatives, 1,2) %>% separate(negatives, c("neg_1", "neg_2", "neg_3", "neg_4", "neg_5"), sep = (";"))

#encode blank values as NA
unenc_neg$neg_1[unenc_neg$neg_1==""] <- NA

effects <- read.csv(paste0(dataFolder, "effects_only.csv"))

#select relevant columns (instead of parsing based on words)
effects <- select(effects, 1:2)

#separate and parse leafly$effects so each effect is in its own column (in a new data frame)
effects$effects <- gsub(" ", ".", effects$effects)
unenc_fx <- select(effects, 1,2) %>% separate(effects, c("fx_1", "fx_2", "fx_3", "fx_4", "fx_5"), sep = (";."))

#encode blank values as NA
unenc_fx$fx_1[unenc_fx$fx_1==""] <- NA

medical <- read.csv(paste0(dataFolder, "medical.csv"))

#select relevant columns (instead of parsing based on words)
medical <- select(medical, 1:2)

#separate and parse leafly$medical so each medical condition is in its own column (in a new data frame)
medical$medical <- gsub(" ", "", medical$medical)
unenc_med <- select(medical, 1,2) %>% separate(medical, c("med_1", "med_2", "med_3", "med_4", "med_5"), sep = (";"))

#encode blank values as NA
unenc_med$med_1[unenc_med$med_1==""] <- NA

#separate parents data
parents_unenc <- select(leafly_unenc, 1, 7) %>% separate(parents._alt, c("p1", "p2", "p3", "p4", "p5", "p6", "p7"), sep = "; ")
#encode blank values as NA
parents_unenc$p1[parents_unenc$p1==""] <- NA


loc_unenc <- select(leafly_unenc, 1, 6)
loc_unenc$most_popular_in <- gsub(",Spain", ",SP", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub(",Netherlands", ",NL", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub("[A-Z]*(\\s)", "--", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub("Spain", "-", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub("Netherlands", "-", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub("[A-Z]{2}", "-", loc_unenc$most_popular_in)
loc_unenc$most_popular_in <- gsub("\\;\\-", "\\,\\-", loc_unenc$most_popular_in)
loc_unenc<- separate(loc_unenc, most_popular_in, c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11", "L12"), sep = "(\\,(\\-)+)+")
loc_unenc[loc_unenc==""] <- NA
loc_unenc[['L12']] <- NULL

#remove extraneous columns
leafly_unenc$strain_highlights <- NULL
leafly_unenc$most_popular_in <- NULL
leafly_unenc$parents._alt <- NULL
leafly_unenc$flavors <- NULL

#prepare all conditions data for EDA
allconditions <- as.data.frame(bind_rows(conditions, conditions1))
#clean allconditions datasets of extraneous data
allconditions$pageUrl <- NULL
allconditions$strains <- NULL

#remove duplicate rows
allconditions <- distinct(allconditions)

#parse allconditions data to contain succinct, relevant data
allconditions$condition <- gsub("Cannabis Strains That Help With|Best Cannabis Strains to Relieve", "", allconditions$condition)
allconditions$strains._source <- gsub("hybrid", "", allconditions$strains._source)
allconditions$strains._source <- gsub("sativa", "", allconditions$strains._source)
allconditions$strains._source <- gsub("indica", "", allconditions$strains._source)
allconditions$strains._source <- gsub("edible", "", allconditions$strains._source)
allconditions$strains._source <- gsub("//", "", allconditions$strains._source)

#separate allcondition strains
allconditions <- separate(allconditions, strains._source, c(1:56), sep = "; ")
t_allconditions <- data.frame(t(allconditions))

names(t_allconditions) <- as.vector(allconditions_tran$condition)

#remove first row from allconditions (since it is really the label for each column)
t_allconditions <- t_allconditions[-1,]

strains <- list() #create an empty list
for (i in 1:nrow(allconditions_tran)){
  this_strains <- unique(t_allconditions[,i]) #remove repeat strains for each condition
  this_strains <- this_strains[!is.na(this_strains)] #get rid of NAs
  temp <- rep.int(allconditions_tran$condition[i], length(this_strains)) #create a vector of 1's of length = number of strains for that condition
  names(temp) <- as.character(unique(as.character(this_strains))) #set the names of the temp vector as the names of the strains
  temp <- t(data.frame(temp)) #transpose the temp data frame
  strains[[allconditions_tran$condition[i]]] <- temp #save the transposed temp data frame to strains, with the column header of what condition it is
}

strains_final <- data.frame(do.call(smartbind, strains)) 

for (i in 1:ncol(strains_final)){
  strains_final[[i]][is.na(strains_final[[i]])] <- 0
}

#transpose strains_final
t_strainsfinal <- data.frame(t(strains_final))
t_strainsfinal$pageUrl <- rownames(t_strainsfinal)

#full join the different categories with leafly_unenc by name
leafly_unenc <- left_join(leafly_unenc, t_strainsfinal, by = "pageUrl")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc$pageUrl <- NULL
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, unenc_neg, by = "name")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, unenc_med, by = "name")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, unenc_fx, by = "name")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, leafly__unenc_flav, by = "name")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, parents_unenc, by = "name")
leafly_unenc <- distinct(leafly_unenc)
leafly_unenc <- left_join(leafly_unenc, loc_unenc, by = "name")
leafly_unenc <- distinct(leafly_unenc)

#create a popularity variable from number_reviews and review
leafly_unenc$popularity <- as.numeric(leafly_unenc$number_reviews)*as.numeric(leafly_unenc$review)

#EDA for leafly_unenc#
qplot(data = leafly_unenc, x = popularity)

#put all effect category variables together in one graph
library(gridExtra)
n1 <- qplot(data = leafly_unenc, x = neg_1)  + theme(axis.text.x = element_text(angle = 90, hjust = 0))
n2 <- qplot(data = leafly_unenc, x = neg_2)  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
n3 <- qplot(data = leafly_unenc, x = neg_3)  + theme(axis.text.x = element_text(angle = 90, hjust = 0))
n4 <- qplot(data = leafly_unenc, x = neg_4)  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
n5 <- qplot(data = leafly_unenc, x = neg_5)  + theme(axis.text.x = element_text(angle = 90, hjust = 0))

grid.arrange(n1, n2, n3, n4, n5, ncol=2)

fx1 <- qplot(data = leafly_unenc, x = fx_1) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fx2 <- qplot(data = leafly_unenc, x = fx_2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fx3 <- qplot(data = leafly_unenc, x = fx_3) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fx4 <- qplot(data = leafly_unenc, x = fx_4) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fx5 <- qplot(data = leafly_unenc, x = fx_5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(fx1, fx2, fx3, fx4, fx5, ncol=1)  

m1 <- qplot(data = leafly_unenc, x = med_1) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
m2 <- qplot(data = leafly_unenc, x = med_2) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
m3 <- qplot(data = leafly_unenc, x = med_3) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
m4 <- qplot(data = leafly_unenc, x = med_4) + theme(axis.text.x = element_text(angle = 30, hjust = 1))
m5 <- qplot(data = leafly_unenc, x = med_5) + theme(axis.text.x = element_text(angle = 30, hjust = 1))

grid.arrange(m1, m2, m3, m4, m5, ncol=1)

flv1 <- qplot(data = leafly_unenc, x = flavor_1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
flv2 <- qplot(data = leafly_unenc, x = flavor_2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
flv3 <- qplot(data = leafly_unenc, x = flavor_3) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(flv1, flv2, flv3, ncol = 1)

pnt1 <- qplot(data = leafly_unenc, x = p1) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt2 <- qplot(data = leafly_unenc, x = p2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt3 <- qplot(data = leafly_unenc, x = p3) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt4 <- qplot(data = leafly_unenc, x = p4) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt5 <- qplot(data = leafly_unenc, x = p5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt6 <- qplot(data = leafly_unenc, x = p6) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
pnt7 <- qplot(data = leafly_unenc, x = p7) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(pnt1, pnt2, pnt3, pnt4, pnt5, pnt6, ncol = 1)

loc1 <- qplot(data = leafly_unenc, x = L1) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
loc2 <- qplot(data = leafly_unenc, x = L2) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
loc3 <- qplot(data = leafly_unenc, x = L3) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
loc4 <- qplot(data = leafly_unenc, x = L4) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc5 <- qplot(data = leafly_unenc, x = L5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc6 <- qplot(data = leafly_unenc, x = L6) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc7 <- qplot(data = leafly_unenc, x = L7) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc8 <- qplot(data = leafly_unenc, x = L8) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc9 <- qplot(data = leafly_unenc, x = L9) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc10 <- qplot(data = leafly_unenc, x = L10) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
loc11 <- qplot(data = leafly_unenc, x = L11) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, ncol = 1)

leafly_unenc$strain.type <- str_to_lower(leafly_unenc$strain.type)
qplot(data = leafly_unenc, x = strain.type)
### the fact that L11 and p7 each have only one strain, 
###tells me they are probably insignificant variables 
###(the same is also possibly true of p5 & p6, 
### each with 3 strains only)

###see if any of the condition - strain from the list have any strains in common with medical effects
by(select(leafly_unenc, X1:X40), leafly_unenc$med_1, summary)
by(select(leafly_unenc, X1:X40), leafly_unenc$med_2, summary)
by(select(leafly_unenc, X1:X40), leafly_unenc$med_3, summary)
by(select(leafly_unenc, X1:X40), leafly_unenc$med_4, summary)
by(select(leafly_unenc, X1:X40), leafly_unenc$med_5, summary)

#create a box & whisker plot popularity statistics for the different values of neg_1:neg_5
qplot(data = subset(leafly_unenc, !is.na(neg_1)), x = neg_1, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000)) 

qplot(data = subset(leafly_unenc, !is.na(neg_2)), x = neg_2, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))

qplot(data = subset(leafly_unenc, !is.na(neg_3)), x = neg_3, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))

qplot(data = subset(leafly_unenc, !is.na(neg_4)), x = neg_4, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))

qplot(data = subset(leafly_unenc, !is.na(neg_5)), x = neg_5, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))  #this appears like if a strain has a serious negative 
#like headaches or paranoia as the last negative, then that strain's popularity will be 
#much better (likely because it is not such a strong negative)

#create a box & whisker plot popularity statistics for the different values of fx_1:fx_5
qplot(data = subset(leafly_unenc, !is.na(fx_1)), x = fx_1, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,600))

qplot(data = subset(leafly_unenc, !is.na(fx_2)), x = fx_2, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,600))

qplot(data = subset(leafly_unenc, !is.na(fx_3)), x = fx_3, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,600))

qplot(data = subset(leafly_unenc, !is.na(fx_4)), x = fx_4, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,600))

qplot(data = subset(leafly_unenc, !is.na(fx_5)), x = fx_5, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,600))

#look at each variable's effect on popularity#
names(leafly)

####create a popularity boxplot for each variable in leafly#####
#separating the images into the different categories of variables#
library(reshape2)
#flavors
leafly_flavors <- select(leafly, flvr_Sweet:flvr_mint, popularity)
manip <- subset(leafly_flavors, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + 
  geom_boxplot(stat = 'boxplot') + facet_grid(.~variable) + 
  ylim(0, quantile(mmanip$popularity, 0.95)) +
  theme(strip.text.x = element_text(size = 12, angle = 90))

#negatives
leafly_negs <- select(leafly, neg_DryMouth:neg_Anxious, popularity)
manip <- subset(leafly_negs, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_grid(.~variable, labeller = label_wrap_gen(width = 5000, multi_line = TRUE)) + ylim(0, quantile(mmanip$popularity, 0.95)) +
  theme(strip.text.x = element_text(size = 12, angle = 90))
#effects(general)
leafly_effects <- select(leafly, fx_Relaxed:fx_Nausea, popularity)
manip <- subset(leafly_effects, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 20) + ylim(0, quantile(mmanip$popularity, 0.95))+
  theme(strip.text.x = element_text(size = 12, angle = 90))
#medical effects
leafly_medical <- select(leafly, med_Stress:med_Paranoid, popularity)
manip <- subset(leafly_medical, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 20) + ylim(0, quantile(mmanip$popularity, 0.95))+
  theme(strip.text.x = element_text(size = 10, angle = 90))
#parents
leafly_parents <- select(leafly, prnts_Afghani:prnts_Bay.11, popularity)
manip <- subset(leafly_parents, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 50) + ylim(0, quantile(mmanip$popularity, 0.75)) +
  theme(strip.text.x = element_text(size = 8, angle = 90))
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 50) + ylim(0, quantile(mmanip$popularity, 0.75))
#types of strains
leafly_types <- select(leafly, type_indica:type_edible, popularity)
manip <- subset(leafly_types, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_grid(.~variable) + ylim(0, quantile(mmanip$popularity, 0.95))
#popular locations
leafly_locations <- select(leafly, loc_Toronto:loc_Hermosa..Beach, popularity)
manip <- subset(leafly_locations, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 42) + ylim(0, quantile(mmanip$popularity, 0.75)) + +
  theme(strip.text.x = element_text(size = 8, angle = 90))
#medical conditions
leafly_conditions <- select(leafly, 948:987, popularity)
manip <- subset(leafly_conditions, !is.na(popularity)) 
mmanip <- melt(manip, id.vars = "popularity")
str(mmanip)
mmanip <- subset(mmanip, value == 1)
ggplot(mmanip, aes(x = value, y = popularity)) + geom_boxplot(stat = 'boxplot') + facet_wrap(~variable, ncol = 20) + ylim(0, quantile(mmanip$popularity, 0.95)) +
  theme(strip.text.x = element_text(size = 10, angle = 90))

table(leafly$popularity)

#add up all the strains for each variable (except popularity
names(leafly)
sumleafly <-select(leafly, -1, -2, -popularity)
leaflycounts <- t(apply(sumleafly, 2, sum))

#add up all the variables for each strain
t_leafly <- data.frame(t(sumleafly))
names(t_leafly) <- as.character(leafly$name)
straincounts <- t(apply(t_leafly, 2, sum))
leafly$straincounts <- t(straincounts)
leafly$straincounts
##correlation####
cor.test(leafly$popularity, leafly$straincounts)
cor.test(leafly$med_Stress, leafly$med_Pain, method = "kendall")
cor.test(leafly$med_Pain, leafly$med_Nausea, method = "kendall")
cor.test(leafly$med_Anxious, leafly$`cnd_ Anxiety`, method = "kendall")
cor.test(leafly$neg_Dizzy, leafly$type_indica, method = "kendall")
cor.test(leafly$neg_Paranoid, leafly$type_sativa)
cor.test(leafly$`cnd_ Anxiety`, leafly$`cnd_ Gastrointestinal Disorder`, method = "kendall")
cor.test(leafly$`cnd_ Anxiety`, leafly$`cnd_ Epilepsy`, method = "kendall")
cor.test(leafly$`cnd_ Anxiety`, leafly$`cnd_ Spasticity`, method = "kendall")
cor.test(leafly$`cnd_ Muscle Spasms`, leafly$`cnd_ Spasticity`, method = "kendall")
cor.test(leafly$neg_DryMouth, leafly$fx_Dry.Mouth, method = "kendall")
cor.test(leafly$fx_Dry.Mouth, leafly$med_DryMouth, method = "kendall")
cor.test(leafly$neg_DryMouth, leafly$fx_Dry.Mouth, method = "kendall")
### looks like the top strains for the top conditions (by popular strains) are just the same strains

##create a correlation matrix 
library(corrplot)
names(leafly)
CL <- (cor(select(leafly, 3:987)))
corr.matrix <- corrplot(CL, method = "color", type = "lower")

#select leafly fx and types and create correlation matrix
leafly_fx_type <- select(leafly, 56:81, type_indica:type_edible)
CL_fx_type <- cor(leafly_fx_type)
corr.matrix.fxtype <- corrplot(CL_fx_type, method = "color")

#select leafly negs and types and create correlation matrix
leafly_neg_type <- select(leafly, 50:55, type_indica:type_edible)
CL_neg_type <- cor(leafly_neg_type)
corr.matrix.negtype <- corrplot(CL_neg_type, method = "color")

#select leafly meds and types and create correlation matrix
leafly_med_type <- select(leafly, 82:99, type_indica:type_edible)
CL_med_type <- cor(leafly_med_type)
corr.matrix.medtype <- corrplot(CL_med_type, method = "color")

#select leafly cnds and types and create correlation matrix
leafly_cnd_type <- select(leafly, 948:987, type_indica:type_edible)
CL_cnd_type <- cor(leafly_cnd_type)
corr.matrix.cndtype <- corrplot(CL_cnd_type, method = "color")

#select leafly flvrs and types and create correlation matrix
leafly_flvr_type <- select(leafly, 3:49, type_indica:type_edible)
CL_flvr_type <- cor(leafly_flvr_type)
cor.matrix.flavtype <- corrplot(CL_flvr_type, method = "color")

#select leafly meds and cnds and create correlation matrix
leafly_meds_cnds <- select(leafly, 82:99, 948:987)
CL_meds_cnds <- cor(leafly_meds_cnds)
corr.matrix.medcnds <- corrplot(CL_meds_cnds, method = "color")

###
#####Clustering#####
#normalize popularity variable & straincounts variable
mean(!is.na(leafly$popularity))
sd(!is.na(leafly$popularity))
leafly$popularity <- (leafly$popularity - mean(!is.na(leafly$popularity)))/(sd(!is.na(leafly$popularity)))
leafly$popularity
mean(leafly$straincounts)
sd(leafly$straincounts)
leafly$straincounts <- (leafly$straincounts- mean(leafly$straincounts))/sd(leafly$straincounts)
leafly$straincounts

#remove strain highlights and any duplicate rows from leafly
leafly$strain_highlights <- NULL
leafly <- unique(leafly)
str(leafly)

#linear regression of populaarity vs. straincounts
countsReg <- lm(popularity ~ straincounts, data = leafly)
summary(countsReg)
countsReg$residuals
SSE <- sum(countsReg$residuals^2)
SSE
qplot(leafly$straincounts, leafly$popularity) + scale_y_log10()
####determine optimal number of clusters for hierarchical clustering###
library(GMD)
#calculate the distances between data points based on medical effects 
distances <- dist(leafly[81:98], method = "euclidean")
#cluster based on distances
clusterLeafly <- hclust(distances, method = "ward.D")
#plot dendrogram
plot(clusterLeafly)
#plot within cluster sum of squares for each number of clusters
css.obj <- css.hclust(distances, clusterLeafly)
plot(css.obj$k, css.obj$tss - css.obj$totbss, pch = 19,
     ylab = "WCSS(k)",xlab = "number of clusters k", cex.lab = 1.5)
##looks like 5 or 6 clusters will be the best fit (where the most obvious elbow is)
##alternatively, the rough estimate for appropriate number of clusters is k = (n/2)^-2
##which is approximately 30, so try k = 6 and k = 30

#assign each strain to a cluster
clusterGroups <- cutree(clusterLeafly, k = 6)


cluster1 <- data.frame(subset(leafly, clusterGroups == 1))
cluster2 <- data.frame(subset(leafly, clusterGroups == 2))
cluster3 <- data.frame(subset(leafly, clusterGroups == 3))
cluster4 <- data.frame(subset(leafly, clusterGroups == 4))
cluster5 <- data.frame(subset(leafly, clusterGroups == 5))
cluster6 <- data.frame(subset(leafly, clusterGroups == 6))
########
library(rpart)
library(rpart.plot)

###Regression Tree###
library(caTools)
set.seed(1000)
leafly[is.na(leafly)] <- 0
LeaflySelect <- select(leafly, - name)
split <- sample.split(LeaflySelect$popularity, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)


LeaflyTree<- rpart(popularity ~ ., data = train, minbucket = 25)
prp(LeaflyTree)
predictTree <- predict(LeaflyTree, test)
tree.SSE <- sum((predictTree - !is.na(test$popularity))^2)
tree.SSE

###linear regression###
LinReg <- lm(popularity ~ ., data = train)
summary(LinReg)

#see how well linear regression does on test data, 
#compared to regression tree
linreg.pred <- predict(LinReg, newdata = test)
linreg.SSE <- sum((linreg.pred - !is.na(test$popularity))^2)
linreg.SSE
#regression tree does better than linear regression on all variables


###build classification trees for each medical effect variable & test accuracy###
LeaflySelect <- select(leafly, - name)
split <- sample.split(LeaflySelect$med_Stress, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
LeaflyStress <- rpart(med_Stress ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(LeaflyStress)

#test classification tree model on test data
predictStress <- predict(LeaflyStress, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Stress, predictStress)
sum(diag(acc))/sum(acc)
#about 91% accuracy

library(ROCR)
predictROCStress <- predict(LeaflyStress, newdata = test)
predictROCStress

#compute ROC curve for StressTree
predStress <- prediction(predictROCStress [,2], test$med_Stress)
perf <- performance(predStress, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Stress == 1)
1378/(1378 + 381)
#about 78% accuracy 
#regresstion tree is better than guessing

#med_Pain
split <- sample.split(LeaflySelect$med_Pain, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
PainTree <- rpart(med_Pain ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(PainTree)

#test classification tree model on test data
predictPain <- predict(PainTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Pain, predictPain)
sum(diag(acc))/sum(acc)
#about 83% accuracy

library(ROCR)
predictROCPain <- predict(PainTree, newdata = test)
predictROCPain

#compute ROC curve for PaibTree
predPain <- prediction(predictROCPain [,2], test$med_Pain)
perf <- performance(predPain, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Pain == 1)
1225/(1225 + 534)
#classification tree for med_Stress is more than 10% better than always guessing 1 for med_Stress on all leafly data

#med_Nausea
split <- sample.split(LeaflySelect$med_Nausea, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
NauseaTree <- rpart(med_Nausea ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(NauseaTree)

#test classification tree model on test data
predictNausea <- predict(NauseaTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Nausea, predictNausea)
sum(diag(acc))/sum(acc)
#about 78% accuracy

predictROCNausea <- predict(NauseaTree, newdata = test)
predictROCNausea

#compute ROC curve for NauseaTree
predNausea <- prediction(predictROCNausea [,2], test$med_Nausea)
perf <- performance(predNausea, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Nausea == 1)
1342/(1342 + 417)
#classification tree is not much better than always guessing 0 for med_Nausea

#med_MuscleSpasms
split <- sample.split(LeaflySelect$med_MuscleSpasms, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
SpasmsTree <- rpart(med_MuscleSpasms ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(SpasmsTree)

#test classification tree model on test data
predictSpasms <- predict(SpasmsTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_MuscleSpasms, predictSpasms)
sum(diag(acc))/sum(acc)
#about 82% accuracy

predictROCSpasms <- predict(SpasmsTree, newdata = test)
predictROCSpasms

#compute ROC curve for NauseaTree
predSpasms <- prediction(predictROCSpasms [,2], test$med_MuscleSpasms)
perf <- performance(predSpasms, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_MuscleSpasms == 1)
1456/(1456 + 303)
#about 83% accuracy just guessing 0 for muscle spasms

#med_lackofAppetite
split <- sample.split(LeaflySelect$med_LackofAppetite, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
LappetiteTree <- rpart(med_LackofAppetite ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(LappetiteTree)

#test classification tree model on test data
predictLappetite <- predict(LappetiteTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_LackofAppetite, predictLappetite)
sum(diag(acc))/sum(acc)
#about 74% accuracy

predictROCLappetite <- predict(LappetiteTree, newdata = test)
predictROCLappetite

#compute ROC curve for lack of appetite
predLappetite <- prediction(predictROCLappetite [,2], test$med_LackofAppetite)
perf <- performance(predLappetite, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_LackofAppetite == 1)
1187/(1187 + 572)
#about 67% accuracy just guessing 0 for lack of appetite (med)

#med_insomnia
split <- sample.split(LeaflySelect$med_Insomnia, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
InsomniaTree <- rpart(med_Insomnia ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(InsomniaTree)

#test classification tree model on test data
predictInsomnia <- predict(InsomniaTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Insomnia, predictInsomnia)
sum(diag(acc))/sum(acc)
#about 78% accuracy

predictROCInsomnia <- predict(InsomniaTree, newdata = test)
predictROCInsomnia

#compute ROC curve
predInsomnia <- prediction(predictROCInsomnia [,2], test$med_Insomnia)
perf <- performance(predInsomnia, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Insomnia == 1)
979/(979 + 780)
#about 56% accuracy just guessing 0 for insomnia

#med_depression
split <- sample.split(LeaflySelect$med_Depression, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
DepressionTree <- rpart(med_Depression ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(DepressionTree)

#test classification tree model on test data
predictDepression <- predict(DepressionTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Depression, predictDepression)
sum(diag(acc))/sum(acc)
#about 82% accuracy

predictROCDeression <- predict(DepressionTree, newdata = test)
predictROCDeression

#compute ROC curve for depression tree
predDepression <- prediction(predictROCDeression [,2], test$med_Depression)
perf <- performance(predDepression, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Depression == 1)
1234/(1234 + 525)
#about 70% accuracy just guessing 0 for Depression

#med_headaches
split <- sample.split(LeaflySelect$med_Headaches, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
HeadacheTree <- rpart(med_Headaches ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(HeadacheTree)

#test classification tree model on test data
predictHeadache <- predict(HeadacheTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Headaches, predictHeadache)
sum(diag(acc))/sum(acc)
#about 79% accuracy

predictROCHeadaches <- predict(HeadacheTree, newdata = test)
predictROCHeadaches

#compute ROC curve for NauseaTree
predHeadache <- prediction(predictROCHeadaches [,2], test$med_Headaches)
perf <- performance(predHeadache, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Headaches == 1)
1283/(1283 + 476)
#about 73% accuracy just guessing 0 for headaches

#med_fatigue
split <- sample.split(LeaflySelect$med_Fatigue, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
FatigueTree <- rpart(med_Fatigue ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(FatigueTree)

#test classification tree model on test data
predictFatigue <- predict(FatigueTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Fatigue, predictFatigue)
sum(diag(acc))/sum(acc)
#about 74% accuracy

predictROCFatigue <- predict(FatigueTree, newdata = test)
predictROCFatigue

#compute ROC curve
predFatigue <- prediction(predictROCFatigue [,2], test$med_Fatigue)
perf <- performance(predFatigue, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Fatigue == 1)
1223/(1223 + 536)
#about 70% accuracy just guessing 0 for fatigue

#med_seizures
split <- sample.split(LeaflySelect$med_Seizures, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
SeizuresTree <- rpart(med_Seizures ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(SeizuresTree)

#test classification tree model on test data
predictSeizures <- predict(SeizuresTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Seizures, predictSeizures)
sum(diag(acc))/sum(acc)
#about 99% accuracy (guessing 0 for seizures, anyway)

predictROCSeizures <- predict(SeizuresTree, newdata = test)
predictROCSeizures

#compute ROC curve
predSeizures <- prediction(predictROCSeizures [,2], test$med_Seizures)
perf <- performance(predSeizures, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Seizures == 1)
1735/(1735 + 24)
#about 99% accuracy just guessing 0 for Seizures

#med_cramps
split <- sample.split(LeaflySelect$med_Cramps, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
CrampsTree <- rpart(med_Cramps ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(CrampsTree)

#test classification tree model on test data
predictCramps <- predict(CrampsTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Cramps, predictCramps)
sum(diag(acc))/sum(acc)
#about 94% accuracy

predictROCCramps <- predict(CrampsTree, newdata = test)
predictROCCramps

#compute ROC curve
predCramps <- prediction(predictROCCramps [,2], test$med_Cramps)
perf <- performance(predCramps, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Cramps == 1)
1641/(1641 + 118)
#about 93% accuracy just guessing 0 for cramps


#med_inflammation
split <- sample.split(LeaflySelect$med_Inflammation, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
InflammationTree <- rpart(med_Inflammation ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(InflammationTree)

#test classification tree model on test data
predictInflammation <- predict(InflammationTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Inflammation, predictInflammation)
sum(diag(acc))/sum(acc)
#about 88% accuracy

predictROCInflammation <- predict(InflammationTree, newdata = test)
predictROCInflammation

#compute ROC curve
predInflammation <- prediction(predictROCInflammation [,2], test$med_Inflammation)
perf <- performance(predInflammation, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Inflammation == 1)
1555/(1555 + 204)
#about 88% accuracy just guessing 0 for muscle spasms

#med_eyePressure
split <- sample.split(LeaflySelect$med_EyePressure, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
EyePressueTree <- rpart(med_EyePressure ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(EyePressueTree)

#test classification tree model on test data
predictEyePressure <- predict(EyePressueTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_EyePressure, predictEyePressure)
sum(diag(acc))/sum(acc)
#about 94% accuracy

predictROCEyePressure <- predict(EyePressueTree, newdata = test)
predictROCEyePressure

#compute ROC curve 
predEyePressure <- prediction(predictROCEyePressure [,2], test$med_EyePressure)
perf <- performance(predEyePressure, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_EyePressure == 1)
1646/(1646 + 113)
#about 94% accuracy just guessing 0 for eye pressure

#med_anxious
split <- sample.split(LeaflySelect$med_Anxious, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
AnxiousTree <- rpart(med_Anxious ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(AnxiousTree)

#test classification tree model on test data
predictAnxious<- predict(AnxiousTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Anxious, predictAnxious)
sum(diag(acc))/sum(acc)
#about 100% accuracy (just guessing 0 for Anxious)

predictROCAnxious <- predict(AnxiousTree, newdata = test)
predictROCAnxious

#compute ROC curve
predAnxious <- prediction(predictROCAnxious [,2], test$med_Anxious)
perf <- performance(predAnxious, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Anxious== 1)
1756/(1756 + 3)
#about 100% accuracy just guessing 0 for anxious


#med_spasticity
split <- sample.split(LeaflySelect$med_Spasticity, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
SpasticityTree <- rpart(med_Spasticity~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(SpasticityTree)

#test classification tree model on test data
predictSpasticity <- predict(SpasticityTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Spasticity, predictSpasticity)
sum(diag(acc))/sum(acc)
#about 99% accuracy (just guessing 0 for med_Spasticity)

predictROCSpasticity <- predict(SpasticityTree, newdata = test)
predictROCSpasticity

#compute ROC curve
predSpasticity <- prediction(predictROCSpasticity [,2], test$med_Spasticity)
perf <- performance(predSpasticity, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Spasticity== 1)
1736/(1736 + 23)
#about 99% accuracy guessing 0 for med_Spasticity

#med_DryMouth
split <- sample.split(LeaflySelect$med_DryMouth, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
DryMouthTree <- rpart(med_DryMouth ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(DryMouthTree)

#test classification tree model on test data
predictDryMouth <- predict(DryMouthTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_DryMouth, predictDryMouth)
sum(diag(acc))/sum(acc)
#about 99.6% accuracy (guessing 0 for dryMouth)

predictROCDryMouth <- predict(DryMouthTree, newdata = test)
predictROCDryMouth

#compute ROC curve
predDryMouth <- prediction(predictROCDryMouth [,2], test$med_DryMouth)
perf <- performance(predDryMouth, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_DryMouth== 1)
1751/(1751 + 8)
#about 100% accuracy guessing 0 for dryMouth

#med_DryEyes
split <- sample.split(LeaflySelect$med_DryEyes, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
DryEyesTree <- rpart(med_DryEyes ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(DryEyesTree)

#test classification tree model on test data
predictDryEyes <- predict(DryEyesTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_DryEyes, predictDryEyes)
sum(diag(acc))/sum(acc)
#about 100% accuracy guessing 0 for DryEyes

predictROCDryEyes <- predict(DryEyesTree, newdata = test)
predictROCDryEyes

#compute ROC curve
predDryEyes <- prediction(predictROCDryEyes [,2], test$med_DryEyes)
perf <- performance(predDryEyes, "tpr", "fpr")
plot(perf)
#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_DryEyes== 1)
1755/(1755 + 4)
#about 100% accuracy guessing 0 for DryMouth

#med_Paranoid
split <- sample.split(LeaflySelect$med_Paranoid, SplitRatio = .7)
train <- subset(LeaflySelect, split == TRUE)
test <- subset(LeaflySelect, split == FALSE)
train <- select(train, -c(popularity, straincounts))
test <- select(test, -c(popularity, straincounts))
ParanoidTree <- rpart(med_Paranoid ~ ., data = train, method = "class", control = rpart.control(minbucket = 10))
prp(ParanoidTree)

#test classification tree model on test data
predictParanoid <- predict(ParanoidTree, newdata = test, type = "class")
#compute accuracy of predictions
acc <- table(test$med_Paranoid, predictParanoid)
sum(diag(acc))/sum(acc)
#100% accuracy

predictROCParanoid <- predict(ParanoidTree, newdata = test)
predictROCParanoid

#compute baseline true positive rate as percentage of strains that are good for stress
count(leafly$med_Paranoid== 1)
1758/(1758 + 1)
#almost 100% accuracy guessing 0 for paranoid
##do not include variables that the tree does not improve over guessing
## 0 since there are not enough outcomes or cases that show they help 
##for those symptoms
###Only include the following medical symptoms:
###Stress, pain, nausea, muscle spasms, lack of appetite, insomnia, 
###depression, headaches, and fatigue.
