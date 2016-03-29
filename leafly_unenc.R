library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)
library(gtools)
dataFolder <- "C:/Users/Nina/Desktop/datafoundations/Capstone Project/"

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

#####one hot encoding for conditions for each strain####
#execute these steps for each condition 
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
qplot(data = leafly_unenc, x = neg_1)
qplot(data = leafly_unenc, x = neg_2)
qplot(data = leafly_unenc, x = neg_3)
qplot(data = leafly_unenc, x = neg_4)
qplot(data = leafly_unenc, x = neg_5)
qplot(data = leafly_unenc, x = med_1)
qplot(data = leafly_unenc, x = med_2)
qplot(data = leafly_unenc, x = med_3)
qplot(data = leafly_unenc, x = med_4)
qplot(data = leafly_unenc, x = med_5)
qplot(data = leafly_unenc, x = fx_1)
qplot(data = leafly_unenc, x = fx_2)
qplot(data = leafly_unenc, x = fx_3)
qplot(data = leafly_unenc, x = fx_4)
qplot(data = leafly_unenc, x = fx_5)
qplot(data = leafly_unenc, x = flavor_1)
qplot(data = leafly_unenc, x = flavor_2)
qplot(data = leafly_unenc, x = flavor_3)
qplot(data = leafly_unenc, x = p1)
qplot(data = leafly_unenc, x = p2)
qplot(data = leafly_unenc, x = p3)
qplot(data = leafly_unenc, x = p4)
qplot(data = leafly_unenc, x = p5)
qplot(data = leafly_unenc, x = p6)
qplot(data = leafly_unenc, x = p7)
qplot(data = leafly_unenc, x = L1)
qplot(data = leafly_unenc, x = L2)
qplot(data = leafly_unenc, x = L3)
qplot(data = leafly_unenc, x = L4)
qplot(data = leafly_unenc, x = L5)
qplot(data = leafly_unenc, x = L6)
qplot(data = leafly_unenc, x = L7)
qplot(data = leafly_unenc, x = L8)
qplot(data = leafly_unenc, x = L9)
qplot(data = leafly_unenc, x = L10)
qplot(data = leafly_unenc, x = L11)
qplot(data = leafly_unenc, x = popularity)
### the fact that L11 and p7 each have only one strain, 
###tells me they are probably insignificant variables 
###(the same is also possibly true of p5 & p6, 
### each with 3 strains only)


qplot(data = subset(leafly_unenc, !is.na(neg_1)), x = popularity, xlim = c(0, 200)) +
  facet_wrap(~ neg_1)

#look at how different conditions may be helped by strains that treat different symptoms (med_1)
by(select(leafly_unenc, X1:X40), leafly_unenc$med_2, summary)

summary(leafly_unenc$popularity)
summary(log10(leafly_unenc$popularity + 1))

library(gridExtra)
n1 <- qplot(data = leafly_unenc, x = neg_1)
n2 <- qplot(data = leafly_unenc, x = neg_2)
n3 <- qplot(data = leafly_unenc, x = neg_3)
n4 <- qplot(data = leafly_unenc, x = neg_4)
n5 <- qplot(data = leafly_unenc, x = neg_5)

grid.arrange(n1, n2, n3, n4, n5, ncol=5)

qplot(x = popularity, y = ..count../sum(..count..), 
      data = leafly_unenc, geom = 'freqpoly', color = neg_1, xlim = c(0,500))
qplot(data = subset(leafly_unenc, !is.na(med_1)), x = med_1, y = popularity,
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,1000))


