library("sp")
library("rgdal")
library("rgeos")
library(tmap)
library(spdep)

#first read in all the datasets
#parties with over 1% in national elections coded as either left-right wing and cosmopolitan/liberal
parties <- read.csv("parties.csv")
#unemployment data for each district from the elections month
unemployment <- read.csv("unemployment.csv")
#election results for every party in every district
el_2006 <- read.csv("csv/election_2006.csv")
el_2010 <- read.csv("csv/election_2010.csv")
el_2012 <- read.csv("csv/election_2012.csv")
el_2016 <- read.csv("csv/election_2016.csv")

#paste the year and month of elections in the corresposponding datasets
colnames(el_2006) <- paste(colnames(el_2006), 200606, sep = "_") 
colnames(el_2010) <- paste(colnames(el_2010), 201006, sep = "_") 
colnames(el_2012) <- paste(colnames(el_2012), 201203, sep = "_") 
colnames(el_2016) <- paste(colnames(el_2016), 201603, sep = "_")

#merge election results datasets into one by districts and delete unnecessary columns
el_20062010 <- merge(el_2006, el_2010, by.x= "Code_district_200606", by.y = "Code_district_201006")
el_20062010$District_201006 <- NULL 

el_20122016 <- merge(el_2012, el_2016, by.x= "Code_district_201203", by.y= "Code_district_201603")
el_20122016$District_201603 <- NULL

elections <- merge(el_20062010, el_20122016, by.x = "Code_district_200606", by.y = "Code_district_201203")
elections$District_201203 <- NULL
names(elections)[1] <- "Code"
names(elections)[2] <- "District"

#create aggregate columns based on the parties.csv coding file
parties$el_year <- as.factor(parties$el_year)

for (i in levels(parties$el_year)){
  #center
  subset_center <- subset(parties, el_year==i & left.right == 0)
  if (nrow(subset_center)>1){
    center <- subset_center[,1]
    center <- as.character(center)
    center <- paste(center, i, sep = "_")
    elections[,paste("center", i, sep = "_")] <- rowSums(elections[, center], na.rm= TRUE)
    elections[,paste("center", i, sep = "_")] <- (elections[,paste("center", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
  }
  if (nrow(subset_center) == 1){
    center <- subset_center[,1]
    center <- as.character(center)
    center <- paste(center, i, sep = "_")
    elections[,paste("center", i, sep = "_")] <- elections[, center]
    elections[,paste("center", i, sep = "_")] <- (elections[,paste("center", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("center")
  #right_reg
  subset_right_reg <- subset(parties, el_year==i & left.right == 1)
  if (nrow(subset_right_reg)>1){
    right_reg <- subset_right_reg[,1]
    right_reg <- as.character(right_reg)
    right_reg <- paste(right_reg, i, sep = "_")
    elections[,paste("right_reg", i, sep = "_")] <- rowSums(elections[, right_reg], na.rm= TRUE)
    elections[,paste("right_reg", i, sep = "_")] <- (elections[,paste("right_reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_right_reg) == 1){
    right_reg <- subset_right_reg[,1]
    right_reg <- as.character(right_reg)
    right_reg <- paste(right_reg, i, sep = "_")
    elections[,paste("right_reg", i, sep = "_")] <- elections[, right_reg]
    elections[,paste("right_reg", i, sep = "_")] <- (elections[,paste("right_reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
  }
  print("right_reg")
  #left_reg
  subset_left_reg <- subset(parties, el_year==i & left.right == -1)
  if (nrow(subset_left_reg)>1){
    partysel <- subset_left_reg[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left_reg", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("left_reg", i, sep = "_")] <- (elections[,paste("left_reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_left_reg) == 1){
    partysel <- subset_left_reg[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left_reg", i, sep = "_")] <- elections[, partysel]
    elections[,paste("left_reg", i, sep = "_")] <- (elections[,paste("left_reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("left_reg")
  #left
  subset_left <- subset(parties, el_year==i & left.right <0)
  if (nrow(subset_left)>1){
    partysel <- subset_left[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("left", i, sep = "_")] <- (elections[,paste("left", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_left) == 1){
    partysel <- subset_left[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left", i, sep = "_")] <- elections[, partysel]
    elections[,paste("left", i, sep = "_")] <- (elections[,paste("left", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("left")
  #right
  subset_right <- subset(parties, el_year==i & left.right >0)
  if (nrow(subset_right)>1){
    partysel <- subset_right[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("right", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("right", i, sep = "_")] <- (elections[,paste("right", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_right) == 1){
    partysel <- subset_right[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("right", i, sep = "_")] <- elections[, partysel]
    elections[,paste("right", i, sep = "_")] <- (elections[,paste("right", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("right")
  #left_ex
  subset_left_ex <- subset(parties, el_year==i & left.right == -2)
  if (nrow(subset_left_ex)>1){
    partysel <- subset_left_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left_ex", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("left_ex", i, sep = "_")] <- (elections[,paste("left_ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_left_ex) == 1){
    partysel <- subset_left_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("left_ex", i, sep = "_")] <- elections[, partysel]
    elections[,paste("left_ex", i, sep = "_")] <- (elections[,paste("left_ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("left_ex")
  #right_ex
  subset_right_ex <- subset(parties, el_year==i & left.right == 2)
  if (nrow(subset_right_ex)>1){
    partysel <- subset_right_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("right_ex", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("right_ex", i, sep = "_")] <- (elections[,paste("right_ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_right_ex) == 1){
    partysel <- subset_right_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("right_ex", i, sep = "_")] <- elections[, partysel]
    elections[,paste("right_ex", i, sep = "_")] <- (elections[,paste("right_ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("right_ex")
  #ex
  subset_ex <- subset(parties, el_year==i & (left.right == -2 | left.right == 2))
  if (nrow(subset_ex)>1){
    partysel <- subset_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("ex", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("ex", i, sep = "_")] <- (elections[,paste("ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_ex) == 1){
    partysel <- subset_ex[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("ex", i, sep = "_")] <- elections[, partysel]
    elections[,paste("ex", i, sep = "_")] <- (elections[,paste("ex", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print("ex")
  #reg
  subset_reg <- subset(parties, el_year==i & (left.right == -1 | left.right == 0 | left.right == 1 ))
  if (nrow(subset_reg)>1){
    partysel <- subset_reg[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("reg", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
    elections[,paste("reg", i, sep = "_")] <- (elections[,paste("reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  if (nrow(subset_reg) == 1){
    partysel <- subset_reg[,1]
    partysel <- as.character(partysel)
    partysel <- paste(partysel, i, sep = "_")
    elections[,paste("reg", i, sep = "_")] <- elections[, partysel]
    elections[,paste("reg", i, sep = "_")] <- (elections[,paste("reg", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
    
  }
  print(paste("reg", i))
}
for (i in levels(parties$el_year)){
  subset_cosmopolitan <- subset(parties, el_year==i & cos.par >= 5)
  partysel <- subset_cosmopolitan[,1]
  partysel <- as.character(partysel)
  partysel <- paste(partysel, i, sep = "_")
  elections[,paste("cosmopolitan", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
  elections[,paste("cosmopolitan", i, sep = "_")] <- (elections[,paste("cosmopolitan", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
  
  
  subset_parochial <- subset(parties, el_year==i & cos.par < 5)
  partysel <- subset_parochial[,1]
  partysel <- as.character(partysel)
  partysel <- paste(partysel, i, sep = "_")
  elections[,paste("parochial", i, sep = "_")] <- rowSums(elections[, partysel], na.rm= TRUE)
  elections[,paste("parochial", i, sep = "_")] <- (elections[,paste("parochial", i, sep = "_")]/elections[,paste("Votescount", i, sep = "_")])*100
  
}

#convert party results into % but the first column has to be the Code column
dataset <- elections
for (i in 4:23){
  dataset[,i]<- (dataset[,i] / dataset[,3])*100
}
for (i in 25:42){
  dataset[,i]<- (dataset[,i] / dataset[,24])*100
}
for (i in 44:69){
  dataset[,i]<- (dataset[,i] / dataset[,43])*100
}
for (i in 71:93){
  dataset[,i]<- (dataset[,i] / dataset[,70])*100
}

#merge with unemployment data
complete_dataset <- merge(elections, unemployment, by= "Code")

#last fixes
complete_dataset[,2] <- NULL
complete_dataset[,"X.y"] <- NULL
complete_dataset[,"District.y"] <- NULL
colnames(complete_dataset)[2] <- "District"

#save as csv we will work further with 
write.csv(complete_dataset, "complete_dataset_per.csv")

#now we will manipulate the downloaded shapefiles
#read in the districts of Slovakia
Output.Areas <- readOGR(".", "okresy-005")
#delete the unnecessary columns before merging
Output.Areas$TXT <- NULL
Output.Areas$unemploy <- NULL
names(Output.Areas)[1] <- "Code"

#now we can create clean shapefiles
writeOGR(Output.Areas, dsn = ".", layer = "Slovakia", driver="ESRI Shapefile")
