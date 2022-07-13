#Load in libraries
library(ggplot2)
library(caret)
library(gpairs)

#Read in Data
train = read.csv("Train.csv")
Test = read.csv("Test.csv")
TrainLabels = read.csv("TrainLabels.csv")

#Put training Labels in the data frame
Train <- merge(train, TrainLabels, by = 'id')

#Check for missing values
Train[which(!complete.cases(Train)),]
Test[which(!complete.cases(Test)),]

#Remove missing values
Train <- Train[which(complete.cases(Train)),]

#Use only selected columns
Train <- Train[,c(1, 2, 7, 8, 11, 16, 18, 24, 25, 32, 37, 40, 41)]
Test <- Test[,c(1, 2, 7, 8, 11, 16, 18, 24, 25, 32, 37, 40)]



#Bar graphs for categorical data visualization
ggplot(Train, aes(x = basin, fill= status_group)) +
  geom_bar() +  labs(x='Basin') + ggtitle("Basins")

ggplot(Train, aes(x = extraction_type, fill= status_group)) +
  geom_bar() +  labs(x='Extraction Type') + ggtitle('Extraction Type')

ggplot(Train, aes(x = water_quality, fill= status_group)) +
  geom_bar() +  labs(x='Water Quality') + ggtitle("Water Quality")

ggplot(Train, aes(x = Train$source_type, fill= status_group)) +
  geom_bar() +  labs(x='Source Type') + ggtitle("Source Type")

ggplot(Train, aes(x = waterpoint_type_group, fill= status_group)) +
  geom_bar() +  labs(x='Waterpoint Type Group') + ggtitle("Water Point Type Group")

ggplot(Train[which(Train$construction_year > 0),], aes(x = construction_year, fill= status_group)) +
  geom_bar() +  labs(x='Construction Year') + ggtitle("Population")


#histogram plots for numerical data
ggplot(Train[which(Train$population > 50 & Train$population < 500),], aes(x = population, fill= status_group)) +
  geom_histogram() +  labs(x='population') + ggtitle("Construction Year")


#Scatter plot of pumps colored by status group 
ggplot(Train[which(Train$longitude > 0),], aes(x = longitude, y = latitude, colour = status_group)) +
  geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) + ggtitle("Pump Locations")




#Separate date by class group functionality
funTrain <- Train[which(Train$status_group == "functional"),]
repairTrain <- Train[which(Train$status_group == "functional needs repair"),]
nonTrain <- Train[which(Train$status_group == "non functional"),]



#####
#Naive Bayes Theorem: P(y|X) = P(x|y)*P(y)
#####                  = Likelikhood * Prior


#Get prior values
Priors <- function(x) {
  prob <- prop.table(table(x))
  
  return(prob)
}

#Get Likelihood values
pTsh <- function(x, y) {
  
  mu <- mean(as.numeric(y))
  sig2 <- var(as.numeric(y))
  
  p <- vector(mode = "list", length = length(x))
  
  p <- 1/sqrt(2*pi*sig2)*exp(-(x-mu)^2/(2*sig2))
  
  return(p)
}

pPopulation <- function(x, y) {
  
  mu <- mean(as.numeric(y))
  sig2 <- var(as.numeric(y))
  
  p <- vector(mode = "list", length = length(x))
  
  p <- 1/sqrt(2*pi*sig2)*exp(-(x-mu)^2/(2*sig2))
  
  return(p)
}

pBasin <- function(x, y) {
  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "Internal")] <- prob["Internal"]
  p[which(x == "Lake Nyasa")] <- prob["Lake Nyasa"]
  p[which(x == "Lake Rukwa")] <- prob["Lake Rukwa"]
  p[which(x == "Lake Tanganyika")] <- prob["Lake Tanganyika"]
  p[which(x == "Lake Victoria")] <- prob["Lake Victoria"]
  p[which(x == "Pangani")] <- prob["Pangani"]
  p[which(x == "Rufiji")] <- prob["Rufiji"]
  p[which(x == "Ruvuma / Southern Coast")] <- prob["Ruvuma / Southern Coast"]
  p[which(x == "Wami / Ruvu")] <- prob["Wami / Ruvu"]
  return(p)
}


pConYear <- function(x, y) {
  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "0")] <- prob["0"]
  p[which(x == "1960")] <- prob["1960"]
  p[which(x == "1961")] <- prob["1961"]
  p[which(x == "1962")] <- prob["1962"]
  p[which(x == "1963")] <- prob["1963"]
  p[which(x == "1964")] <- prob["1964"]
  p[which(x == "1965")] <- prob["1965"]
  p[which(x == "1966")] <- prob["1966"]
  p[which(x == "1967")] <- prob["1967"]
  p[which(x == "1968")] <- prob["1968"]
  p[which(x == "1969")] <- prob["1969"]
  p[which(x == "1970")] <- prob["1970"]
  p[which(x == "1971")] <- prob["1971"]
  p[which(x == "1972")] <- prob["1972"]
  p[which(x == "1973")] <- prob["1973"]
  p[which(x == "1974")] <- prob["1974"]
  p[which(x == "1975")] <- prob["1975"]
  p[which(x == "1976")] <- prob["1976"]
  p[which(x == "1977")] <- prob["1977"]
  p[which(x == "1978")] <- prob["1978"]
  p[which(x == "1979")] <- prob["1979"]
  p[which(x == "1980")] <- prob["1980"]
  p[which(x == "1981")] <- prob["1981"]
  p[which(x == "1982")] <- prob["1982"]
  p[which(x == "1983")] <- prob["1983"]
  p[which(x == "1984")] <- prob["1984"]
  p[which(x == "1985")] <- prob["1985"]
  p[which(x == "1986")] <- prob["1986"]
  p[which(x == "1987")] <- prob["1987"]
  p[which(x == "1988")] <- prob["1988"]
  p[which(x == "1989")] <- prob["1989"]
  p[which(x == "1990")] <- prob["1990"]
  p[which(x == "1991")] <- prob["1991"]
  p[which(x == "1992")] <- prob["1992"]
  p[which(x == "1993")] <- prob["1993"]
  p[which(x == "1994")] <- prob["1994"]
  p[which(x == "1995")] <- prob["1995"]
  p[which(x == "1996")] <- prob["1996"]
  p[which(x == "1997")] <- prob["1997"]
  p[which(x == "1998")] <- prob["1998"]
  p[which(x == "1999")] <- prob["1999"]
  p[which(x == "2000")] <- prob["2000"]
  p[which(x == "2001")] <- prob["2001"]
  p[which(x == "2002")] <- prob["2002"]
  p[which(x == "2003")] <- prob["2003"]
  p[which(x == "2004")] <- prob["2004"]
  p[which(x == "2005")] <- prob["2005"]
  p[which(x == "2006")] <- prob["2006"]
  p[which(x == "2007")] <- prob["2007"]
  p[which(x == "2008")] <- prob["2008"]
  p[which(x == "2009")] <- prob["2009"]
  p[which(x == "2010")] <- prob["2010"]
  p[which(x == "2011")] <- prob["2011"]
  p[which(x == "2012")] <- prob["2012"]
  p[which(x == "2013")] <- prob["2013"]
  
  return(p)
}

pExtract <- function(x, y) {
  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "afridev")] <- prob["afridev"]
  p[which(x == "cemo")] <- prob["cemo"]
  p[which(x == "climax")] <- prob["climax"]
  p[which(x == "gravity")] <- prob["gravity"]
  p[which(x == "india mark ii")] <- prob["india mark ii"]
  p[which(x == "india mark iii")] <- prob["india mark iii"]
  p[which(x == "ksb")] <- prob["ksb"]
  p[which(x == "mono")] <- prob["mono"]
  p[which(x == "nira/tanira")] <- prob["nira/tanira"]
  p[which(x == "other")] <- prob["other"]
  p[which(x == "other - mkulima/shinyanga")] <- prob["other - mkulima/shinyanga"]
  p[which(x == "other - play pump")] <- prob["other - play pump"]
  p[which(x == "other - rope pump")] <- prob["other - rope pump"]
  p[which(x == "other - swn 81")] <- prob["other - swn 81"]
  p[which(x == "submersible")] <- prob["submersible"]
  p[which(x == "swn 80")] <- prob["swn 80"]
  p[which(x == "walimi")] <- prob["walimi"]
  p[which(x == "windmill")] <- prob["windmill"]
  
  return(p)
}

pQuality <- function(x, y) {

  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "coloured")] <- prob["coloured"]
  p[which(x == "fluoride")] <- prob["fluoride"]
  p[which(x == "fluoride abandoned")] <- prob["fluoride abandoned"]
  p[which(x == "milky")] <- prob["milky"]
  p[which(x == "salty")] <- prob["salty"]
  p[which(x == "salty abandoned")] <- prob["salty abandoned"]
  p[which(x == "soft")] <- prob["soft"]
  p[which(x == "unknown")] <- prob["unknown"]
  
  return(p)
}

pSource <- function(x, y) {
  
  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "borehole")] <- prob["borehole"]
  p[which(x == "dam")] <- prob["dam"]
  p[which(x == "other")] <- prob["other"]
  p[which(x == "rainwater harvesting")] <- prob["rainwater harvesting"]
  p[which(x == "river/lake")] <- prob["river/lake"]
  p[which(x == "shallow well")] <- prob["shallow well"]
  p[which(x == "spring")] <- prob["spring"]
  
  return(p)
}

pWaterpoint <- function(x, y) {

  prob <- prop.table(table(y))
  p <- vector(mode = "list", length = length(x))
  
  p[which(x == "cattle trough")] <- prob["cattle trough"]
  p[which(x == "communal standpipe")] <- prob["communal standpipe"]
  p[which(x == "dam")] <- prob["dam"]
  p[which(x == "hand pump")] <- prob["hand pump"]
  p[which(x == "improved spring")] <- prob["improved spring"]
  p[which(x == "other")] <- prob["other"]
  
  return(p)
}


##Probabilities given "functional" Test Data
tsh_Prob <- pTsh(Test$amount_tsh, funTrain$amount_tsh)
population_Prob <- pPopulation(Test$population, funTrain$population)
basin_Prob <- pBasin(Test$basin, funTrain$basin)
year_Prob <- pConYear(Test$construction_year, funTrain$construction_year)
extract_Prob <- pExtract(Test$extraction_type, funTrain$extraction_type)
quality_Prob <- pQuality(Test$water_quality, funTrain$water_quality)
source_Prob <- pSource(Test$source_type, funTrain$source_type)
waterpoint_Prob <- pWaterpoint(Test$waterpoint_type_group, funTrain$waterpoint_type_group)

funResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
funResults$basin_Prob <- as.numeric(basin_Prob)
funResults$year_Prob <- as.numeric(year_Prob)
funResults$extract_Prob <- as.numeric(extract_Prob)
funResults$quality_Prob <- as.numeric(quality_Prob)
funResults$source_Prob <- as.numeric(source_Prob)
funResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
funResults$extract_Prob[which(!complete.cases(funResults$extract_Prob))] <- 1

funResults$Liklihood <- funResults$tsh_Prob * 
  funResults$population_Prob * 
  funResults$basin_Prob * 
  #funResults$year_Prob *
  funResults$extract_Prob * 
  #funResults$quality_Prob * 
  funResults$source_Prob #* funResults$waterpoint_Prob 

funResults$Probability <- funResults$Liklihood * Priors(Train$status_group)[1]




##Probabilities given "functional needs replacement" Test data
tsh_Prob <- pTsh(Test$amount_tsh, repairTrain$amount_tsh)
population_Prob <- pPopulation(Test$population, repairTrain$population)
basin_Prob <- pBasin(Test$basin, repairTrain$basin)
year_Prob <- pConYear(Test$construction_year, repairTrain$construction_year)
extract_Prob <- pExtract(Test$extraction_type, repairTrain$extraction_type)
quality_Prob <- pQuality(Test$water_quality, repairTrain$water_quality)
source_Prob <- pSource(Test$source_type, repairTrain$source_type)
waterpoint_Prob <- pWaterpoint(Test$waterpoint_type_group, repairTrain$waterpoint_type_group)

repairResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
repairResults$basin_Prob <- as.numeric(basin_Prob)
repairResults$year_Prob <- as.numeric(year_Prob)
repairResults$extract_Prob <- as.numeric(extract_Prob)
repairResults$quality_Prob <- as.numeric(quality_Prob)
repairResults$source_Prob <- as.numeric(source_Prob)
repairResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
repairResults$year_Prob[which(!complete.cases(repairResults$year_Prob))] <- 1 
repairResults$quality_Prob[which(!complete.cases(repairResults$quality_Prob))] <- 1 
repairResults$waterpoint_Prob[which(!complete.cases(repairResults$waterpoint_Prob))] <- 1 
repairResults$extract_Prob[which(!complete.cases(repairResults$extract_Prob))] <- 1 

repairResults$Liklihood <-  repairResults$tsh_Prob * 
  repairResults$population_Prob *  
  repairResults$basin_Prob * 
  #repairResults$year_Prob * 
  repairResults$extract_Prob * 
  #repairResults$quality_Prob * 
  repairResults$source_Prob #* repairResults$waterpoint_Prob 

repairResults$Probability <- repairResults$Liklihood * Priors(Train$status_group)[2]




##Probabilities given "non functional" Test data
tsh_Prob <- pTsh(Test$amount_tsh, nonTrain$amount_tsh)
population_Prob <- pPopulation(Test$population, nonTrain$population)
basin_Prob <- pBasin(Test$basin, nonTrain$basin)
year_Prob <- pConYear(Test$construction_year, nonTrain$construction_year)
extract_Prob <- pExtract(Test$extraction_type, nonTrain$extraction_type)
quality_Prob <- pQuality(Test$water_quality, nonTrain$water_quality)
source_Prob <- pSource(Test$source_type, nonTrain$source_type)
waterpoint_Prob <- pWaterpoint(Test$waterpoint_type_group, nonTrain$waterpoint_type_group)

nonResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
nonResults$basin_Prob <- as.numeric(basin_Prob)
nonResults$year_Prob <- as.numeric(year_Prob)
nonResults$extract_Prob <- as.numeric(extract_Prob)
nonResults$quality_Prob <- as.numeric(quality_Prob)
nonResults$source_Prob <- as.numeric(source_Prob)
nonResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
nonResults$waterpoint_Prob[which(!complete.cases(nonResults$waterpoint_Prob))] <- 0

nonResults$Liklihood <- nonResults$tsh_Prob * 
  nonResults$population_Prob * 
  nonResults$basin_Prob * 
  #nonResults$year_Prob * 
  nonResults$extract_Prob * 
  #nonResults$quality_Prob * 
  nonResults$source_Prob #* nonResults$waterpoint_Prob 

nonResults$Probability <- nonResults$Liklihood * Priors(Train$status_group)[3]
                        


#######Predictions
Results <- Test[, c(1, 3, 4)]
Results$Functionality[which(nonResults$Probability > repairResults$Probability & nonResults$Probability > repairResults$Probability)] <- "non functional"
Results$Functionality[which(funResults$Probability > repairResults$Probability & funResults$Probability >= nonResults$Probability)] <- "functional"
Results$Functionality[which(repairResults$Probability >= funResults$Probability & repairResults$Probability >= nonResults$Probability)] <- "functional needs repair"


#plot all results by functionality
#All of Tanzania
Tanzania <- ggplot(Results[which(Results$longitude > 0),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) +
       ggtitle("Locations Across Tanzania")


#Graphs by basin
#Internal basin
Internal <- ggplot(Results[which(Test$basin == "Internal"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) +
        ggtitle("Locations in Internal Basin")

#Lake Nyasa basin
Nyasa <- ggplot(Results[which(Test$basin == "Lake Nyasa"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
       ggtitle("Locations in Lake Nyasa Basin")

#Lake Rukwa basin
Rukwa <- ggplot(Results[which(Test$basin == "Lake Rukwa"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
  ggtitle("Locations in Lake Rukwa Basin")

#Lake Tanganyika basin
Tanganyika <- ggplot(Results[which(Test$basin == "Lake Tanganyika" & Results$longitude > 0),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
       ggtitle("Locations in Lake Tanganyika Basin")

#Lake Victoria basin
Victoria <- ggplot(Results[which(Test$basin == "Lake Victoria" & Results$longitude > 0),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) +
       ggtitle("Locations in Lake Victoria Basin")

#Pangani basin
Pangani <- ggplot(Results[which(Test$basin == "Pangani"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) +
       ggtitle("Locations in Pangani Basin")

#Rufiji basin
Rufiji <- ggplot(Results[which(Test$basin == "Rufiji"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
       ggtitle("Locations in Rufiji Basin")

#Ruvuma / Southern Coast basin
Ruvuma <- ggplot(Results[which(Test$basin == "Ruvuma / Southern Coast"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
       ggtitle("Locations in Ruvuma / Southern Coast Basin")

#Wami / Ruvu basin
Wami <- ggplot(Results[which(Test$basin == "Wami / Ruvu"),], 
       aes(x = longitude, y = latitude, colour = Functionality)) +
       geom_point() + scale_color_manual(values = c("Green", "Red")) +
       ggtitle("Wami / Ruvu Basin") 











###################################################################################
############### The Following lines of code were uses for testing #################
############## and evaluating the F1 score for various compbination ###############
################################### of varibles ###################################
####################### Skip down to next block for GUI code ######################
###################################################################################







##Probabilities given "functional" Train Data
tsh_Prob <- pTsh(Train$amount_tsh, funTrain$amount_tsh)
population_Prob <- pPopulation(Train$population, funTrain$population)
basin_Prob <- pBasin(Train$basin, funTrain$basin)
year_Prob <- pConYear(Train$construction_year, funTrain$construction_year)
extract_Prob <- pExtract(Train$extraction_type, funTrain$extraction_type)
quality_Prob <- pQuality(Train$water_quality, funTrain$water_quality)
source_Prob <- pSource(Train$source_type, funTrain$source_type)
waterpoint_Prob <- pWaterpoint(Train$waterpoint_type_group, funTrain$waterpoint_type_group)

funResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
funResults$basin_Prob <- as.numeric(basin_Prob)
funResults$year_Prob <- as.numeric(year_Prob)
funResults$extract_Prob <- as.numeric(extract_Prob)
funResults$quality_Prob <- as.numeric(quality_Prob)
funResults$source_Prob <- as.numeric(source_Prob)
funResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
funResults$extract_Prob[which(!complete.cases(funResults$extract_Prob))] <- 1

funResults$Liklihood <- funResults$tsh_Prob * 
  funResults$population_Prob * 
  funResults$basin_Prob * 
  #funResults$year_Prob *
  funResults$extract_Prob * 
  #funResults$quality_Prob #* 
  funResults$source_Prob #* 
  #funResults$waterpoint_Prob 

funResults$Probability <- funResults$Liklihood * Priors(Train$status_group)[1]




##Probabilities given "functional needs replacement" Test data
tsh_Prob <- pTsh(Train$amount_tsh, repairTrain$amount_tsh)
population_Prob <- pPopulation(Train$population, repairTrain$population)
basin_Prob <- pBasin(Train$basin, repairTrain$basin)
year_Prob <- pConYear(Train$construction_year, repairTrain$construction_year)
extract_Prob <- pExtract(Train$extraction_type, repairTrain$extraction_type)
quality_Prob <- pQuality(Train$water_quality, repairTrain$water_quality)
source_Prob <- pSource(Train$source_type, repairTrain$source_type)
waterpoint_Prob <- pWaterpoint(Train$waterpoint_type_group, repairTrain$waterpoint_type_group)

repairResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
repairResults$basin_Prob <- as.numeric(basin_Prob)
repairResults$year_Prob <- as.numeric(year_Prob)
repairResults$extract_Prob <- as.numeric(extract_Prob)
repairResults$quality_Prob <- as.numeric(quality_Prob)
repairResults$source_Prob <- as.numeric(source_Prob)
repairResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
repairResults$year_Prob[which(!complete.cases(repairResults$year_Prob))] <- 1 
repairResults$quality_Prob[which(!complete.cases(repairResults$quality_Prob))] <- 1 
repairResults$waterpoint_Prob[which(!complete.cases(repairResults$waterpoint_Prob))] <- 1 
repairResults$extract_Prob[which(!complete.cases(repairResults$extract_Prob))] <- 1 

repairResults$Liklihood <-  repairResults$tsh_Prob * 
  repairResults$population_Prob *  
  repairResults$basin_Prob * 
  #repairResults$year_Prob * 
  repairResults$extract_Prob * 
  #repairResults$quality_Prob #* 
  repairResults$source_Prob #* 
  #repairResults$waterpoint_Prob 

repairResults$Probability <- repairResults$Liklihood * Priors(Train$status_group)[2]




##Probabilities given "non functional" Test data
tsh_Prob <- pTsh(Train$amount_tsh, nonTrain$amount_tsh)
population_Prob <- pPopulation(Train$population, nonTrain$population)
basin_Prob <- pBasin(Train$basin, nonTrain$basin)
year_Prob <- pConYear(Train$construction_year, nonTrain$construction_year)
extract_Prob <- pExtract(Train$extraction_type, nonTrain$extraction_type)
quality_Prob <- pQuality(Train$water_quality, nonTrain$water_quality)
source_Prob <- pSource(Train$source_type, nonTrain$source_type)
waterpoint_Prob <- pWaterpoint(Train$waterpoint_type_group, nonTrain$waterpoint_type_group)

nonResults <- data.frame(tsh_Prob = tsh_Prob, population_Prob = population_Prob)
nonResults$basin_Prob <- as.numeric(basin_Prob)
nonResults$year_Prob <- as.numeric(year_Prob)
nonResults$extract_Prob <- as.numeric(extract_Prob)
nonResults$quality_Prob <- as.numeric(quality_Prob)
nonResults$source_Prob <- as.numeric(source_Prob)
nonResults$waterpoint_Prob <- as.numeric(waterpoint_Prob)
nonResults$waterpoint_Prob[which(!complete.cases(nonResults$waterpoint_Prob))] <- 0

nonResults$Liklihood <- nonResults$tsh_Prob * 
  nonResults$population_Prob * 
  nonResults$basin_Prob * 
  #nonResults$year_Prob * 
  nonResults$extract_Prob * 
  #nonResults$quality_Prob #* 
  nonResults$source_Prob #* 
  #nonResults$waterpoint_Prob 

nonResults$Probability <- nonResults$Liklihood * Priors(Train$status_group)[3]



#######Predictions
Results <- Train[, c(1, 3, 4)]
Results$Functionality[which(nonResults$Probability > repairResults$Probability & nonResults$Probability > repairResults$Probability)] <- "non functional"
Results$Functionality[which(funResults$Probability > repairResults$Probability & funResults$Probability >= nonResults$Probability)] <- "functional"
Results$Functionality[which(repairResults$Probability >= funResults$Probability & repairResults$Probability >= nonResults$Probability)] <- "functional needs repair"

#Calculate - F-score
precision <- length(Results$id[which(Results$Functionality == Train$status_group)])/length(Results$id)
recall <- length(Results$id[which(Results$Functionality != Train$status_group)])/length(Results$id)

2*precision*recall/(precision + recall)


###################################################################################
###################################################################################
###################################################################################
#################################### GUI Code #####################################
###################################################################################
###################################################################################
###################################################################################


library(shiny)

ui <- fluidPage(
  titlePanel("Water Pumps"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Show plots from various areas in Tanzania"),
      
      selectInput("var", 
                  label = "Choose a region to display",
                  choices = c("Tanzania", 
                              "Lake Nyasa Basin",
                              "Lake Rukwa Basin", 
                              "Lake Tanganyika Basin",
                              "Lake Victoria Basin",
                              "Pangani Basin",
                              "Rufiji Basin",
                              "Ruvuma/Southern Coast Basin",
                              "Wami/Ruvu Basin"),
                  selected = "Percent White"),
      
    ),
    
    mainPanel(
      textOutput("selected_var")
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    ggplot(Results[which(Results$longitude > 0),], 
           aes(x = longitude, y = latitude, colour = Functionality)) +
      geom_point() + scale_color_manual(values = c("Green", "Blue", "Red")) +
      ggtitle("Locations Across Tanzania")
  }, res = 96)
  
}


library(ggplot2)

ui <- fluidPage(titlePanel("Water Pumps"),
  sidebarLayout(
    sidebarPanel(
      helpText("Show plots from various areas in Tanzania"),
      
      selectInput("map", 
                  label = "Choose a region to display",
                  choices = c("Tanzania", 
                              "Lake Nyasa Basin",
                              "Lake Rukwa Basin", 
                              "Lake Tanganyika Basin",
                              "Lake Victoria Basin",
                              "Pangani Basin",
                              "Rufiji Basin",
                              "Ruvuma/Southern Coast Basin",
                              "Wami/Ruvu Basin"),
      
      )
      
    ),
    mainPanel(
      textOutput("selected_map")
    ),
  ), 
  mainPanel(
    plotOutput("plot", brush = "plot_brush"),
    tableOutput("data")
  )
)

server <- function(input, output, session) {
  
  choosePlot <- reactive({
    if ( "Tanzania" %in% input$map) return(Tanzania)
    if ( "Lake Nyasa Basin" %in% input$map) return(Nyasa)
    if( "Lake Rukwa Basin" %in% input$map) return(Rukwa) 
    if ( "Lake Tanganyika Basin" %in% input$map) return(Tanganyika)
    if ( "Lake Victoria Basin" %in% input$map) return(Victoria)
    if( "Pangani Basin" %in% input$map) return(Pangani) 
    if ( "Rufiji Basin" %in% input$map) return(Rufiji)
    if ( "Ruvuma/Southern Coast Basin" %in% input$map) return(Ruvuma)
    if( "Wami/Ruvu Basin" %in% input$map) return(Wami) 
  })
  
  output$plot <- renderPlot({
    
    choosePlot()
  }, res = 96)

  
  output$data <- renderTable({
    brushedPoints(Results, input$plot_brush)
  },
  
  
  )
}

#Launch User Interface
shinyApp(ui, server)






