library("sp")
library("rgdal")
library("rgeos")
library(tmap)
library(spdep)
library("spgwr")
library(grid)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(texreg)
library(RColorBrewer)

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
} #function to get the p-value from lm regression

#read in the prepared files
dataset <- read.csv('csv/complete_dataset_per2.csv')
Slovakia <- readOGR("./shapefiles/", "Slovakia")

#bind the data
Slovakia_data <- merge(Slovakia, dataset, by="Code")

myfunction <- function(inputParty, inputYear, ncrit= "contiguity", queen=FALSE, style="W", k=2){
  AIC_model_err <- 999
  AIC_model_lag <- 999
  Party <- paste(inputParty, inputYear, sep = "_")
  Year <- paste("U", inputYear, sep="")
  #first a regular regression
  model <- lm(Slovakia_data@data[,Party] ~ Slovakia_data@data[,Year])
  sum_model <- summary(model)
  # 1 - check whether the P-value and the values of coefficients in lm model are below 0.05 
  if(lmp(model) < 0.05 & sum_model$coefficients[1,4] < 0.05 & sum_model$coefficients[2,4] < 0.05){
  #2
  # - check whether there is spatial clusterification
  if(ncrit=="contiguity") {
    neighbours <- poly2nb(Slovakia_data, queen=queen) #create neigbours
    neighbours[9] <- ifelse(neighbours[9]==0, as.integer(6), neighbours[9]) #connect Petrzalka to Bratislava II - justification - urban population similarity
    plot(Slovakia_data, border = 'lightgrey')
    plot(neighbours, coordinates(Slovakia_data), add=TRUE, col='red')
    dev.off()
  }
  if(ncrit=="distance") {
    neighbours <- knn2nb(knearneigh(coordinates(Slovakia_data), k=k))
    pdf("neighbours.pdf")
    plot(Slovakia_data, border = 'lightgrey')
    plot(neighbours, coordinates(Slovakia_data), add=TRUE, col='red')
    dev.off()
  }
  listw <- nb2listw(neighbours, style=style) #create a listw object with weights for neighbours
  #3 - moran's tests
  if(moran.test(Slovakia_data@data[,Party], listw = listw)$p.value < 0.05 & lm.morantest(model, listw)$p.value < 0.05){
    #4- here continue - successful 
    alltests <- lm.LMtests(model, listw, test="all")
    # here check whether the p value from LMerr and LMlag indicates significance
    if(alltests$LMerr$p.value < 0.05 & alltests$LMlag$p.value < 0.05) {
      #test whether the p-value from the robust lag test is significant
      if(alltests$RLMerr$p.value > alltests$RLMlag$p.value) {
        #if the p-value of the spatially lagged regression is below 0.05 continue 
        model_lag <- lagsarlm(Slovakia_data@data[,Party] ~ Slovakia_data@data[,Year], listw= listw) 
        lag_sum <- summary(model_lag)
        #tests whether the p value of the model is below 0.05
        if(lag_sum$LR1$p.value <0.05){
          
          AIC_model_lag <<- AIC(model_lag) #saves AIC score
          
        }
      }
      else {
        model_err <- errorsarlm(Slovakia_data@data[,Party] ~ Slovakia_data@data[,Year], listw= listw)
        err_sum <- summary(model_err)
        #check whether the spatial error model is significant
        if(err_sum$LR1$p.value<0.05){
          AIC_model_err <<- AIC(model_err) # saves AIC score
          
        }
      }
    }
  #5 - GWR 
  Slovakia_data@data$unemployment <- Slovakia_data@data[,Year]
  Slovakia_data@data$votes <- Slovakia_data@data[,Party]#create a new column with the % votes for one particular party
  GWRbandwidth <- gwr.sel(Slovakia_data@data[,Party] ~ Slovakia_data@data[,Year],data=Slovakia_data,adapt=T, method="aic", verbose =FALSE) #calculate the bandwidth
  gwr.model = gwr(Slovakia_data@data[,Party] ~ Slovakia_data@data[,Year], data=Slovakia_data, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) #create gwr.model
  AIC_gwr <<- gwr.model$results$AICh #get the AIC from GWR
  #print(AIC_gwr) 
  results <- as.data.frame(gwr.model$SDF) #create dataframe with results from GWR
  names(results)[3] <- "Unemployment_coef" #rename the third column
  gwr.map <- Slovakia_data 
  gwr.map@data <- cbind(Slovakia_data@data, as.matrix(results)) #bind results onto the dataset
  #create visual maps
  map2 <- tm_shape(gwr.map) + tm_fill("votes", style= "quantile", title="Votes in %", palette = "Oranges") + tm_borders(alpha=.4)
  map3 <- tm_shape(gwr.map) + tm_fill("Unemployment_coef", style= "quantile",  title= "Unemployment coefficient") + tm_borders(alpha=.4)
  map4 <- tm_shape(gwr.map) + tm_fill("localR2", style= "quantile",  palette = "Greens") + tm_borders(alpha=.4)
  map5 <- tm_shape(gwr.map) + tm_fill("unemployment", style= "quantile", title = "Unemployment in %", palette = "Reds") + tm_borders(alpha=.4)
  
  #plot data in a Grid
  grid.newpage()
  # assigns the cell size of the grid, in this case 2 by 2
  pushViewport(viewport(layout=grid.layout(2,2)))
  # prints a map object into a defined cell   
  print(map2, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
  print(map5, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
  print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
  print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
  grid.text(Party, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1:2))
  #Print the standard OLS regression- even though the output might be biased it is easier to interpret and therefore will be used
  print(screenreg(model, custom.coef.names = c("Intercept", Year)))
  #Check whether the AIC score is the highest from GWR
  if(AIC_model_err == min(c(AIC_model_err, AIC_model_lag, AIC_gwr))){
    print("The spatial error model has a lower AIC than the GWR model and may be therefore interesting to look at.")
  }
  if(AIC_model_lag == min(c(AIC_model_err, AIC_model_lag, AIC_gwr))){
    print("The spatial lag model has a lower AIC than the GWR model and may be therefore interesting to look at.")
  }
  }
  else {
    print(screenreg(model))
    print("The Moran's I test for spatial autocorrelation is insignificant.")
    return("The function will break here.")  
  }
  } 
  else {
  print("The regression model is not significant with the model p-value higher than 0.05 or one of the coefficient p-values above 0.05.")
    print(screenreg(model, custom.coef.names = c("Intercept", Year)))
  return("The function will break here.")
  }
}

myfunction("SaS", 201603)

