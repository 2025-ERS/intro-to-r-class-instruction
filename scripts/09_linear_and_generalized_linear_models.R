# --------------------------HEADER ----------------
# title: "09_linear_models.R"
# author: "Han Olff"
# date: "2025-9-13"
# description: "Introduction to linear models - single and multiple linear regression"
# subject: how Orchestia gammarellus changes with elevation along the gradient
# input databases: macrodetritivore data 
# output: "No output files, but the script produces some plots"

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")


#--------------------------02 Read and combine the datasets -----
#### 02.1) macrotransect database
# read the whole macrotransect database as list object into R, creating separate tibbles for each sheet within this list, by using the helper function read_gsdb() as defined in the script 00-helpers.R (see there also how to use it)
# use the gid link to the database, so not the csv link to one of its tables!
MacrotransectDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1UmpZoNIjo5lXdcpGKuBHe7wFX1Z3AQioeHjzrFgYnxo")  
# show the tables in the database
MacrotransectDB$MetTables
# show the variables used in the table FactElevation
MacrotransectDB$MetVariables |> dplyr::filter(Table_ID=="FactElevation")  


##### 02.2) read the macrodetritivore database
# filter to use only years 2018,2019,2021,2022,2023
MacrodetritivoresDB<-read_gsdb("https://docs.google.com/spreadsheets/d/13PCiw3rkXnbcBSMn8ejtjdEXAo_-2uf9OU6VTFuNK7I")  
# list  the tables in the database
MacrodetritivoresDB$MetTables
# list  the variables in the table FactSpeciesCount
MacrodetritivoresDB$MetVariables |> dplyr::filter(Table_ID=="FactSpeciesCount")

##### create an additional table in the database through a "relational query"
##### that holds the summed Orchestia count per TransectPoint
MacrodetritivoresDB$Orchestia <- MacrodetritivoresDB$FactSpeciesCount |>
  # filter to only use replicates 1, 2 and 3 of each year,
  # and only include 2017,2018, 2019,2021, 2023, 2024, 2025
  dplyr::filter(Year %in% c(2017:2019,2021,2023:2025), 
                Replicate<=3,
                SpeciesCode=="Orchestia_gammarellus") |>
  # group by year and TransectPoint_ID
  dplyr::group_by(Year,TransectPoint_ID) |>
  # calculate the sum of the number of Orchestia found per year and Distance_ID
  dplyr::summarize(Orchestia_n=sum(Count,na.rm=T)) 


MacrodetritivoresDB$Orchestia

# merge the elevation data since 2017 with the Orchestia data, adding zero observations
MacrodetritivoresDB$OrchestiaElev<-dplyr::left_join(MacrotransectDB$FactElevation |> dplyr::filter(Year %in% c(2017:2019,2021,2023:2025)),
                                                    MacrodetritivoresDB$Orchestia,
                                                    by=c("Year","TransectPoint_ID")) |>
  # replace NA in Orchestia by zero
  dplyr::mutate(Orchestia_n=ifelse(is.na(Orchestia_n),0,as.integer(Orchestia_n))) |>
  # keep only the relevant variables
  dplyr::select(Year, TransectPoint_ID,Orchestia_n, Elevation_m) |>
  # make year into a factor
  dplyr::mutate(Year=factor(Year),
                Orchestia_n=as.integer(Orchestia_n))  |> 
  # keep only the relevant and correct data points
  dplyr::filter(TransectPoint_ID>=207 & TransectPoint_ID != 260 & TransectPoint_ID<=1000)

# view the result, note the addition of the zeros!
MacrodetritivoresDB$OrchestiaElev


##### 02.3) read the Schiermonnikoog tides data database, only the FactTransProb table (rest takes long time) and its metadata
# this takes about 2 minutes
SchierTidesDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1DOzvscotzWXm5MmEFrZhvPY820weEZVcSfUYz_5Gyf4",
                         sheets=c("MetTables", "MetVariables", "FactTransProb"))  
SchierTidesDB$MetTables
SchierTidesDB$MetVariables

#--------------------------03 explore the  data -------------------------------
#  how the transgression probability changes with elevation

# how  sea level has changed since 1990 (expressed as transgression probability of 3 elevations)

# plot Orchestia (y) versus elevation_m (x) in ggplot as a scatterplot, with each year as a different color


# add the two plots p1 and p3 above eachother in a panel, using the patchwork library

# calculate the optimal preferred elevation by Orchestia for each year as the weighted average elevation (using weighted.mean function)
# store the result in the graphical object p1


## explore response to elevation and year as a linear model, call this m1
# first only elevation (not yet year)


#add the linear model to the plot

# fitmodel  m2 by adding a quadratic term for elevation_m to check for an ecological optimum
# y=b0 + b1x1 + b2x1^2


# test if the new model m2   significantly explains more variation than the first model m1
# although this is also shown by significance of the quadratic term in summary(m2)
anova(m1,m2)



# explore the consequences of a log transformation of y values
x<-c(0,1,2,3,4,5)
y<-c(1,10,100,1000,10000,100000)
dat<-data.frame(x,y)
dat



### develop, test for significance and plot different models of increasing complexity using a glm approach
#  using  multiple regresssion, assuming a poisson distribution (so use a generalized linear model)
# Explore  how the abundance of Orchestia depends on elevation_m and year,  their potential interaction,
# and a potential ecological optimum of Orchestia with respect to elevation_m
# show the effect of elevation but now in a generalized linear model instead of linear model, using a log link function and a poisson distribution
x<-MacrodetritivoresDB$OrchestiaElev$Elevation_m
y<-MacrodetritivoresDB$OrchestiaElev$Orchestia_n
names(dat)


anova(m5,test="Chisq")
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it



# now test and show  the effect of both elevation , elevation squared and year


#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it


# better than the previous?


# add the interaction to the model: elevation + elevation ^2 + year + elevation*year
# now test and show  the effect of both elevation + year


#add the  model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
