# --------------------------HEADER ----------------
# title: "08_linear_models.R"
# author: "Han Olff"
# date: "2025-9-13"
# description: "Introduction to linear models - single and multiple linear regression"
# subject: how Orchestia gammarellus changes with elevation along the gradient
# input databases: macrodetritivore data 
# output: "No output files, but the script produces some plots"

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")
gsheets_auth()

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

##### create an additional table in the database through a (in SQL terminology) a "relational query"
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
p1<-SchierTidesDB$FactTransProb |> dplyr::filter(Year>=2017) |>
  ggplot(aes(x=Sealevel_m,y=TransProb,col=factor(Year))) +
    geom_line(linewidth=1.2) +
   xlim(1,2) + ylim(0,0.2) +
   ylab("transgression probability 1 mar - 30 aug")
p1

# how  sea level has changed since 1990 (expressed as transgression probability of 3 elevations)
p2<-SchierTidesDB$FactTransProb |> dplyr::filter(Sealevel_m %in% c(-1,0,1), Year>=1990) |>
   ggplot(aes(x=Year,y=TransProb, color=factor(Sealevel_m))) +
     geom_point() +
     geom_smooth(method="lm") +
     ylab("transgression probability 1 mar - 30 aug")
p2

# plot Orchestia (y) versus elevation_m (x) in ggplot as a scatterplot, with each year as a different color
p3<- MacrodetritivoresDB$OrchestiaElev|>
  ggplot(aes(x=Elevation_m,y=Orchestia_n,color=Year)) +
  geom_point(size=3) +
  xlim(1,2)
p3


# add the two plots p1 and p3 above eachother in a panel, using the patchwork library
p1 / p3

# calculate the optimal preferred elevation by Orchestia for each year as the weighted average elevation (using weighted.mean function)
# store the result in the graphical object p1
MacrodetritivoresDB$OrchestiaElev2<-MacrodetritivoresDB$OrchestiaElev |>
  group_by(Year) |>
  summarize(OptimalElev_m=weighted.mean(x=Elevation_m,w=Orchestia_n,na.rm=T))


## explore response to elevation and year as a linear model, call this m1
# first only elevation (not yet year)

m1<-lm(Orchestia_n~Elevation_m,data=MacrodetritivoresDB$OrchestiaElev)
summary(m1)

#add the linear model to the plot
MacrodetritivoresDB$OrchestiaElev
MacrodetritivoresDB$OrchestiaElev$predm1<-predict(m1)
p5<-p3+ geom_line(data=MacrodetritivoresDB$OrchestiaElev,
                  aes(y=predm1),
                  col="black",
                  size=1.2)
p5

# fitmodel  m2 by adding a quadratic term for elevation_m to check for an ecological optimum
# y=b0 + b1x1 + b2x1^2
m2<-lm(Orchestia_n~Elevation_m+I(Elevation_m^2),data=MacrodetritivoresDB$OrchestiaElev)
summary(m2)
MacrodetritivoresDB$OrchestiaElev$predm2<-predict(m2)
p6<-p5+ geom_line(data=MacrodetritivoresDB$OrchestiaElev,
                  aes(y=predm2),
                  col="black",
                  linetype="dashed",
                  size=1.2)
p6
# test if the new model m2   significantly explains more variation than the first model m1
# although this is also shown by significance of the quadratic term in summary(m2)
anova(m1,m2)



# explore the consequences of a log transformation of y values
x<-c(0,1,2,3,4,5)
y<-c(1,10,100,1000,10000,100000)
dat<-data.frame(x,y)
dat
dat %>% ggplot(aes(x=x,y=y)) +
  geom_point(shape=16,size=5) +
  geom_line(size=1.2)
dat %>% ggplot(aes(x=x,y=log10(y))) +
  geom_point(shape=16,size=3) +
  geom_line(size=1.2)
dat %>% ggplot(aes(x=x,y=log(y))) + # use log with base number e
  geom_point(shape=16,size=3) +
  geom_line(size=1.2)
# explore the consequences of the log base number (10 or e)
log(0)
log10(1)
log10(10)
log(0)
log(1)
exp(1)
log(exp(1))

### develop, test for significance and plot different models of increasing complexity using a glm approach
#  using  multiple regresssion, assuming a poisson distribution (so use a generalized linear model)
# Explore  how the abundance of Orchestia depends on elevation_m and year,  their potential interaction,
# and a potential ecological optimum of Orchestia with respect to elevation_m
# show the effect of elevation but now in a generalized linear model instead of linear model, using a log link function and a poisson distribution
x<-MacrodetritivoresDB$OrchestiaElev$Elevation_m
y<-MacrodetritivoresDB$OrchestiaElev$Orchestia_n
names(dat)
m3 <- glm(y ~ x +I(x^2),
          family = poisson(link = "log"))

m3
MacrodetritivoresDB$OrchestiaElev$predm3<-exp(predict(m3))
anova(m3, test="Chisq")

p8<-p3+ geom_line(data=MacrodetritivoresDB$OrchestiaElev,
                  aes(y=predm3),
                  col="black",
                  linewidth=1.2)
p8
anova(m5,test="Chisq")
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it



# now test and show  the effect of both elevation , elevation squared and year
m6<-glm(Orchestia_n~elevation_m+I(elevation_m^2)+factor(year),
        family=poisson(log),
        data=orchdat2)
anova(m6,test="Chisq")
#add the linear model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat2$pred6<-exp(predict(m6))  # backtransform the predicted values
orchdat2
p5<-p1+ geom_line(data=orchdat2,
                  aes(y=pred6,col=factor(year)),
                  size=1.2,linetype="solid")
p5
# better than the previous?
anova(m5,m6,test="Chisq")

# add the interaction to the model: elevation + elevation ^2 + year + elevation*year
# now test and show  the effect of both elevation + year
orchdat2
m7<-glm(Orchestia_n~elevation_m+I(elevation_m^2)+factor(year)+elevation_m:factor(year),
        family=poisson(log),
        data=orchdat)
anova(m7,test="Chisq")
anova(m6,m7,test="Chisq")
#add the  model to the plot
# calculate the predicted value of m2 for every observation, add to the dataset as a variable as pred2
# add the new predicted line to the previous plot p2, store as object p3 and show it
orchdat$pred7<-exp(predict(m7))  # backtransform the predicted values
p6<-p1+ geom_line(data=orchdat,
                  aes(y=pred7,col=factor(year)),
                  size=1.2,linetype="solid")
p6

p0 / p6


