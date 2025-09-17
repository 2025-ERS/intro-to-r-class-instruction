# --------------------------HEADER ----------------
# title: 10-logistic-regression.R
# author: Han Olff
# date: 2025-9-16
# description: Generalized linear regression - logistic regression, and working with functions
# input databases: MicrotransectDB

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")
gsheets_auth()
library(glmmTMB)
library(DHARMa)

#--------------------------02 Read and combine the datasets -----
# read the microtransect data, noting that the data are in wide format
# database: microtransectDB
# browseURL("https://docs.google.com/spreadsheets/d/1dJkH09imko9RgOkGzYQT74IeGY56QwiXjjBl7u0KcK0/")

# show the variable names of the table FactVegClay



# in MicrotransectDB$FactVegClay, the presence per species is stored in wide format, convert to long format with Species as a variable
# keeping only the 2025 data, define the new variables Species (name) and Present (0,1)
MicrotransectDB$FactVegClay<-

# calculate in addition to the clay layer also a sand layer depth
MicrotransectDB$Soil2025<-MicrotransectDB$FactVegClay |>
  dplyr::filter(Year==2025) |>
  dplyr::mutate(SandDepth_cm=100*Elevation_m - ClayDepth_cm) |>
  dplyr::select(Point_ID,SandDepth_cm,ClayDepth_cm) |>
  tidyr::pivot_longer(-Point_ID,  # what to keep
                      names_to="SoilLayer",
                      values_to = "LayerThickness_cm") 
MicrotransectDB$Soil2025

# plot the depth of the sand ayer and the clay layer
p1<-MicrotransectDB$Soil2025 |>
  ggplot(aes(x=Point_ID,y=LayerThickness_cm,fill=SoilLayer)) +
  geom_area() +
  coord_cartesian(ylim=c(95,125)) + 
  scale_fill_manual(
    values = c(
      "SandDepth_cm" = "khaki",       # sand colour
      "ClayDepth_cm" = "saddlebrown"  # brown
    )
  ) +
  labs(x="distance along transect (m)",y="layer thickness (cm)")
p1

# show the species presence along the transect as a dot graph
MicrotransectDB$PlantPres<-MicrotransectDB$FactVegClay2025 %>% 
  dplyr::filter(Present==1) %>% 
  mutate(SpeciesNum=as.numeric(factor(Species)))
p2<-ggplot(data=MicrotransectDB$PlantPres,aes(x=Point_ID,y=max(SpeciesNum)-SpeciesNum,col=Species)) +
  geom_point(size=2) +
  labs(x=NULL,y=NULL) +
  theme(axis.text.x = element_blank(),  # remove x tick labels
        axis.text.y = element_blank())   # remove y tick labels
p2

# put the two graphs in one panel figure (vertical above eachother)


# calculate the frequency (as a proportion) of occurrence of each species 
# in 2023, and sort according to frequency


DomSpec <- c("Puccinellia.maritima","Salicornia.europaea","Limonium.vulgare",
             "Plantago.maritima","Glaux.maritima","Salicornia.procumbens")


# fit a logistic regression model for Limonium vulgare

# define a set of helper functions
#----------------------------------------Helpers --------------------------------------
# 1) function to fit a logistic regression logit(Present)~Elevation_m


# 2) function to fit a logistic regression logit(Present)~Elevation_m+I(Elevation_m^2)

# 3) add predicted probability of occurrence for a particular species to the dataset


#####--------------------------------Fit logistic regression models 

# Model Puccinellia.maritima and add predicted values to the dataset


# Model Limonium vulgare and add predicted values to the dataset

# Model Salicornia.europaea and add predicted values to the dataset

# Model Salicornia.procumbens and add predicted values to the dataset


##### -----------------------Plot the results
# plot the 6 most frequently occurring species with present/absence in a panel plot, with predicted curve


# Plot the predicted values (without observed values) of the different species in 1 plot


# Combine the last two plots in a vertical panel plot

