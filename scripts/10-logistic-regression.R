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
MicrotransectDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1dJkH09imko9RgOkGzYQT74IeGY56QwiXjjBl7u0KcK0")
MicrotransectDB$MetTables

# show the variable names of the table FactVegClay
names(MicrotransectDB$FactVegClay)

# as the presence per species is stored in wide format, convert to long format
MicrotransectDB$FactVegClay2025<-MicrotransectDB$FactVegClay |> 
  tidyr::pivot_longer(-c(Point_ID,Year,X,Y,Elevation_m,Distance_rtk_m,ClayDepth_cm), # keep all variables that are not a species
                      names_to="Species",   # variable to which remaining variables are collapsed
                      values_to = "Present") |>
  # sort the dataframe
  dplyr::arrange(Year,Species,Point_ID) %>%
  dplyr::filter(Year==2025, Species!='Salicornia.sp')
MicrotransectDB$FactVegClay2025


MicrotransectDB$Soil2025<-MicrotransectDB$FactVegClay2025 |>
  dplyr::mutate(SandDepth_cm=100*Elevation_m - ClayDepth_cm) |>
  dplyr::select(Point_ID,SandDepth_cm,ClayDepth_cm) |>
  tidyr::pivot_longer(-Point_ID,  # what to keep
                      names_to="SoilLayer",
                      values_to = "LayerThickness_cm") 
MicrotransectDB$Soil2025

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
p2 / p1 # see https://ggplot2-book.org/arranging-plots


# calculate the frequency (as a proportion) of occurrence of each species 
# in 2023, and sort according to frequency
MicrotransectDB$FactVegClay2025 |> 
  group_by(Species) %>%
  summarize(PropPoints=mean(Present,na.rm=T)) %>%
  arrange(-PropPoints)
DomSpec <- c("Puccinellia.maritima","Salicornia.europaea","Limonium.vulgare",
             "Plantago.maritima","Glaux.maritima","Salicornia.procumbens")


# fit a logistic regression model 
m1<-MicrotransectDB$FactVegClay2025 |>
  dplyr::filter(Species == "Limonium.vulgare") |>
  glmmTMB::glmmTMB(Present~Elevation_m ,
                   family=binomial(link=logit),
                   data=_) 
summary(m1)
#----------------------------------------Helpers --------------------------------------

# 1) function to fit a logistic regression logit(Present)~Elevation_m
fit1 <- function(species_name, db = MicrotransectDB$FactVegClay2025) {
  db |>
    dplyr::filter(Species == species_name) |>
    glmmTMB::glmmTMB(Present~Elevation_m ,
                     family=binomial(link=logit),
                     data=_) 
}
# 2) function to fit a logistic regression logit(Present)~Elevation_m+I(Elevation_m^2)
fit2 <- function(species_name, db = MicrotransectDB$FactVegClay2025) {
  db |>
    dplyr::filter(Species == species_name) |>
    glmmTMB::glmmTMB(Present~Elevation_m +I(Elevation_m^2) ,
                     family=binomial(link=logit),
                     data=_) 
}

# 3) add predicted probability of occurrence for a particular species to the dataset
add_predictions <- function(db, species_name, model) {
  db %>%
    mutate(
      Predicted = if (any(names(.) == "Predicted")) Predicted else NA_real_,
      Predicted = if_else(
        Species == species_name,
        predict(model, newdata = pick(everything()), type = "response"),
        Predicted
      )
    )
}


#####--------------------------------Fit logistic regression models 
DomSpec

# Model Puccinellia.maritima and add predicted values to the dataset
summary(fit1("Puccinellia.maritima"))
summary(fit2("Puccinellia.maritima"))
MicrotransectDB$FactVegClay2025<-
  add_predictions(MicrotransectDB$FactVegClay2025,"Puccinellia.maritima",fit1("Puccinellia.maritima"))
view(MicrotransectDB$FactVegClay2025)
# Model Limonium vulgare and add predicted values to the dataset
summary(fit1("Limonium.vulgare"))
summary(fit2("Limonium.vulgare"))
MicrotransectDB$FactVegClay2025<-
  add_predictions(MicrotransectDB$FactVegClay2025,"Limonium.vulgare",fit1("Limonium.vulgare"))

# Model Salicornia.europaea and add predicted values to the dataset
summary(fit1("Salicornia.europaea"))
summary(fit2("Salicornia.europaea"))
MicrotransectDB$FactVegClay2025<-
  add_predictions(MicrotransectDB$FactVegClay2025,"Salicornia.europaea",fit2("Salicornia.europaea"))

# Model Salicornia.procumbens and add predicted values to the dataset
summary(fit1("Salicornia.procumbens"))
summary(fit2("Salicornia.procumbens"))
MicrotransectDB$FactVegClay2025<-
  add_predictions(MicrotransectDB$FactVegClay2025,"Salicornia.procumbens",fit1("Salicornia.procumbens"))



##### -----------------------Plot the results
# plot the 6 most frequently occurring species
DomSpec 
p4<-MicrotransectDB$FactVegClay2025 |> dplyr::filter(Species %in% DomSpec) |>
  ggplot(aes(x = Elevation_m, y = Present)) +
  geom_point(size = 1) +
  #  geom_line(aes(y=Predicted)) +
  labs(
    title = "Microtransect",
    x = "Elevation (m)",
    y = "Presence"
  ) +
  ylim(0,1) +
  facet_wrap(~Species)
p4

# Plot the predicted values of the different species in 1 plot
p5<-MicrotransectDB$FactVegClay2025 |> dplyr::filter(Species %in% DomSpec) |>
  ggplot(aes(x = Elevation_m, y = Present, color=Species)) +
  geom_line(aes(y=Predicted)) +
  labs(
    title = "Microtransect",
    x = "Elevation (m)",
    y = "Presence",
    ylim(0,1)
  ) 
p5

p6<-MicrotransectDB$FactVegClay2025 |>
  dplyr::filter(Species %in% DomSpec) |>
  ggplot(aes(x = Elevation_m)) +
  geom_histogram(binwidth = 0.010, alpha = 0.6, position = "identity", color = "black") +
  labs(
    title = "Microtransect",
    x = "Elevation (m)",
    y = "Count"
  )
p6

p5 /p6

