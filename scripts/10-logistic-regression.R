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
MicrotransectDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1dJkH09imko9RgOkGzYQT74IeGY56QwiXjjBl7u0KcK0/")

# show the variable names of the table FactVegClay
names(MicrotransectDB$FactVegClay)


# in MicrotransectDB$FactVegClay, the presence per species is stored in wide format, convert to long format with Species as a variable
# keeping only the 2025 data, define the new variables Species (name) and Present (0,1)
# sort the dataframe for year, species and Point_ID
# filter it for year 2025 and exclude species Salicornia.sp
MicrotransectDB$FactVegClay2025<- MicrotransectDB$FactVegClay |>
  tidyr::pivot_longer(cols = -c(Point_ID:ClayDepth_cm),  # what to keep
                      names_to="Species",
                      values_to = "Present") |>
  dplyr::arrange(Year,Species,Point_ID) |>
  dplyr::filter(Year==2025 & Species!="Salicornia.sp")
MicrotransectDB$FactVegClay2025

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
p1 / p2

# calculate the frequency (as a proportion) of occurrence of each species 
# in 2025, and sort according to decreasing frequency
SpeciesFreq2025<-MicrotransectDB$FactVegClay2025 %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(Freq=mean(Present)) %>%
  dplyr::arrange(desc(Freq))
SpeciesFreq2025

DomSpec <- c("Puccinellia.maritima","Salicornia.europaea","Limonium.vulgare",
             "Plantago.maritima","Glaux.maritima","Salicornia.procumbens")
DomSpec

# fit a logistic regression model for Glaux.maritima
m.Glaux1<-MicrotransectDB$FactVegClay2025 |>
  dplyr::filter(Species=="Glaux.maritima") |>
  glmmTMB::glmmTMB(Present~Elevation_m,
                   family = binomial(link="logit"),
                   data=_)
summary(m.Glaux)

m.Glaux2<-MicrotransectDB$FactVegClay2025 |>
  dplyr::filter(Species=="Glaux.maritima") |>
  glmmTMB::glmmTMB(Present~Elevation_m + I(Elevation_m^2),
                   family = binomial(link="logit"),
                   data=_)
summary(m.Glaux2)


# define a set of helper functions
#----------------------------------------Helpers --------------------------------------
# 1) function to fit a logistic regression logit(Present)~Elevation_m
fit1<- function(species_name, db=MicrotransectDB$FactVegClay2025) {
  db |>
  dplyr::filter(Species==species_name) |>
  glmmTMB::glmmTMB(Present~Elevation_m,
                   family = binomial(link="logit"),
                   data=_)
}

# 2) function to fit a logistic regression logit(Present)~Elevation_m+I(Elevation_m^2)
fit2<- function(species_name, db=MicrotransectDB$FactVegClay2025) {
  db |>
    dplyr::filter(Species==species_name) |>
    glmmTMB::glmmTMB(Present~Elevation_m+I(Elevation_m^2),
                     family = binomial(link="logit"),
                     data=_)
}

# 3) add predicted probability of occurrence for a particular species to the dataset
add_predictions<- function(species_name,model,db=MicrotransectDB$FactVegClay2025) {
  db %>%
    dplyr::mutate(
      Predicted = if (any(names(.) == "Predicted")) Predicted else NA_real_,
      Predicted = if_else(
        Species == species_name,
        predict(model,newdata = pick(everything()), type="response"),
        Predicted
      )
    )
}

#####--------------------------------Fit logistic regression models 
DomSpec
# fit the models for Glaux.maritima
summary(fit1("Glaux.maritima"))
summary(fit2("Glaux.maritima"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
   add_predictions(species_name="Glaux.maritima",model=fit1("Glaux.maritima"))
names(MicrotransectDB$FactVegClay2025)

# Model next species and add predicted values to the dataset
summary(fit1("Limonium.vulgare"))
summary(fit2("Limonium.vulgare"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
   add_predictions(species_name="Limonium.vulgare",model=fit1("Limonium.vulgare"))

# Model next species and add predicted values to the dataset
summary(fit1("Plantago.maritima"))
summary(fit2("Plantago.maritima"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
  add_predictions(species_name="Plantago.maritima",model=fit2("Plantago.maritima"))

# Model next one and add predicted values to the dataset
summary(fit1("Puccinellia.maritima"))
summary(fit2("Puccinellia.maritima"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
  add_predictions(species_name="Puccinellia.maritima",model=fit1("Puccinellia.maritima"))

# Model next one and add predicted values to the dataset
summary(fit1("Salicornia.europaea"))
summary(fit2("Salicornia.europaea"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
  add_predictions(species_name="Salicornia.europaea",model=fit2("Salicornia.europaea"))


# Model next one and add predicted values to the dataset
summary(fit1("Salicornia.procumbens"))
summary(fit2("Salicornia.procumbens"))
# choose which model to use, and add its predictions to the dataframe
MicrotransectDB$FactVegClay2025 <-
  add_predictions(species_name="Salicornia.procumbens",model=fit1("Salicornia.procumbens"))


##### -----------------------Plot the results
# plot the 6 most frequently occurring species with present/absence in a panel plot, with predicted curve
DomSpec 
p4<-MicrotransectDB$FactVegClay2025 |> dplyr::filter(Species %in% DomSpec) |>
  ggplot(aes(x = Elevation_m, y = Present)) +
  geom_point(size = 1) +
  geom_line(aes(y=Predicted)) +
  labs(
    title = "Microtransect",
    x = "Elevation (m)",
    y = "Presence"
  ) +
  ylim(-1,2) +
  facet_wrap(~Species)
p4


# Plot the predicted values (without observed values) of the different species in 1 plot
p5<-MicrotransectDB$FactVegClay2025 |> dplyr::filter(Species %in% DomSpec) |>
  ggplot(aes(x = Elevation_m, y = Present, color=Species)) +
  geom_line(aes(y=Predicted),linewidth=2) +
  labs(
    title = "Microtransect",
    x = "Elevation (m)",
    y = "Presence"
  ) +
  ylim(0,1) 
p5

# Combine the last two plots in a vertical panel plot

