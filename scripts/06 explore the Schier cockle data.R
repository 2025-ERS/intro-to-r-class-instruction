# --------------------------HEADER ----------------
# title: "06 explore the Schier cockle data.R"
# author: "Han Olff"
# date: "2025-9-02"
# description: "Combine cockle data with elevation data using a relational database approach"
# input databases: elevation and epibenthos
# output: "No output files, but the script produces some plots"

#--------------------------01 Set up the environment ----
rm(list=ls()) # clear working memory
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
library(renv) # load renv library
renv::restore() # restore the packages in the renv.lock file
library(tidyverse) # load the tidyverse package

#--------------------------02 explore the input databases   -----
# potentially view the input databases (remove # before browseURL to run the commands)
# epibenthos database: 
# browseURL("https://docs.google.com/spreadsheets/d/1E1vUWAHhse7fhjBf94Kiog3Rsj7IkMuFtP-BTQX2oI8/edit?gid=1358417603#gid=1358417603")
# elevation database: 
# browseURL("https://docs.google.com/spreadsheets/d/1UmpZoNIjo5lXdcpGKuBHe7wFX1Z3AQioeHjzrFgYnxo/edit?usp=sharing")
# browseURL("https://docs.google.com/spreadsheets/d/1E1vUWAHhse7fhjBf94Kiog3Rsj7IkMuFtP-BTQX2oI8/edit?usp=sharing")
# note that the above opens the whole database in Google Sheets, not the CSV files

#--------------------------03 Read and combine the data -----
# load the elevation data and show the first 10 records of the dataset
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") 
  
elevdat
names(elevdat)

# plot the change in transect  elevation along the transect, using a separate graph for each for each year 
elevdat |>
  ggplot2::ggplot(aes(x=distance_rtk_m,y=elevation_m)) +
  geom_line() +
  facet_grid(rows=vars(year))

# plot the change in transect  elevation along the transect, using a separate line color for each year 
elevdat |>
  ggplot2::ggplot(aes(x=distance_rtk_m,y=elevation_m, 
                      color=as.factor(year)
                      )
                  ) +
  geom_line()


# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Transectpoint_ID is missing (NA)
elevdat2017 <- elevdat |>
  dplyr::filter(year==2017,!is.na(TransectPoint_ID)) |>
  dplyr::select(TransectPoint_ID,elevation_m)
print(elevdat2017)
  

# read the cockle data 
# keep or filter only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# only keep data for the first three replicates (replicate<=3)
# calculate the mean number of cockles and mean size for each distance
cdat2017 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSpormjGiM4uWMTIZYm9Upt5j1Ige_Wu9CrhGGdvjXwxmHP2R6S7JDyEmiGTn3fXihl5s5yBn_sdo8h/pub?gid=1538766002&single=true&output=csv") |>
  dplyr::filter(year==2017,replicate<=3,
                !CockleObs_ID %in% c(468,1531)) |>
  dplyr::group_by(TransectPoint_ID) |>
  dplyr::summarise(n_obs=n(),
                   avg_l=mean(length_mm,na.rm=T),
                   sd_l=sd(length_mm,na.rm=T), # measure of group variation 
                   se_l=sd_l/sqrt(n_obs)
                   ) # measure of how precise you measure the mean 
print(cdat2017)


# plot (with a line and points)  how the number of cockles changes with distance (Transectpoint_ID) along the transect
cdat2017 |>
  ggplot2::ggplot(aes(x=TransectPoint_ID,y=n_obs)) +
  geom_point() +
  geom_line() +
  labs(x="transect point", y="number of cockles per 3 plots")


##### merge the cockle and elevation data into a single table you call "combidat"
# using Distance_ID as the common variable between the two tables
# also replace in variable n_obs all NA with zero

combidat <- 
  dplyr::left_join(elevdat2017,cdat2017,
                             by="TransectPoint_ID") |>
  dplyr::mutate(n_obs=tidyr::replace_na(n_obs,0)) #replace na by 0
  
combidat


# show in a plot how cockle density changes with elevation
combidat |>
  ggplot2::ggplot(aes(x=elevation_m,y=n_obs)) +
  geom_point(size=2) +
  geom_line()

# fit a linear regression ~ "depends on / as a function of"
# lm(response_variable~predictor_variable)
linreg<-stats::lm(n_obs~elevation_m, data=combidat)
summary(linreg)

# show this model as a line in ggplot, with the confidence interval

combidat |>
  ggplot2::ggplot(aes(x=elevation_m,y=n_obs)) +
  geom_point(size=2) +
  geom_smooth(method="lm",formula="y~x")

# fit a better model, using a loess smoother
# show this model in ggplot
combidat |>
  ggplot2::ggplot(aes(x=elevation_m,y=n_obs)) +
  geom_point(size=2) +
  geom_smooth(method="loess",fill=NA)

##### plot  how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 
combidat |>
  dplyr::filter(!is.na(avg_l)) |>
  ggplot2::ggplot(aes(x=elevation_m,y=avg_l)) +
    geom_point() +
    geom_smooth(method="lm", formula="y~x", fill=NA) +
    geom_smooth(method="lm", formula="y~x+I(x^2)", 
                color="red",fill=NA) +
    xlim(0,0.8) +
    labs(x="elevation in m + NAP", y="mean cockle length (mm)")
  
  
  
# test with linear regression if this relation is significant
linreg2<-stats::lm(avg_l~elevation_m, data=combidat)
summary(linreg2)
linreg3<-stats::lm(avg_l~elevation_m+I(elevation_m^2), data=combidat)
summary(linreg3)

