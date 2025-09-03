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
# cockles database: 
# elevation database: 
# browseURL("https://docs.google.com/spreadsheets/d/1UmpZoNIjo5lXdcpGKuBHe7wFX1Z3AQioeHjzrFgYnxo/edit?usp=sharing")
# browseURL("https://docs.google.com/spreadsheets/d/1E1vUWAHhse7fhjBf94Kiog3Rsj7IkMuFtP-BTQX2oI8/edit?usp=sharing")
# note that the above opens the whole database in Google Sheets, not the CSV files

#--------------------------03 Read and combine the data -----
# load the elevation data and show the first 10 records of the dataset
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv")
elevdat

# plot the change in transect  elevation along the transect, using a separate graph for each for each year 

# plot the change in transect  elevation along the transect, using a separate line color for each year 


# Extract the data for 2017 in a new tibble, keep only variables distance_m and elevation
# omit records where Distance_ID is missing (NA)

# read the cockle data 
# keep only the data for 2017, 
# omit observations (Obs_ID) 468 and 1531
# calculate the mean number of cockles and mean size for each distance
cdat2017 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSpormjGiM4uWMTIZYm9Upt5j1Ige_Wu9CrhGGdvjXwxmHP2R6S7JDyEmiGTn3fXihl5s5yBn_sdo8h/pub?gid=1538766002&single=true&output=csv") 


# plot (with a line and points)  how the number of cockles changes with distance along the transect


##### merge the cockle and elevation data into a single table you call "combidat"
# using Distance_ID as the common variable between the two tables

# replace in variable n_obs all NA with zero


# show in a plot how cockle density changes with elevation

# fit a linear regression

# show this model as a line in ggplot, with the confidence interval

# fit a better model, using a loess smoother
# show this model in ggplot


##### plot  how the size (as mean length) of cockles changes with  elevation along the transect
# omit the observations where length is NA (because no cockles were measures)
# fit a quadratic model (second-order polynomial)
# show for each point also the standard errors
# add appropriate labels for the x and y axis 

