# --------------------------HEADER ----------------
# title: "07 long wide and multivariate data.R"
# author: "Han Olff"
# date: "2025-9-02"
# description: "reshape wide data into long data using tidyr and ggplot the results"
# input databases: vegetation data
# output: "No output files, but the script produces some plots"

#--------------------------01 Set up the environment ----
rm(list=ls()) # clear working memory
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
library(renv) # load renv library
renv::restore() # restore the packages in the renv.lock file
library(tidyverse) # load the tidyverse package

#--------------------------02 Read and reshape the data -----
# input vegetation database: 
# browseURL("https://docs.google.com/spreadsheets/d/1WIzkUvunLk1buybZ04Oko8wnF65EbuXjWEviWbeAwu4/edit?gid=1958021242#gid=1958021242")

# read the vegetation data from the google sheet
vdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJFU7gDlXuBM6hgWwcYJ-c_ofNdeqBe50_lDCEWO5Du3K7kPUDRh_esyKuHpoF_GbmBAoT4ZygrGWq/pub?gid=2036214007&single=true&output=csv")

# show the variables in the dataset 

# show in which unique years  data were recorded

# reshape the data using tidyr piping from wide to long format, where species is a single variable instead of distributed over multiple columns, treat bare, litter and moss as if they are species (see the "data import cheatsheet")
# remove Salicornia.europaea and Salicornia.procumbens from the dataset
# as Salicornia.sp is their sum (the 2 species where not separated in earlier years)
# also remove the variables bare,litter,mosses 

#show the names of all the species in the dataset


# find the most abundant species in the dataset
# add a variable to the dataset that is the rank number of the species 
# according to summed abundance of each species over
# the whole dataset (1=most abundant species)


# merge the two files

### plot the 5 most dominant species as a line diagram, cover (y) versus distance_m (x)with ggplot, separate plot for each year, each species with a different line color

# plot the change in cover along the distance  transect 
# and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species


# save the file to a png file of  1920 x 1080 pixels (first make a folder figures)
# specifuing the width and height in pixels


# load the elevation data from 2017-2023, 
# select the variables year, distance and elevation_m, 
# and  add  the elevation_m variable to the vdat3 vegetation data of 2017-2020

# join with the vegetation data


# plot the change in cover along the elevation  gradient 
#and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species
