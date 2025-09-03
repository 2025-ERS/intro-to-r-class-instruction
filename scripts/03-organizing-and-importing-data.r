# --------------------------HEADER ----------------
# title: "03-organizing and importing data.R"
# author: "Han Olff"
# date: "2025-8-22"
# description: "How to import data into R from different sources, including local files and online databases."
# input: "No input files, but the script reads data from an online Google Sheets database"
# output: "No output files, but the script produces some plots"


# --------------------------01 Set up the environment ----------------
# This script uses the renv package to manage the R environment and the tidyverse package for data manipulation and visualization.
# Make sure that you have the renv package installed. If not, install it using install.packages("renv")
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
# Load the renv package
library(renv)
# restore the library versions from the lock file
renv::restore() 
# Load the libraries that you will use in this script. Only load the libraries that you will actually use!
library(tidyverse) # load the tidyverse libraries, including readr and ggplot2


# --------------------------02 Different ways to enter data ----------------
# Data can be imported in different ways in R:\
# 1. entering data directly in your script\
# 2. reading data from a .csv or .xlsx file from your local computer or online source\
# 3. reading data from a published csv link in a Google Sheets database\
#  We will mostly use the last method during this course. When reading data, we prefer the readr::read_csv()
# function, as it reads data directly into a tibble instead of a dataframe. A tibble is more compact when printed # and shows the variable types directly.


# --------------------------03 Method 1 - enter data directly in your script  ----------------
# This is only practical for small datasets, but not for large datasets.
x<-c(1,2,3,4)
y<-as.integer(c(1,4,3,5))
z<-c("A","B","C","D")
# combine the two vectors into a data frame using data.frame
data1a<-data.frame(x,y,z)
print(data1a)
# combine the two vectors into a tibble using tibble, note the subblte different, variables types are shown, and 
data1b<-dplyr::tibble(x,y,z)
print(data1b)


# --------------------------04 Method 2 - read data from a local file ----------------
# If you want to read a file from a drive from your computer, 
# it is a good idea to set a working directory to point where your different datasets are located
setwd("C:/Users/holff/data") # note that the slashes have to be forward, not backward
data2<-readr::read_csv("EVI2001_2023.csv")
print(data2)
# This reads the file EVI2001_2023.csv from the folder data on my C drive.
# The problem however, is that this script likely does not work on your computer. 
# This is because you do not have this file and folder on your local computer. 
# I can of course send you the data, but then if I change the I should send them again. 
# But this leads to different versions of the same file to exist in multiple places, which is not good data
# management. You will not be shure what the most up-to-dat version of the file is. 
# So in collaborative projects it is much better to read data from an online data source, 
# making sure that there is only one file that all the collaborators read to get the data.\


# --------------------------05 Method 3 - read data from an online database 
# This is the hihgly recommended choice of these three methods. 
# in this case the data are in one online database, in our course mostly a Google Sheets database.
# This means that there is only one version of the data, and all collaborators read the same data.
# This is the preferred method for collaborative projects, and also for your own projects.
# Google Sheets is a Spreadsheet app, similar to Microsoft Excel. But Google Sheets files "live" in the cloud
# associated with your Google account. A spreadsheet app is formally not a database app.
# But a Google Sheets document can be set up quite well as a 'lightweight' relational database. 
# When done well according to a couple of clear principles (as described here),
# A handy feature is that Google Sheets can  be set up to "publish" online tables (=sheets) as a .CSV (Comma
# Separated Values ascii text) file, that can be directly read into R. 
# For this use in Google Sheets menu File/ Share / Publish to web, choose the sheet to publish and as type
# choose Comma-separated values. This produces a link, that you put in your R script to read that datafile.
# This comes with large benefits of managing the data in one place in a relational database. 
# It is best practice to give the dataframe that you read in R the same name as the online table (sheet).
# In this way, data can be read on any computer, and multiple computers can read the same datafile.
# That is what you want in a collaborative project.

# --------------------------06 Introduction of the example  online database ----------------
# We explore this  with this example database  (explore by copy-paste the link below in your browser):
# https://docs.google.com/spreadsheets/d/1m-liu8omZMewqz_YP9j_YUmQ0zwATl3z4aRLnZFnfWc/edit?usp=sharing

# This is a Google Sheets database that only exists in one location in the cloud.
# It is a relational database, with multiple tables (sheets) that are linked to each other.
# The database is set up as a Star Schema, which is a particular way of organising data in a relational database.
# The database contains data on a study on the effects of wildlife on vegetation in East Africa.
# The database contains the following tables (sheets):
# - MetStudyInfo: short descriptions of the key features of your study
# - MetStill2Do: list of what you still need to do, your ToDo list for the database
# - MetTables: the list of tables (sheets) in your database, with for each table a short contents description
# - DimTransect: the list of transects in the study, with their properties
# - DimPlot: the list of plots in the study, with their properties
# - DimSpecies: the list of species in the study, with their properties
# - DimSection: the list of sections in the study, with their properties
# - FactSectionAnimals: the data on the number of animals observed in each section of each transect
# - FactSectionVegetation: the data on the vegetation in each section of each transect
# - FactPlotVegetation: the data on the vegetation in each plot of each transect

# --------------------------07 Star schema database organization ----------------
# This example database is organized as a Star Schema database, which is explained in detail in [this document](https://docs.google.com/document/d/1UbUMVFfF4muRqt_YOT73NT7xt-vsoLHs0xNza_Le56U/edit?usp=sharing).
# This is a particular type of organisation of data into a relational database using Dim and Fact tables.
# In addition I also recommend Met tables. These **Met tables** contain documents, 'data about data'.
# Dim tables: contain information on lists of objects and subjects that you study with their properties
# Fact tables: are the data that you collect on the objects or subjects listed in your Dim tables.
# The Dim tables are the points of the star, and the Fact tables are at the center of the star.
# The Dim tables are linked to the Fact tables using the unique ID variable in each Dim table.

# --------------------------08 MET tables: metadata ----------------
# Met tables are a special type of table that contain meta-data, which is data about the data in your database.
# Met tables are not part of the Star Schema, but they are highly recommended to use in your database.
# Met tables are used to document the study design, the variables, and the data collection process.
# They are also used to keep track of what still needs to be done in the database, such as entering data, checking data, etc.
# Met tables are typically filled in at the start of the project, and updated during the project.
# The following Met tables are recommended to use in your database: 
# - MetStudyInfo: short descriptions of the key features of your study, such as primary investigator, starting date.
# - MetStill2Do: list of what you still need to do, your ToDo list for the database, such as entering data, checking something still in your field book, etc. In this table, keep track of who will do it of your team, in which table, for which variable and put a date when it is complete.
# - MetTables: the list of tables (sheets) in your database, with for each table a short contents description and a CSV link to read the table in an R script. This link for each table you produce from the menu File /Share /Publish to web and then selecting the table name (instead of entire document) and Comma separated values (instead of Web page). This then shows the link. If you put this link in your browser, it gives a download as a csv file. But you can also use this link directly in R to read the data using readr::read_csv(link). So this avoids the use of intermediary data files. Anyone with a script containing that link can read the data. Because such links are impossible to guess this is still sufficiently safe for regular ecological data. If you however want additional security, you can also set up access to tables using the google_drive package in R, allowing user authentication.
# - MetVariables: the list of variables in your database, with for each variable a short description, the unit, the type of variable (numeric, character, factor), and the Dim table it is linked to. This is useful to document the variables in your database. 

# --------------------------09 DIM tables: dimensions ----------------
# Dim tables contain information on lists of objects and subjects that you study with their properties.
# Dim tables are the points of the star, and the Fact tables are at the center of the star.
# The Dim tables are linked to the Fact tables using the unique ID variable in each Dim table.
# In this example database, the following Dim tables are used:
# - DimTransect: the list of transects in the study, with their properties such as length, habitat type, etc.
# - DimPlot: the list of plots in the study, with their properties such as size, habitat type, etc.
# - DimSpecies: the list of species in the study, with their properties such as common name, scientific name, etc.
# - DimSection: the list of sections in the study, with their properties such as length, habitat type, etc.
# Each Dim table has a unique ID variable that is used to link to the Fact tables.
# For example, the DimSpecies table has a unique ID variable SpeciesID that is used to link to the FactSectionAnimals table.
# --------------------------10 FACT tables: data ----------------
# Fact tables are the data that you collect on the objects or subjects listed in your Dim tables.
# Fact tables are at the center of the star, and are linked to the Dim tables using the unique ID variable in each Dim table.
# In this example database, the following Fact tables are used:
# - FactSectionAnimals: the data on the number of animals observed in each section of each transect, with the SpeciesID linking to the DimSpecies table and the SectionID linking to the DimSection table.
# - FactSectionVegetation: the data on the vegetation in each section of each transect, with the SectionID linking to the DimSection table.
# - FactPlotVegetation: the data on the vegetation in each plot of each transect, with the PlotID linking to the DimPlot table.
# Each Fact table has several ID variables that  to link to the Dim tables, characterising the observation 
# as which species is observed, at which transect, etc

# --------------------------11 Example of reading data from a Google Sheets database ----------------
# We will now read in one of the Fact tables from the example database

# first read MetTables as this contains the links to read the tables
# define the link to read the FactSectionAnimals table
MetTables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vTtMDeI9k2M2xsukcZm-kE_gbYi3kYJDfbp1vEGKtpajM84GSFYrpd2U0P3kqgnbLQUKhhHciDZpV-j/pub?gid=1387882554&single=true&output=csv"
MetTables<-readr::read_csv(MetTables_link)
MetTables

# Now we can read the FactSectionAnimals table using the link in the MetTables table
FactSectionAnimals_link<-MetTables |>
  dplyr::filter(data_table=="FactSectionAnimals") |>
  dplyr::pull(CSV_link)
FactSectionAnimals_link
# read the link into R as a tibble
FactSectionAnimals<-readr::read_csv(FactSectionAnimals_link)
FactSectionAnimals
# this uses the function read_csv() from the readr package, which is part of the tidyverse
# This loads the dataset directly from Google Sheets into R, there are not intermediate files.
# If they data change in Google Sheets, then the new version will be loaded when the read_csv() 
# function is run again 
# check the result in the 'environment' tab in R Studio topright in your screen
# or inspect the data by printing the first few lines
print(FactSectionAnimals)
names(FactSectionAnimals)

# --------------------------12 Example of plotting data ----------------
ggplot2::ggplot(data=FactSectionAnimals, 
                mapping=aes(x=SpCode2,y=CountLeft)) +
  geom_boxplot()

ggplot2::ggplot() +
  geom_boxplot(data=FactSectionAnimals, 
               mapping=aes(x=SpCode2,y=CountLeft))
# This makes a boxplot of the number of animals observed (CountLeft) for each species (Spcode6)
# You can see that the species codes are not very informative.
# We can link the species codes to the DimSpecies table to get the full species names

# read in the DimSpecies table from MetTables
DimSpecies_link<-MetTables |>
  dplyr::filter(data_table=="DimSpecies") |>
  dplyr::pull(CSV_link)
# read the DimSpecies table into R
DimSpecies<-readr::read_csv(DimSpecies_link)
print(DimSpecies)

# join the two tables using a left_join with SpCode2 as the key variable linking the tables 
# always put the table with the most rows first, so that you do not lose any rows
FactSectionAnimals2<-dplyr::left_join(FactSectionAnimals,DimSpecies,by=c("SpCode2"="SpCode2"))
# check the 'environment' tab in R Studio topright in your screen that you are adding variables not rows!
# inspect the data by printing the first few lines
print(FactSectionAnimals2)
# now make the boxplot again, but now with the full species names
ggplot2::ggplot(data=FactSectionAnimals2, 
                mapping=aes(x=Name_eng,y=CountLeft)) +
  geom_boxplot() +
  labs(x="Species",y="Transect count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This makes a boxplot of the number of animals observed (CountLeft) for each species (CommonName)