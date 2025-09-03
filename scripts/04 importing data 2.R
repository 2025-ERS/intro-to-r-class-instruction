# --------------------------HEADER ----------------
# title: "04-organizing data 2.R"
# author: "Han Olff"
# date: "2025-8-22"
# description: "How to import data into R from different sources, including local files and online databases using the bird example database"
# input: "No input files, but the script reads data from an online Google Sheets database"
# output: "No output files, but the script produces some plots"

# --------------------------01 Set up the environment ----
rm(list=ls()) # clear working memory
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
library(renv) # load renv library
renv::restore() # restore the packages in the renv.lock file
library(tidyverse) # load the tidyverse package

#--------------------------02 Read and combine the data -----
# first read MetTables as this contains the links to read the tables
# define the link to read the FactSectionAnimals table
MetTables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vTis2qLWuvgnoNPejiDi0k7T2yq9qszY_JmnwyjeSKFCrWQSOSM_QsJSYs0-RndDBt1W1YXNH-WVsfe/pub?gid=1387882554&single=true&output=csv"
MetTables<-readr::read_csv(MetTables_link)
MetTables

# Now we can read the FactPeckObs table using the link in the MetTables table
FactPeckObs_link<-MetTables |>
  dplyr::filter(data_table=="FactPeckObs") |>
  dplyr::pull(CSV_link)
FactPeckObs_link
# read the link into R as a tibble
FactPeckObs<-readr::read_csv(FactPeckObs_link)
FactPeckObs

# read in the DimSpecies table from MetTables
DimSpecies_link<-MetTables |>
  dplyr::filter(data_table=="DimSpecies") |>
  dplyr::pull(CSV_link)
# read the DimSpecies table into R
DimSpecies<-readr::read_csv(DimSpecies_link)
print(DimSpecies)

# read in the DimSections table from MetTables
DimSections_link<-MetTables |>
  dplyr::filter(data_table=="DimSections") |>
  dplyr::pull(CSV_link)
# read the DimSpecies table into R
DimSections<-readr::read_csv(DimSections_link)
print(DimSections)

# read in the DimDates table from MetTables
DimDates_link<-MetTables |>
  dplyr::filter(data_table=="DimDates") |>
  dplyr::pull(CSV_link)
# read the DimSpecies table into R
DimDates<-readr::read_csv(DimDates_link)
print(DimDates)


# join all tables using a left_join with SpCode2 as the key variable linking the tables 
# always put the table with the most rows first, so that you do not lose any rows
AllData<-dplyr::left_join(FactPeckObs,DimSpecies,by="Species_ID") |>
         dplyr::left_join(DimSections, by="Section_ID") |>
         dplyr::left_join(DimDates,by="Date_ID") 

names(AllData)


#--------------------------03 Plot the results  -----

ggplot2::ggplot(data=AllData, 
                mapping=aes(x=SpeciesEngName,y=PeckRateMin,
                            fill=Section_ID, color=Section_ID)) +
  geom_boxplot() +
  labs(x="Species",y="Pecking rate per minute") +
  facet_wrap(~Section_ID,nrow=2)
