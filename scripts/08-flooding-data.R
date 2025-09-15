# --------------------------HEADER ----------------
# title: 09-flooding-data.R
# author: Han Olff"
# date: 2025-9-14
# description: Calculate flooding probability at 1 cm resolution from the Schiermonnikoog ferry pier tidal gauge data
# description: data were collected at 10 min intervals 1 Jan 202
# input database: SchierTideDB 
# browseURL("https://docs.google.com/spreadsheets/d/1DOzvscotzWXm5MmEFrZhvPY820weEZVcSfUYz_5Gyf4")
# WARNING: this is a very large database (~ 2 million records)
# this script requires sufficient ram (at least 16 Gb) and good processor (>i7) to run


#--------------------------01 Set up the environment ----
source("scripts/00-helpers.R")


#--------------------------02 Read the datasets -----
# Schiermonnikoog tidal data 1980-2025
# calculate flooding probability from the Rijkswaterstaat tidal gauge data at the Schiermonnikoog pier

# set your local working directory
# setwd("G:/My Drive/_Database/WADZ/ERS_transect_database/tide")
# read data (be patient for the file to read, file is ~ 2 million records)
# for working with dates and times see https://r4ds.had.co.nz/dates-and-times.html

# Read Database ERS-Schier_tides (! takes a more than 10 minutes)
SchierTideDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1DOzvscotzWXm5MmEFrZhvPY820weEZVcSfUYz_5Gyf4")  
SchierTideDB$MetTables
# re-format the data for further use
SchierTideDB$FactSealevel2<-SchierTideDB$FactSeaLevel |>
  dplyr::mutate(Sealevel_cm=ifelse(Sealevel_cm>1000,NA,Sealevel_cm), # set missing values to na
                Sealevel_m=Sealevel_cm/100, # cm to m + NAP
Total                Year=lubridate::year(DateTime),
                Month=lubridate::month(DateTime)
  )
SchierTideDB$FactSealevel2


# histogram of tidal levels for summer of each of the last five years
SchierTideDB$FactSealevel2 |>
  dplyr::filter(Month>=4 & Month<9 & Year %in% 2018:2025 & !is.na(Sealevel_m)) |>
  ggplot(aes(x=Sealevel_m))+   # histogram of tidal heights during this period
  geom_histogram(alpha=0.5,fill="blue",binwidth=0.2) +
  facet_wrap(~Year,nrow=2) +
  xlab("elevation (m to NAP)") +
  ylab("number of times tide at this level")


SchierTideDB$FloodProb<- SchierTideDB$FactSealevel2 |>      # calculate the april-august flooding frequency of each tidal elevation
  dplyr::filter(Month>=4 & Month<8 & !is.na(Sealevel_m) & Year>=1980) |>
  group_by(Year, Sealevel_m) |>
  summarize(Tides_n=n()) %>%
  mutate(TransFreq=cumsum(Tides_n),
         Summer_n=max(TransFreq),
         TransProb=round((1-TransFreq/Summer_n),3))  # transgression probability of each elevation in a year
SchierTideDB$FloodProb

readr::write_csv(SchierTideDB$FloodProb,"out/transgres_freq.csv")  # this file goes into the SchierTideDB, needs updating every year

# explore sea level rise
tdata.trf %>%  # flooding probability at a particular elevation
  dplyr::filter(sealevel==-0) %>%
  ggplot(aes(x=year,y=transprob) +
  geom_point(size=3) +
  geom_smooth() +
  xlab("elevation (m to NAP") +
  ylab("transgression probability of elevation") +
  ggtitle("summer flooding probability at -1 m to NAP")

tdata.trf %>% # plot the summer flooding frequency across the elevational gradient
  ggplot(aes(x=sealevel,y=transprob,col=factor(year))) +
  geom_line(size=1) +
  xlab("elevation (m to NAP") +
  ylab("flooding probability of elevation")

tdata.trf %>% # plot the summer flooding frequency across the elevational gradient
  filter(year %in% 2018:2022) %>%
  ggplot(aes(x=sealevel,y=transprob,col=factor(year))) +
  geom_line(size=1) +
  xlab("elevation (m to NAP") +
  ylab("flooding probability of elevation")

