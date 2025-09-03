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
names(vdat)

# show in which unique years  data were recorded
unique(vdat$year)

# reshape the data using tidyr piping from wide to long format, where species is a single variable instead of distributed over multiple columns, treat bare, litter and moss as if they are species (see the "data import cheatsheet")
# remove Salicornia.europaea and Salicornia.procumbens from the dataset
# as Salicornia.sp is their sum (the 2 species where not separated in earlier years)
# also remove the variables bare,litter,mosses 
vdat1<-vdat %>%
  tidyr::pivot_longer(-c(year,TransectPoint_ID), # which variables to not include in the wide-long pivot
                      names_to="Species_ID",  # what is the name of the new variable to which variables are collapsed
                      values_to="cover") %>%  # what do the cells in the original table represent
  filter(!Species_ID %in% c("bare","litter","mosses","SalicEur","SalicPro"))

#show the names of all the species in the dataset
vdat1
unique(vdat1$Species_ID)

# find the most abundant species in the dataset
# add a variable to the dataset that is the rank number of the species 
# according to summed abundance of each species over
# the whole dataset (1=most abundant species)
vdat2<-vdat1 %>%
  group_by(Species_ID) %>%
  summarise(sumcov=sum(cover,na.rm = T)) %>%
  mutate(rank=rank(-sumcov)) %>%
  arrange(rank)
vdat2
vdat1
# merge the two files
vdat3<-left_join(vdat1,vdat2,by="Species_ID")
vdat3
### plot the 5 most dominant species as a line diagram, cover (y) versus distance_m (x)with ggplot, separate plot for each year, each species with a different line color

# plot the change in cover along the distance  transect 
# and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species
vdat3 |> filter(rank<=10) |>
  ggplot(aes(x=as.factor(TransectPoint_ID),y=as.factor(-year),fill=cover)) +
  geom_tile() +
  scale_fill_gradient(low="yellow",high="red") +
  labs(x="transect point", y="year",fill="cover") +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, x, "")
                   ) +
  scale_y_discrete(labels = function(x) ifelse(seq_along(x) %% 3 == 0, x, "")
                   ) +
  theme( text = element_text(size = 8),    
         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)
         ) +
  facet_wrap(~Species_ID,ncol=2) 
# save the file to a png file of  1920 x 1080 pixels (first make a folder figures)
# specifuing the width and height in pixels
ggsave("figures/heatmap_10_most_abundant_species.png",width=1920/300,height=1080/300,dpi=300, units="in")


# load the elevation data from 2017-2023, 
# select the variables year, distance and elevation_m, 
# and  add  the elevation_m variable to the vdat3 vegetation data of 2017-2020
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::select(year,TransectPoint_ID,elevation_m) %>%
  dplyr::filter(!is.na(TransectPoint_ID))
elevdat
# join with the vegetation data
vdat4<- vdat3 %>% dplyr::filter(year>=2017) # make new file with filtered vegetation data only year >= 2017
names(vdat4)
vdat5<- dplyr::left_join(vdat4,elevdat,by=c("year","TransectPoint_ID")) # join the result with the elevation data
vdat5

hist(vdat5$elevation_m) # 13 classes 

# plot the change in cover along the elevation  gradient 
#and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species
vdat5 %>% filter(rank<=10) %>%
  ggplot(aes(x=cut(elevation_m,13),y=as.factor(-year),fill=cover)) +
  geom_tile() +
  scale_fill_gradient(low="yellow",high="red") +
  facet_wrap(~Species_ID,ncol=2)

cut(vdat5$elevation_m,13)

