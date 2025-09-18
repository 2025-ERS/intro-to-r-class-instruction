# --------------------------HEADER ----------------
# title: 11-multivariate-analysis.R
# author: Han Olff
# date: 2025-9-16
# description: Multivariate analysis: DCA, CCA, PCA
# input databases: VegetationDB plus other databases with environmental data

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")
gsheets_auth()
library(vegan)
library(psych)

#--------------------------02 Read and combine the datasets -----

# 1) read and filter the vegetation data for 2025
# browseURL("https://docs.google.com/spreadsheets/d/1WIzkUvunLk1buybZ04Oko8wnF65EbuXjWEviWbeAwu4")
VegetationDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1WIzkUvunLk1buybZ04Oko8wnF65EbuXjWEviWbeAwu4",
                        sheet="FactVegCov")
VegetationDB$FactVegCov2025<-VegetationDB$FactVegCov |>
  # filter for only the only 2023 data and plots with vegetation
  filter(Year==2025 & TransectPoint_ID<=1150) |>     
  # select  only distance_m and the species names as variable to use
  dplyr::select(-c(Year,Bare,Litter,Mosses,SalicSpp)) |>
  # convert distance_m to the row names of the tibble (required by vegan)
  tibble::column_to_rownames(var="TransectPoint_ID") %>%  
  # %>% needed because of  . representing the input data in next line
  # remove species not found anywhere in this year
  dplyr::select(which(colSums(.) != 0))  
VegetationDB$FactVegCov2025 |> as_tibble()


# 2) read and filter the macrotransect elevation data for 2025
MacrotransectDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1UmpZoNIjo5lXdcpGKuBHe7wFX1Z3AQioeHjzrFgYnxo",
                           sheet=c("FactElevation","FactDist2Gully"))
MacrotransectDB$FactElevation2025 <- MacrotransectDB$FactElevation |>
  dplyr::filter(Year==2025 & !is.na(TransectPoint_ID) & TransectPoint_ID<=1150) |>
  dplyr::select(TransectPoint_ID,Elevation_m) |>   # select  only distance_m and elevation 
  dplyr::mutate(Elevation_m=round(Elevation_m,2))
MacrotransectDB$FactElevation2025

MacrotransectDB$FactDist2Gully2025 <- MacrotransectDB$FactDist2Gully |>
  dplyr::filter(Year==2025  & TransectPoint_ID<=1150) |>
  dplyr::select(TransectPoint_ID,Dist2Gully_m)   
print(MacrotransectDB$FactDist2Gully2025,n=25)


# 3) read the transgression probability (proportion of the time of  the growing season flooded) of different elevations (per cm)
# from 1 april - 30 aug
SchierTideDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1DOzvscotzWXm5MmEFrZhvPY820weEZVcSfUYz_5Gyf4",
                        sheets="FactTransProb") 
SchierTideDB$FactTransProb2025<-SchierTideDB$FactTransProb |>
  dplyr::filter(Year==2025) |>
  dplyr::select(Elevation_m,TransProb)
SchierTideDB$FactTransProb2025


# 4) read the clay thickness and redox potential from the soil  database
SoilDB<-read_gsdb("https://docs.google.com/spreadsheets/d/1oHYCBlLR47Pxov5iF8PS_71lIRJwxgUfAP_t8JumJ7o",
                  sheet=c("FactProfile","FactRedox"))
SoilDB$FactProfile2025<-SoilDB$FactProfile |>
  dplyr::filter(Year==2025 & SoilType_ID %in% c("clay","clay-organic") & TransectPoint_ID<=1150) |>
  dplyr::select(TransectPoint_ID,corrected_depth) |>     
  group_by(TransectPoint_ID) |> 
  dplyr::summarize(Clay_cm=mean(corrected_depth,na.rm=T)) #calculate average clay layer thickness  for each pole
SoilDB$FactProfile2025


SoilDB$FactRedox2025<-SoilDB$FactRedox |>
  dplyr::filter(Year==2025,TransectPoint_ID<=1150) |>
  dplyr::group_by(TransectPoint_ID,ProbeDepth) |>
  dplyr::summarize(redox_mV=mean(redox_raw2_mV,na.rm=T)) |>
  tidyr::pivot_wider(id_cols=TransectPoint_ID,
                     names_from=ProbeDepth,
                     names_prefix = "Redox",
                     values_from = redox_mV)
SoilDB$FactRedox2025

# 5) read the distance to gulley




##### Merge all previous 2025 datafiles into a single file EnvDat
# a sequential join operation in a pipe
# set distance_m as the row names of the  tibble
# set the NAs for floodprop equal to zero
# set the NAs for clay thickness equal to zero
# round floodprob to 2 decimals

EnvDat <- 
  MacrotransectDB$FactElevation2025 |>
  dplyr::left_join(MacrotransectDB$FactDist2Gully2025, by="TransectPoint_ID") |>
  dplyr::left_join(SchierTideDB$FactTransProb2025, by="Elevation_m") |>
  dplyr::left_join(SoilDB$FactProfile2025, by="TransectPoint_ID") |>
  dplyr::left_join(SoilDB$FactRedox2025, by="TransectPoint_ID") |>
  dplyr::mutate(TransProb=ifelse(is.na(TransProb),0,TransProb),
                TransProb=round(TransProb,3),
                Clay_cm=ifelse(is.na(Clay_cm),0,Clay_cm)) |>
  # convert to dataframe with row names required by vegan
  tibble::column_to_rownames(var="TransectPoint_ID")  
print(EnvDat)  

# so for ordination with functions from the vegan library you need an separate environmental factors dataset
# and a species composition dataset with the same rownames, indicating  the same sites / samples
# the species data (community composition) need to be in wide format, so we produced this
vegdat<-VegetationDB$FactVegCov2025
envdat<-EnvDat

##### explore the correlations among the environmental factors in a panel pairs plot
# using the pearson correlation coefficient
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="pearson")
# using the spearman rank-correlation coefficient
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="spearman")
# note that the units are very different! 

##### Ordination: run a Principal Component Analysis (PCA) on the environmental data
# .scale=T means: use correlations instead of covariances
# use .scale=T for datasets where the variables are measured in different use

# do a principal component analysis (pca) 
pca_env<-prcomp(envdat,center=T,scale=T)
pca_env
summary(pca_env)
# show the site scores for axis 1
pca_env$x

# the PCs are reduced dimensions of the dataset
# you reduce 6 variables to 2 dimensions
# make a biplot (variable scores plus sample score) the pca ordination
# and label the axis with the explained variation
biplot(pca_env,xlab="PC1 49%",ylab="PC2 21%")


##### ordination: calculate and plot a Non-metric Multidimensional Scaling (NMDS) ordination
# explore the distance (dissimilarity) in species composition between plots
vegdat
d1<-vegan::vegdist(vegdat,method="euclidean") # Euclidean dissimilarity
d1
d2<-vegan::vegdist(vegdat,method="bray") # Bray-Curtis dissimilarity
d2


# non-metric multidimension scaling / indirect gradient analysis (only species composition)
nmds_veg<-metaMDS(vegdat,k=2,trace=F,trymax=1000,distance="bray")
nmds_veg
vegan::ordiplot(nmds_veg,type="t")
# and show the ordination with the most abundance species with priority
SpecTotCov<-colSums(vegdat)
vegan::ordiplot(nmds_veg,display="sites",cex=1,type="t")
vegan::orditorp(nmds_veg,dis="sp",priority = SpecTotCov,
                col="red",pcol = "red",pch="+",cex=1.1)

#### ordination: compare to a DCA -> decide what ordination we should do, linear or unimodal? 
# how long are the gradients? Should I use linear (PCA)or unimodal method (NMDS, DCA)
dca<-vegan::decorana(vegdat)
dca
# first axis is ~10 standard deviations of species responses
# result: length of first ordination axis is >8 standard deviations
# only when <1.5 you can use a PCA or RDA
# plot the dca results as a biplot
vegan::ordiplot(dca,display="sites",cex=0.7,type="text",xlim=c(-5,5))
vegan::orditorp(dca,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=0.8,xlim=c(-5,5))
##### fit the environmental factors to the dca ordination surface
names(envdat)
ef_dca<-vegan::envfit(dca~Clay_cm+Elevation_m+Dist2Gully_m+TransProb+Redox5+Redox10,
                      data=envdat,na.rm=T)
#add the result to the ordination plot as vectors for each variable

plot(ef_dca,add=T)
##### add contour surfaces to the dca ordination for the relevant abiotic variables
vegan::ordisurf(dca,envdat$Dist2Gully_m,add=T,col="blue")
vegan::ordisurf(dca,envdat$TransProb,add=T,col="red")

# make the plot again with contours of abundance of Plantago maritima
vegan::ordiplot(dca,display="sites",cex=0.7,type="text",xlim=c(-5,5))
vegan::orditorp(dca,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=0.8,xlim=c(-5,5))
vegan::ordisurf(dca,vegdat$AtripPor,add=T,col="red")

##### make the same plot but using a nmds
##### fit the environmental factors to the nmds ordination surface
vegan::ordiplot(nmds_veg,display="sites",cex=1,type="t",xlim=c(-4,4))
vegan::orditorp(nmds_veg,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=0.9,xlim=c(-4,4))
##### fit the environmental factors to the dca ordination surface
ef_nmds<-vegan::envfit(nmds_veg~Clay_cm+Elevation_m+Dist2Gully_m+TransProb+Redox5+Redox10,
                       data=envdat,na.rm=T)
#add the result to the ordination plot as vectors for each variable
plot(ef_nmds,add=T)
##### add contour surfaces to the nmds ordination for the relevant abiotic variables
vegan::ordisurf(nmds_veg,envdat$Dist2Gully_m,add=T,col="blue")
vegan::ordisurf(nmds_veg,envdat$TransProb,add=T,col="red")


##### compare an unconstrainted (DCA) and constrained (CCA) ordination
# did you miss important environmental factors?
# the eigenvalues represent the variation explained by each axis

# run a cca - the constrained version (axes restricted by environmental factors)


# Test the whole model


# Test axes


# Test terms (environmental variables)


# kick out variables that are not significant - simplify the model



# Test the whole model

# Test axes

# Test terms (environmental variables)


# Plot the ordination 


# show the species


### draw the environmental factor arrows
# extract biplot scores of the constrained variables
bp <- scores(cca2, display = "bp", scaling = 2)
# scale arrows to fit the plotting region (same trick envfit uses)
mul <- vegan::ordiArrowMul(bp)
# draw arrows + labels
arrows(0, 0, bp[,1] * mul, bp[,2] * mul, length = 0.08, col = "blue")
text(bp[,1] * mul, bp[,2] * mul, labels = rownames(bp), col = "blue", pos = 4, xpd = NA)

# add  environmental factor contours to the cca ordination plot
vegan::ordisurf(cca2,envdat$TransProb,add=T,col="green")
vegan::ordisurf(cca2,envdat$Clay_cm,add=T,col="brown")


# You have measured the right things that matter for the vegetation composition!


##### --------------------cluster analysis (classification) of  communities

# first calculate a dissimilarity matrix, using Bray-Curtis dissimilarity index
#  and show the dissimilarity matrix (1= completely different, 0= exactly the same)


# now cluster the sites based on similarity in species composition 
# using average linkage as the sorting algorithm


# show the dendrogram and cut it in 5 communities


# do indicator species analysis - which species characterize each cluster? 




##### add the clustering of plots to your dca ordination



#add the vegetation type to the environmental data

# name the different communities that you identified


# show boxplot of elevation differences between vegetation types

# show boxplot of flooding probability differences between vegetation types

# show boxplot of clay thickness differences between vegetation types

# show boxplot of distance to gully  between vegetation types

# put all plots together in a panel figure with patchwork

