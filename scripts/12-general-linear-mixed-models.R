# --------------------------HEADER ----------------
# title: 12-linear-mixed-models.R
# author: Han Olff
# date: 2025-9-21
# description: linear mixed models with lmer
# input databases: simulated data

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")
gsheets_auth()
library(glmmTMB)
library(car)

#--------------------------02 Simulate a dataset -----
# Define the effect size for the treatment as a continuous factor
treatment_effect1 <- c(5, 10, 15, 20)
treatment_effect2<- c(5, 8, 12, 15)
treatment_effect3<- c(20, 25, 27, 30)
# Define the means for the three blocks
block_means <- c(5, 10,15)
# Define the number of observations per block
n <- 10
# Initialize an empty data frame to store the results
data <- data.frame()
# Loop over each block and each treatment level to generate the data
for (block in 1:3) {
  for (treatment in 1:4) {
    # Calculate the mean for this block-treatment combination, only for block 1 and 2
    if(block==1) {mean_value <- treatment * treatment_effect1[treatment] + block_means[block]}
    else if(block==2) {mean_value <- treatment * treatment_effect2[treatment] + block_means[block]}
    else  {mean_value <- treatment * treatment_effect3[treatment] + block_means[block]}    
    # Generate random values for each treatment level within the block
    values <- rnorm(n, mean = mean_value, sd = 5)
    # Create a temporary data frame for this block-treatment combination
    temp_data <- data.frame(
      Block = paste("Block", block, sep = ""),
      Treatment = treatment,  # Treating treatment as a continuous variable
      Value = values
    )
    # Combine with the main data frame
    data <- rbind(data, temp_data) |> as_tibble()
  }
}

# plot the data
p0<-ggplot(data, aes(x = Treatment, y = Value, color=Block)) +
  geom_jitter(width=0.05) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)",
       title="fixed slopes")
p0

# fit and show a linear model with treatment and block, but no interaction
# this we call a "random intercepts, fixed slopes model": 
# each block has its own intercept but the same slope

# fit predictions and show the observations and predictions
newdat <- expand.grid(Treatment = seq(1:4),
                      Block = c("Block1","Block2","Block3"))
newdat$Pred_m1 <- predict(m1, newdata = newdat)
p1<-ggplot(data, aes(x = Treatment, y = Value)) +
  geom_jitter(width=0.1,aes(color=Block)) +
  geom_smooth(data=newdat,aes(x=Treatment, y=Pred_m1, color=Block), 
              method="lm", formula="y~x", fill=NA) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)",
       title="fixed slopes")
p1
# fit and show a linear model with treatment, block, and their interaction
# this we call a "random slopes model": 
# each level of the random factor (Block) has its own intercept and its own slope
# while estimates of slopes and intercepts are not expected to be correlated


newdat$Pred_m2 <- predict(m2, newdata = newdat)
p2<-ggplot(data, aes(x = Treatment, y = Value)) +
  geom_jitter(width=0.1,aes(color=Block)) +
  geom_smooth(data=newdat,aes(x=Treatment, y=Pred_m2, color=Block), 
              method="lm", formula="y~x", fill=NA) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)",
       title="random slopes")
p2
AIC(m2,m1)

# also include estimates for the covariance between intercepts and slopes
# -> groups with higher intercept also have higher slope

newdat$Pred_m3 <- predict(m3, newdata = newdat)
p3<-ggplot(data, aes(x = Treatment, y = Value)) +
  geom_jitter(width=0.1,aes(color=Block)) +
  geom_smooth(data=newdat,aes(x=Treatment, y=Pred_m3, color=Block), 
              method="lm", formula="y~x", fill=NA) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)",
       title="random slopes")
# test which model is better (when AIC value is 2 units lower)
AIC(m3,m1)


p1/p3


#--------------------Nested variables -------------------------------
data(Salamanders, package = "glmmTMB")
# for dataset / variables description see 
# browseURL("https://rdrr.io/cran/glmmTMB/man/Salamanders.html")
# for more details see the whole  paper  
# browseURL("https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.12585") 
unique(Salamanders$spp)
dat<-Salamanders |> 
  dplyr::filter(spp=="DM") |> # adult Desmognathus monticola
  mutate(sample=factor(sample)) |> 
  as_tibble() 
dat
# explore design
table(dat$site,dat$mined)
table(dat$site,dat$sample)
# explore results
ggplot(data=dat, aes(x=mined,y=count, color=sample)) +
  geom_boxplot()
ggplot(data=dat, aes(x=site,y=count)) +
  geom_boxplot(aes(fill=site))
ggplot(data=dat, aes(x=cover,y=count, color=site)) +
  geom_point(aes(shape=mined)) +
  geom_smooth(method="lm")

# test for effect of cover

# add the effect of mining


# account that samples were taking at different sites and sampling dates
# different sample were taken on the same sites, so repeated measurements

