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
p0<-ggplot(data, aes(x = Treatment, y = Value)) +
  geom_jitter(width=0.05,aes(color=Block)) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)")
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

# fit and show a linear model with treatment, block, and their interaction
# this we call a "random slopes model": 
# each level of the random factor (Block) has its own intercept and its own slope

newdat$Pred_m2 <- predict(m2, newdata = newdat)
p2<-ggplot(data, aes(x = Treatment, y = Value)) +
  geom_jitter(width=0.1,aes(color=Block)) +
  geom_smooth(data=newdat,aes(x=Treatment, y=Pred_m2, color=Block), 
              method="lm", formula="y~x", fill=NA) +
  labs(x="nitrogen addition (kg/ha)", y="biomass (g/m2)",
       title="random slopes")

# show both plots
p1/p2

# test which model is better (when AIC value is 2 units lower)
