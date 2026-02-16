# --------------------------HEADER ----------------
# title: 12-linear-mixed-models.R
# author: Han Olff
# date: 2025-9-21
# description: linear mixed models with lmer
# input databases: simulated data

#--------------------------01 Set up the environment ----
source("scripts/00-setup.R")
gsheets_auth()
# load package lme4 and testing 
library(glmmTMB)
library(DHARMa)       # for testing assumptions of glmmTBD
library(car)         # for anova test of glmmTMB objects
library(emmeans)     # for tests of significance of mixed-effects models

#--------------------- 02 Read the data 
# Database: 
# browseURL("https://docs.google.com/spreadsheets/d/1pZzz337HcuQ28i7sIZu64JWAah9W77fI3hp0I9StZHA")

JK_vegetation<-read_gsdb("https://docs.google.com/spreadsheets/d/1pZzz337HcuQ28i7sIZu64JWAah9W77fI3hp0I9StZHA")
JK_vegetation$FactSpecRich$graztreat<-factor(JK_vegetation$FactSpecRich$graztreat,
                                             levels=c("vole","rabbit","cattle"))
levels(JK_vegetation$FactSpecRich$graztreat)<-c("vole","vole+rabbit","vole+rabbit+cow")

#-------------------- 03 Explore the results------------------------

# plot the results for species richness, ignoring the block effects
# so the observations in blocks are treated as replicates
ggplot(data=JK_vegetation$FactSpecRich,aes(x=graztreat,y=specrich,fill=graztreat)) +
  geom_boxplot()

hist(JK_vegetation$FactSpecRich$specrich)
# explore the distribution of the species richness data within each treatment
JK_vegetation$FactSpecRich |> 
  ggplot(aes(x=specrich)) +
  geom_histogram(binwidth = 3) +
  facet_grid(graztreat~.) +
  ylab("frequency") +
  xlab("plant species richness per 2m2") 

# test with  linear model assuming the residuals are normally distributed,
# and inspect the Q-Q plot of the residuals, and do a Shapiro test for normality 
# do a one-way anova ( 1 predictor with >2 categories)
m1<-lm(specrich~graztreat,
       data=JK_vegetation$FactSpecRich)
summary(m1)
# show the table of the results of the one-way analysis of variance
stats::anova(m1,test="F")

# test which differences between treatments are significant
emm <- emmeans::emmeans(m1, ~graztreat)
pairs(emm,adjust="tukey")
# letters: a a b
# means with the same letter are not significantly differently

# explore the q-q plot of the residuals of the model to check normality
plot(m1,which=2)

# plot the histogram of the residuals with normal curve
g=m1$residuals
m=mean(g)
std=sd(g)
hist(g,prob=F, main="histogram of residuals")
hist(g,prob=T, main="histogram of residuals")
curve(dnorm(x,mean=m,sd=std),
      col="blue", lwd=2,add=T,yaxt="n")
shapiro.test(g) # test if significant different from a normal distribution

#--------------------04 explore the results with a generalized linear mixed model
# do a glmmTMB assuming normally distributed residuals (so similar to lm)
m2<-glmmTMB::glmmTMB(specrich~graztreat,
                     family=gaussian(link="identity"),
                     data=JK_vegetation$FactSpecRich)
car::Anova(m2,test="Chisq")


# but the response variable is not continuous - only integers
# test with generalized linear model assuming now a poisson distribution of residuals 
# if plant species richness is different between the treatments 
m2<-glmmTMB::glmmTMB(specrich~graztreat,
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
car::Anova(m2,test="Chisq")

# compare the models with AIC test, when more then 2 lower, then better model
AIC(m2,m1)

# pairwaise comparisons, not that Tukey test is not valid for this model

emm <- emmeans::emmeans(m2, ~graztreat)
pairs(emm,adjust="tukey")

# explore the effect of blocks on the species richness
# plot cover mean species richness with the random factor
ggplot(aes(x=graztreat,y=specrich,fill=block),
       data=JK_vegetation$FactSpecRich) +
  geom_boxplot()
ggplot(aes(x=block,y=specrich,fill=graztreat),
       data=JK_vegetation$FactSpecRich) +
  geom_boxplot()


# test with standard linear model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming (incorrectly) that
# block is a fixed effect
# this is not correct because block is a random effect, requiring a mixed model 

m3<-glmmTMB::glmmTMB(specrich~graztreat + block,
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
car::Anova(m3,test="Chisq")

# test with  linear mixed model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a random effect
# first fit a model with only block, assuming poisson distribution
m4<-glmmTMB::glmmTMB(specrich ~ (1|block),
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
summary(m4)
car::Anova(m4,test="Chisq")

# random intercept, fixed slopes model, assuming the treatment effect is the same for every block
m5<-glmmTMB::glmmTMB(specrich ~ graztreat + (1|block),
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
summary(m5)
car::Anova(m5,test="Chisq")



# check if this model explains more variation, at least 2 units lower AIC
AIC(m5,m4)

# random intercept, random slopes model, assume the treatment effect is different per block
# so the slope of the treatment effect is allowed to vary per block

m6<-glmmTMB::glmmTMB(specrich ~ graztreat + (1|block) + (1 + (graztreat|block)),
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
summary(m6)
car::Anova(m6,test="Chisq")


# drop the fitting of covariances between the parameters in the model
m7<-glmmTMB::glmmTMB(specrich ~ graztreat + (1|block) + (0 + (graztreat|block)),
                     family=poisson(link="log"),
                     data=JK_vegetation$FactSpecRich)
summary(m7)
car::Anova(m7,test="Chisq")


# check if better than the previous

# check model assumptions


# would assuming a negative bionomial distribution of errors still improve the model?
