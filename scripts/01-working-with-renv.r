#------------HEADER --------
# title: "Using the renv library for making R predictable"
# author: "Han Olff"
# date: "2025-08-22"
# project: Course ERS 2025
# ---

#------------01 Working with collapsable sections in your script ----
# When a line in your script starts with #---- (or more -) then you can collapse and expand it, 
# try this by clicking on the small triangle next to the line number 8 above. 
# Combine this by numbering the sections of your script, and always make section 01 the Header
# This allows you to show an outline of your script, in the R Studio menu use /Code/Show document outline 
# and allows quick naviations through your script


#------------02 Using renv for making R predictable ----
# A major strength, but also potential challenge of R is that the software changes all the time. 
# All functionality # of R comes from packages, and these are improved, expanded, updated all the time. 
# Say that you develop a set of scripts in R now, for example to analyse the data in your master project,
# and you develop a publication from the results. But you get several revisions, and finally complete
# the paper two years later. By that time, you will have updated your versions of R and your 
# packages several times.This may change the functionality of your scripts, they may not work anymore. 
# And when you publish the paper and deposit the data and scripts in a  repository, 
# even 10 years from now you want to be able to replicate your analyses, with the "old" versions of the
# packages that you used at the time when you did the analysis for the paper.

# A similar problem works when working on scripts and data in a collaborative project with multiple people. Then
# typically, not every every collaborator will have exactly the same packages and samve versions of the same
# package installed in their R library. This means only one person can improve the scripts
# and run them, instead of  everyone contributing.

# The renv package solves this in combination with using an R Studio project and Git/Github. 
# for the basis see https://rstudio.github.io/renv/
# In a "fresh" project, you first create an renv.lock file with renv::init(). 
# You typically only do this once. This initialization of your project 
# This serves two goals:
# 1) new packages are installed from then on in a library in the local folder of your project, not anymore in your
# general R installation. So the packages "belong" to the project
# 2) you, or your collaborators, can use renv::restore() to create now or later exactly the same R library 
# (same versions of the packages) as you are using to develop the scripts, where these version numbers are read
# from the renv.lock file. If certain packages are not installed yet on that specific computer, they will be
# installed.

# This ensures that your project uses the exact same package versions and dependencies that were initially 
# recorded, making it easier to reproduce analyses and maintain consistency across different environments or
# collaborators. The version of packages in the library can be update by yourself or your collaborator, but then
# then renv::restore() causes all to use the same versions of all libraries. Be carefull with this, only update
# your library when really nescessary (eg a package contains an error or missing functionality). Otherwise just
# stick to the version that you used when you started the project.

# Make sure that before you use renv::restore(), you have rtools installed. This is a set of tools that are 
# needed to compile some packages from source. Install the RTools version that matches your R version. 
# So if you have R version 4.5.x you need RTools version 45 You can download rtools from the [CRAN website at 
# this link: https://cran.r-project.org/bin/windows/Rtools
# and choose the Rtools installer, using default installation settings.

# -------------------------- 03 Setup working environment----------------
# Make sure that you have the renv package installed. If not, install it using install.packages("renv")
if (!requireNamespace("renv", quietly = TRUE)) {install.packages("renv")}
# Load the renv package
library(renv)
# Make sure that before you use renv::restore(), you have rtools installed (outside R Studio), 
# in the version that matches your R version (eg R version 4.5.x needs RTools version 45).
# You can download rtools from the CRAN website at this link: https://cran.r-project.org/bin/windows/Rtools
renv::restore() 
# This will install the packages that are listed in the renv.lock file in the versions that are listed there.
# these are not necessarily the latest versions of the packages, 
# but the versions that were used when the renv.lock file was created
# this makes sure that the script always works, for different collaborators now, and for yourself in the future
# when packackes may have been updated and changed


# Load the libraries that you will use in this script. Only load the libraries that you will actually use!
library(tidyverse) # load the tidyverse libraries, including readr and ggplot2

# The above section --- 01 Setup ----you always want to include at the start of your script!