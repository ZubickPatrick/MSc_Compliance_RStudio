# here we will be doing the the analyses for my compliance audit of fish stream crossings. 
#general ideas are to perform fish passage assesment on all crossings, run through the compliance audit, compare compliance with fish passage results
#do more in depth analyses on specific variables (SWR and H2O velocity, bankfull, riprap...)

# For now lets start by importing my data.
library(readr)
Compliance_Master_2022_R <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/R-Coding-Folder/Compliance-Master-2022-R.csv")
View(Compliance_Master_2022_R)

#import packages necessary for analyses --> tidyverse
library(tidyverse)
library(ggpubr)
library(car)
library(goft)
library(dplyr)

#lets make sure the raw data table looks good

view(Compliance_Master_2022_R)

# looks mostly good, lots of NA and extra rows. Want to remove rows 31-68 (will keep 31,32,33,34 once data for those sites are collected in AUGUST)


Compliance_Master2022_clean = slice(Compliance_Master_2022_R, 1:30)
view(Compliance_Master2022_clean)

#data now looks good.Will do some preliminary visual analyses of normalcy, with Cost, length, percent cover

qqPlot(Compliance_Master2022_clean$Cost) 

#visually Cost looks good, will do some more general visual analyses of normalcy.

qqPlot(Compliance_Master2022_clean$Length_stream)

qqPlot(Compliance_Master2022_clean$Percent_Coverage_Natural_streambed)
qqPlot(Compliance_Master2022_clean$Outlet_Velocity)

#lets do shapiro-wilks test for normalcy

shapiro.test(Compliance_Master2022_clean$Cost)

# cost is not normally distributed, but thats okay as it does not really need to be analyzed 

#same test but for length, and velocities
shapiro.test(Compliance_Master2022_clean$Length_stream)
#not normal.. but looks normal on visual inspection 

shapiro.test(Compliance_Master2022_clean$Outlet_Velocity)
shapiro.test(Compliance_Master2022_clean$Remediation_Velocity)
shapiro.test(Compliance_Master2022_clean$Inlet_Velocity)

#lets get some long form data for BFull and Slope
#need to subset the rows of interest, start with Slope
library(dplyr)
Slope_LF = dplyr:: select(Compliance_Master2022_clean, Site, DS_Slope_10m, DS_Slope_20m, DS_Slope_30m, DS_Slope_40m, DS_Slope_50m, DS_Slope_60m, DS_Slope_70m, DS_Slope_80m, DS_Slope_90m, DS_Slope_100m, US_Slope_10m, US_Slope_20m, US_Slope_30m, US_Slope_40m, US_Slope_50m, US_Slope_60m, US_Slope_70m, US_Slope_80m, US_Slope_90m, US_Slope_100m, Structure_Slope)

#not entirely sure why I need to Include dplyr:: but it works that way.
view(Slope_LF)

#need to convert DS_Slope_20m to dbl
head(Slope_LF)
Slope_LF$DS_Slope_20m <- as.double(as.character(Slope_LF$DS_Slope_20m))

#check data type
head(Slope_LF)

#looks good to proceed with flipping df

# lets rotate this DF
Slope_LF_Good = Slope_LF %>% pivot_longer(cols = DS_Slope_10m:Structure_Slope, names_to = "Site", values_to = "Slope")   
view(Slope_LF_Good)
rlang::last_error()