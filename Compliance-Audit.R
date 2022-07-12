#here we will be doing the the analyses for my compliance audit of fish stream crossings. 
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

#lets get the avg slope data in here DS/US/STRUCTURE
#need to summarize my stream reach slope data and then divide by 10 to get average.
#need to replace NA with 0 to ensure calc is all inclusive even if some streams did not have 100m stream reaches US and DS.

LF.Slope = Slope_LF %>% replace(is.na(.),0)%>% mutate (DS_slope_avg = DS_Slope_10m+DS_Slope_20m + DS_Slope_30m + DS_Slope_40m + DS_Slope_50m + DS_Slope_60m + DS_Slope_70m + DS_Slope_80m + DS_Slope_90m + DS_Slope_100m)%>%
mutate(US_slope_avg = US_Slope_10m+US_Slope_20m + US_Slope_30m + US_Slope_40m + US_Slope_50m + US_Slope_60m + US_Slope_70m + US_Slope_80m + US_Slope_90m + US_Slope_100m)%>% 
mutate(US_slope_avg = US_slope_avg/10)%>%
mutate(DS_slope_avg = DS_slope_avg/10)%>% 
mutate(Stream_slope_avg = US_slope_avg + DS_slope_avg)%>%
mutate(Stream_slope_avg = Stream_slope_avg/2)

LF.Slope = dplyr::select(LF.Slope, Site, US_slope_avg, DS_slope_avg, Stream_slope_avg, Structure_Slope)
view(LF.Slope)

#can now use LF.Slope for all slope analyses

#lets do the same thing with BFull measurements

BFull = dplyr:: select(Compliance_Master2022_clean, Site, DS_Bankfull_10m, DS_Bankfull_20m, DS_Bankfull_30m, DS_Bankfull_40m, DS_Bankfull_50m, DS_Bankfull_60m, DS_Bankfull_70m, DS_Bankfull_80m, DS_Bankfull_90m, DS_Bankfull_100m, US_Bankfull_10m, US_Bankfull_20m, US_Bankfull_30m, US_Bankfull_40m, US_Bankfull_50m, US_Bankfull_60m, US_Bankfull_70m, US_Bankfull_80m, US_Bankfull_90m, US_Bankfull_100m, Structure_Bankful_Outlet, Structure_Bankful_Mid, Structure_Bankful_Inlet)

view(BFull)

BFull = BFull %>% replace(is.na(.),0)%>% mutate (DS_bfull_avg = DS_Bankfull_10m+DS_Bankfull_20m+ DS_Bankfull_30m+ DS_Bankfull_40m+ DS_Bankfull_50m+ DS_Bankfull_60m+ DS_Bankfull_70m+ DS_Bankfull_80m+ DS_Bankfull_90m+ DS_Bankfull_100m)%>%
  mutate(US_bfull_avg = US_Bankfull_10m+ US_Bankfull_20m+ US_Bankfull_30m+ US_Bankfull_40m+ US_Bankfull_50m+ US_Bankfull_60m+ US_Bankfull_70m+ US_Bankfull_80m+ US_Bankfull_90m+ US_Bankfull_100m)%>% 
  mutate(US_bfull_avg = US_bfull_avg/10)%>%
  mutate(DS_bfull_avg = DS_bfull_avg/10)%>% 
  mutate(Crossing_avg = Structure_Bankful_Outlet+ Structure_Bankful_Mid+ Structure_Bankful_Inlet)%>% mutate(Crossing_avg =Crossing_avg/3)

BFull = dplyr::select(BFull, Site, US_bfull_avg, DS_bfull_avg,Structure_Bankful_Outlet, Structure_Bankful_Mid, Structure_Bankful_Inlet, Crossing_avg)
view(BFull)
head(BFull)

#lets calculate SWR

SWR = BFull %>% mutate (bfull_stream = (DS_bfull_avg+US_bfull_avg)/2)%>% mutate(SWR = bfull_stream/Crossing_avg)
view(SWR)









