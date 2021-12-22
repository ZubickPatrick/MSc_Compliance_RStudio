# lets try to do something and send it over to git
# lets just pull up the data sheet and do a small barplot. 
library(readr)
X2021_Compliance_Data_R <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/2021-Compliance-Data-R.csv")
ADD_DFO_FPTWG <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/ADD_DFO_FPTWG.csv")
# MAKE new table with new column 
newdataplay = merge(X2021_Compliance_Data_R, ADD_DFO_FPTWG, by="Site")
View(newdataplay)
#new day on R work for CWF report, using fish data

library(readr)
FISH_Data_CWF <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/Fish-Data/FISH_Data_CWF.csv")
FISH_Data_CWF
# remove extra columns zach had in there 
FISH_Data_CWF2 = subset(FISH_Data_CWF, select = -c(...13,...14))
FISH_Data_CWF2
#make new data frame subsetting fish data by species and reach
library(dplyr)
fishcount = dplyr::count(FISH_Data_CWF2,Species,Reach)
fishcount
#make plots from this new data frame
fishplot2 = ggplot(fishcount, aes (x= Species, y = n, fill = Reach))+geom_bar(stat="identity", position = position_dodge()) +scale_fill_manual(values = c("red", "blue")) +theme_classic()+coord_flip()
fishplot2
#make new dataframe for fish size distribution graph. 
fishsize = dplyr::count(FISH_Data_CWF2,Species,FL,Reach)
fishsize
#now need to break up dataframe to US and DS 
#remove DS values
fishsizeUS2 = subset(fishsize, Reach!="DS")
fishsizeUS2
#removed US values
fishsizeDS2 = subset(fishsize, Reach!="US")
fishsizeDS2
#remove DS values
fishsizeplotUS = ggplot(fishsizeUS2, aes (x= FL, y = n))+geom_bar(stat="identity", position = position_dodge())+theme_classic() + xlim(0,250)+labs(x="Fork Length (mm)", y = "Count")
fishsizeplotUS
fishsizeplotDS = ggplot(fishsizeDS2, aes (x= FL, y = n))+geom_bar(stat="identity", position = position_dodge())+theme_classic() + xlim(0,250)+labs(x="Fork Length (mm)", y = "Count")
fishsizeplotDS
#bringing an end to this session. All code works and produces figures as I would hope. 

# new day, trying to apply my compliance assessment to the structures I have completely assessed. 

library(readr)
X2021_Compliance_Data_R <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/R-Coding-Folder/2021-Compliance-Data-R.csv")                                                                                                                                              

library(tidyverse)

# pull out riprap bank slopes for assesment. 

ripslope = select(X2021_Compliance_Data_R, Site, rise_run_bank1, rise_run_bank2, rise_run_bank3, rise_run_bank4
)
view (ripslope)

# could use the above technique and handwritte in my spreadsheet the values. OR could try to automate the output using if statements 
# i cant figure out if statments rn, so will return later. 

# riprap length assesment. 

riplength = select(X2021_Compliance_Data_R, Site, US_Bankfull_avg, DS_avg_Bankfull, Width_Structure_inlet, Width_Structure_outlet, rip_bank_Length_1, rip_bank_Length_2, rip_bank_Length_3, rip_bank_Length_4
)
view (riplength)

str(riplength)

# see the bareminumum riprap bank length (20% of channel width)
riplength = riplength %>% mutate(US_Channelwidth20= US_Bankfull_avg* .20)
riplength = riplength %>% mutate(DS_Channelwidth20= DS_avg_Bankfull* .20)

view(riplength)

# clean up data frame so i cna analyze and pull to excel comnplaince sheet
riplength2 = select(riplength, Site,Width_Structure_inlet, Width_Structure_outlet, rip_bank_Length_1, rip_bank_Length_2, rip_bank_Length_3, rip_bank_Length_4, US_Channelwidth20,DS_Channelwidth20
)
view(riplength2)

#riprap area compliance assessment. 
# filter out data frame. 
riparea = select(X2021_Compliance_Data_R, Site,Width_Structure_inlet, Width_Structure_outlet, Length_stream, rip_bank_Length_1, rip_bank_Length_2, rip_bank_Length_3, rip_bank_Length_4, RIP_bank_Height_1, RIP_bank_Height_2, RIP_bank_Height_3, RIP_bank_Height_4)
view(riparea)


riparea = mutate(riparea, avg_width = (Width_Structure_inlet + Width_Structure_outlet)/2)
view(riparea)

riparea = mutate(riparea, crossing_area  = (avg_width * Length_stream)/10000)
view(riparea)

# now have square meters of crossing area. for use in comparison with rip rap area.
#multiply all length with their corresponding wodths then will add together. 

riparea = mutate(riparea, riparea_1  = (rip_bank_Length_1 * RIP_bank_Height_1)/10000)
riparea = mutate(riparea, riparea_2  = (rip_bank_Length_2 * RIP_bank_Height_2)/10000)
riparea = mutate(riparea, riparea_3  = (rip_bank_Length_3 * RIP_bank_Height_3)/10000)
riparea = mutate(riparea, riparea_4  = (rip_bank_Length_4 * RIP_bank_Height_4)/10000)
view(riparea)

riparea = mutate(riparea, ripareatot  = (riparea_1 + riparea_2 +riparea_3 + riparea_4))
view(riparea)

riparea_good = select(riparea, Site, ripareatot, crossing_area)
view(riparea_good)

riparea_good = mutate(riparea_good, crossingarea_2x  = (crossing_area * 2))
view(riparea_good)

# works but missing crossing area due to not having the data for these sites complete. 

# rip rock size assessment. First will filter out my columns of interest.then perform some mutations.

rip_rock_size = select(X2021_Compliance_Data_R, Site, Inlet_Velocity, Remediation_Velocity
, Outlet_Velocity, Rip_rap_Rock_Size_avg)
