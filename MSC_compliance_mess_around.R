# lets try to do something and send it over to git
# lets just pull up the data sheet and do a small barplot. 
library(readr)
X2021_Compliance_Data_R2 <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/2021-Compliance-Data-R.csv")
ADD_DFO_FPTWG <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/ADD_DFO_FPTWG.csv")
# MAKE new table with new column 
newdataplay = merge(X2021_Compliance_Data_R2, ADD_DFO_FPTWG, by="Site")
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
X2021_Compliance_Data_R2 <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/R-Coding-Folder/2021-Compliance-Data-R.csv")                                                                                                                                              

library(tidyverse)

# pull out riprap bank slopes for assesment. 

ripslope = select(X2021_Compliance_Data_R2, Site, rise_run_bank1, rise_run_bank2, rise_run_bank3, rise_run_bank4
)
view (ripslope)

# could use the above technique and handwritte in my spreadsheet the values. OR could try to automate the output using if statements 
# i cant figure out if statments rn, so will return later. 

# riprap length assesment. 

riplength = select(X2021_Compliance_Data_R2, Site, US_Bankfull_avg, DS_avg_Bankfull, Width_Structure_inlet, Width_Structure_outlet, rip_bank_Length_1, rip_bank_Length_2, rip_bank_Length_3, rip_bank_Length_4
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
riparea = select(X2021_Compliance_Data_R2, Site,Width_Structure_inlet, Width_Structure_outlet, Length_stream, rip_bank_Length_1, rip_bank_Length_2, rip_bank_Length_3, rip_bank_Length_4, RIP_bank_Height_1, RIP_bank_Height_2, RIP_bank_Height_3, RIP_bank_Height_4)
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

rip_rock_size = select(X2021_Compliance_Data_R2, Site, Inlet_Velocity, Remediation_Velocity, Outlet_Velocity, Rip_rap_Rock_Size_avg)
view(rip_rock_size)

# make cm rock size to mm 

rip_rock_size = mutate(rip_rock_size, riprock_mm = (Rip_rap_Rock_Size_avg * 10))
view(rip_rock_size)

# make avg velocity for analysis with rock size. 

rip_rock_size = mutate(rip_rock_size, avg_velocity = (Inlet_Velocity+Remediation_Velocity+Outlet_Velocity)/ 3)
view(rip_rock_size)

# compliance for rock size is now complete save for the sites with incomplete data. 

# lets do the SWR compliance assesment.

SWR = select(X2021_Compliance_Data_R2, Site, Structure_avg_bankfull, DS_avg_Bankfull, US_Bankfull_avg)
view(SWR)
SWR = mutate(SWR,bfull = (DS_avg_Bankfull + US_Bankfull_avg)/2)
SWR = mutate(SWR, swr = (bfull)/Structure_avg_bankfull)
view(SWR)

# all sites non compliant except for DFO 12 (johnson log stringer)

#lets do crossing slope assesment 

Cross_slope = select(X2021_Compliance_Data_R2, Site, Structure_Slope,DS_Slope_avg, US_Slope_avg)
Cross_slope = mutate(Cross_slope, avg_slope = (DS_Slope_avg = US_Slope_avg)/2)
Cross_slope = mutate(Cross_slope, slope_score = (avg_slope/ Structure_Slope))

view(Cross_slope)

# this works as expected but I need to adjust how I score this based on how the scoring doesnt add up for sites where the str_slope < avg stream slope, and when str slope = 0 and the avg stream slope differs.

# trying to get this if statement thing to work so I can automate my compliance assessment.

if (Cross_slope$Structure_Slope > 1){
  Cross_slope$Structure_Slope <- 0
} else if (Cross_slope$Structure_Slope < 1){
  Cross_slope$Structure_Slope <- 1
}
Cross_slope$Structure_Slope

# doesnt work as I woudl like but will keep trying. 

# assesment for stream velocity 

velocity_scoring = select(X2021_Compliance_Data_R2, Site, Inlet_Velocity, Remediation_Velocity, Outlet_Velocity)
velocity_scoring = velocity_scoring %>% mutate(avg_velocity = (Inlet_Velocity+Remediation_Velocity+Outlet_Velocity)/3)
view(velocity_scoring)

# assessment for embeddedness

embed = select(X2021_Compliance_Data_R2, Site,Crossing_type,Percent_Coverage_Natural_streambed, Height_Inlet, Height_outlet, Width_Structure_inlet, Width_Structure_outlet)
view(embed)

embed = embed %>% mutate(avg_diameter = (Width_Structure_inlet + Width_Structure_outlet)/2) %>% mutate(avg_height = (Height_Inlet+Height_outlet)/2) %>% mutate(embeddness = (avg_diameter-avg_height)) %>% select(Site,Percent_Coverage_Natural_streambed,Crossing_type, embeddness)
view(embed)

# assessment for water depth 

H2O_DEPTH = select(X2021_Compliance_Data_R2, Site, avg_water_depth_structure)
view(H2O_DEPTH)

H2O_DEPTH2 = select(X2021_Compliance_Data_R2, avg_water_depth_structure)
view(H2O_DEPTH2)

if (H2O_DEPTH2 > 10){
  H2O_DEPTH2 <- 1
} else if (H2O_DEPTH2 < 10){
  H2O_DEPTH2 <- 0
}
H2O_DEPTH2

# NOT REALLY SURE WHY THIS ISNT WORKING... WILL FIGURE OUT LATER! bREAK TIME NOW THO. 

# lets Build a quick figure showing the remediation treatments for use in comitee meeting

# change unknown to obs

X2021_Compliance_Data_R2$remediation_type[X2021_Compliance_Data_R2$remediation_type=="unknown"]="OBS"
X2021_Compliance_Data_R2$remediation_type[X2021_Compliance_Data_R2$remediation_type=="baffle and weir"]="baffles and weir"
view(X2021_Compliance_Data_R2)

library(readr)
X2021_Compliance_Data_R2 <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/R-Coding-Folder/2021-Compliance-Data-R2.csv")
type= select(X2021_Compliance_Data_R2, Site, actual_treatment)
type = count(type,actual_treatment)
view(type)
type = na.omit(type)

treatments =ggplot(data= type,aes(x=actual_treatment ,y= n, fill = actual_treatment))+geom_bar(stat="identity")

                                        
treatments + coord_flip() + labs(x = "Remediation Treatment", y = "Count")+ theme_classic()+ theme(legend.position="none")


# add remediator to the DF then make box plot comparing gradient US/Str/DS

ADD_Remediator <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-R-Git-CODING/R-Coding-Folder/ADD_Remediator.csv")
newdf_remediatoradd = dplyr::left_join(ADD_Remediator,X2021_Compliance_Data_R2,by = "Site")
view(newdf_remediatoradd)
gradientS= select(newdf_remediatoradd,Site,US_Slope_avg,DS_Slope_avg, Structure_Slope,Remediator)
view(gradientS)

# make a boxplotof slopes comparing across remediator, need to bring in longform gradient data

Gradient_LongForm <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/Collected-Data/Gradient_LongForm.csv")

boxslope =ggplot(Gradient_LongForm, aes(x=Remediator, y=Slope, fill=Reach))+  geom_boxplot()
                                      
boxslope

longform_FPTWG = dplyr::filter(Gradient_LongForm  , Remediator == "FPTWG")
longform_FPTWG

view(longform_FPTWG)
longform_DFO = dplyr::filter(Gradient_LongForm  , Remediator == "DFO")
longform_DFO
view(longform_DFO)
boxslopeDFO=ggplot(longform_DFO, aes(x=Remediator, y=Slope, fill=Reach))+  geom_boxplot()
boxslopeDFO + theme_classic()        

boxslopeFPTWG=ggplot(longform_FPTWG, aes(x=Remediator, y=Slope, fill=Reach))+  geom_boxplot()
boxslopeFPTWG + theme_classic()


