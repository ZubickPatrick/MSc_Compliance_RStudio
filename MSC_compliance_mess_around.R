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
                                                                                                                                              

