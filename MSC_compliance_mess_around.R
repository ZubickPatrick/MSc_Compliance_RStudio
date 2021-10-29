# lets try to do something and send it over to git
# lets just pull up the data sheet and do a small barplot. 
library(readr)
X2021_Compliance_Data_R <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/2021-Compliance-Data-R.csv")
ADD_DFO_FPTWG <- read_csv("C:/Users/patch/OneDrive/Desktop/MSc-Culvert/MSc-Proposal/Compliance-Spreadsheets/ADD_DFO_FPTWG.csv")
# MAKE new table with new column 
newdataplay = merge(X2021_Compliance_Data_R, ADD_DFO_FPTWG, by="Site")
View(newdataplay)
