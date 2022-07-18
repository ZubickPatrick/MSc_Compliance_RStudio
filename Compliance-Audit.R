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

SWR = BFull %>% mutate (bfull_stream = (DS_bfull_avg+US_bfull_avg)/2)%>% mutate(swr = bfull_stream/Crossing_avg)
view(SWR)

# can use this SWR df for swr compliance assesment and assesment for FPTWG

# starting FPTWG assessment of stream crossings using for loops combine SWR and complianceclean DF

FPTWG_Asses = left_join(SWR, Compliance_Master2022_clean, by = "Site")
view(FPTWG_Asses)

#need to clean up dataframe for assessment select site, remediation class, SWR, structure slope, length stream, perch height, embeddedness

FPTWG_Assessment = dplyr::select(FPTWG_Asses, Site,remediation_class,Crossing_type, swr, Length_stream, Height_perch, Percent_Coverage_Natural_streambed, Structure_Slope)
view(FPTWG_Assessment)
head(FPTWG_Assessment)

# score crossing based on FPTWG rating for length. <15 m = 0, 15-30m = 3, >30 = 6.

FPTWG_Assessment = mutate(FPTWG_Assessment, Length_Result = ifelse(Length_stream < 1500, "0",
                                                       ifelse(Length_stream %in% 1500:3000, "3","6")))

# score crossing based on FPTWG rating for SWR. <1.0 = 0, 1-3 = 5, >1.3 = 6 

FPTWG_Assessment = mutate(FPTWG_Assessment, SWR_Result = ifelse(swr <= 1, "0",
                                                    ifelse(swr>1& swr<3, "3","6")))

# score crossing based on FPTWG rating for perching. <15 cm = 0, 15-30cm = 5, >30cm = 10 

FPTWG_Assessment = mutate(FPTWG_Assessment, Perch_Result = ifelse(Height_perch < 15, "0",
                                                      ifelse(Height_perch %in% 15:30, "5","10")))

# score crossing based on FPTWG rating for slope. <1 % = 0, 1-3% = 5, >3 = 10 

FPTWG_Assessment = mutate(FPTWG_Assessment, Slope_Result = ifelse(Structure_Slope < 1, "0",
                                                      ifelse(Structure_Slope %in% 1:3, "5","10")))

view(FPTWG_Assessment)

# score crossing based on FPTWG rating for embedded. <100% = 10, 100% and ,20% diameter = 5, 100% and > 20% diameter or 30cm = 0 --> gotta sort out the embeddness calc --> presumably OBS = 0 and then only assess CBC based on a ratio of height to width
# likely easier to take a new whole stab at this one with a new DF with required info then left join once finished

embed = dplyr::select(Compliance_Master2022_clean, Site,Stream_Name, Percent_Coverage_Natural_streambed, Crossing_type,Height_Inlet, Width_Structure_inlet)

view(embed)

#quick calculcation to estimate embeddedness in a culvert.
embed = mutate(embed, embeddedness = Width_Structure_inlet - Height_Inlet )
embed = mutate(embed, embed.ratio = (embeddedness/Height_Inlet)*100)

view(embed)

#looks good, lets try to use multiple columns in a ifelse statement.

embed = mutate(embed, embed_Result = ifelse(Percent_Coverage_Natural_streambed < 100, "10",
                                                                  ifelse(Percent_Coverage_Natural_streambed == 100 & embed.ratio >20, "0","5")))
view(embed)

#i am shocked that this worked, but it does so thats neat. lets go and add this embed df to the prior FPTWG assesment one and then select out the columns of interest and then use ifelse to get result.

FPTWG_Assessment_full = left_join(embed, FPTWG_Assessment, by = "Site")

view(FPTWG_Assessment_full)

FPTWG_Assessment_full = dplyr::select(FPTWG_Assessment_full, Site, Stream_Name, remediation_class, Length_Result, Slope_Result, Perch_Result,SWR_Result,embed_Result)
view(FPTWG_Assessment_full)

# let sum the columns and then see how scoring works and use ifelse to get results.
head(FPTWG_Assessment_full)
sapply(FPTWG_Assessment_full, class)

#need to convert scores for each columns from character to numeric before summing --> may be able to pipe this to make cleaner

FPTWG_Assessment_full$Length_Result = as.numeric(as.character(FPTWG_Assessment_full$Length_Result))

FPTWG_Assessment_full$SWR_Result = as.numeric(as.character(FPTWG_Assessment_full$SWR_Result))

FPTWG_Assessment_full$Slope_Result = as.numeric(as.character(FPTWG_Assessment_full$Slope_Result))

FPTWG_Assessment_full$Perch_Result = as.numeric(as.character(FPTWG_Assessment_full$Perch_Result))

FPTWG_Assessment_full$embed_Result = as.numeric(as.character(FPTWG_Assessment_full$embed_Result))

sapply(FPTWG_Assessment_full, class)

#all columns are numeric, so should be able to sum --> we proceed from here.

FPTWG_Assessment_full = mutate(FPTWG_Assessment_full, barrier.score = Length_Result+embed_Result+Slope_Result+Perch_Result+SWR_Result)

view(FPTWG_Assessment_full)

#score looks good, lets use ifelse to get barrier results

FPTWG_Results = mutate(FPTWG_Assessment_full, Barrier_Result = ifelse(barrier.score >= 0 & barrier.score < 14 , "passable",
                                            ifelse(barrier.score >= 15 & barrier.score <= 19, "potential barrier","barrier")))
view(FPTWG_Results)

# going to add a line that produces a numeric result from the barrier assesment.

FPTWG_Results_num = mutate(FPTWG_Assessment_full, Barrier_Result_num = ifelse(barrier.score >= 0 & barrier.score < 14 , "0",
                                                                      ifelse(barrier.score >= 15 & barrier.score <= 19, "1","2")))

view(FPTWG_Results_num)

# not sure why it wont let me keep both columns in. but alas not a big deal. 

# started off today by removing the duplicated rows of data from the DF FPTWG results. Now we have a nice DF that shows the results of the FPTWG Assesment on all sites with full data. 

# results look pretty telling... basically all retrofits score as barriers (due to combination of all potential attributes...) 3 retrofits passable, 13 others either potential barriers or full barriers. Replacements as expected do not score as barriers. 
#REPLACEMENTS WITH EXCEPTION OF NISKONLITH AND 6 MILE CREEK ARE ALL PASSABLE --> THESE ONES NOT DUE TO THE EMBEDDNESS SCORING HIGH DUE TO NOT 100% NATURAL STREAM BED MATERIAL WITH GARDENING FABRIC EXPOSED. 
# THIS FABRIC LIKELY DOES NOT AFFECT FISH PASSAGE... AND SITES ARE LIKLEY BEING WRONGFULLY PUNISHED.

# lets see if i can turn my FPTWG assesment into a barplot quick here. Group by remediation class and then count n in each category of passability
# cant count character --> convert to numeric? Add numbers to the plot.

sapply(FPTWG_Results_num, class)

#appears as though my column is class character --> convert to numeric.

FPTWG_Results_num$Barrier_Result_num = as.numeric(as.character(FPTWG_Results_num$Barrier_Result_num))
sapply(FPTWG_Results_num, class)

count(FPTWG_Results_num, Barrier_Result_num, wt = remediation_class)

# does not work, have a different idea involving grouping by remediation type, then using sum() and aggregate()

FPTWG_Results_play = group_by(FPTWG_Results,remediation_class)

sapply(FPTWG_Results_play, class)

#need to change character vector to factor.

FPTWG_Results_play$remediation_class <- as.factor(FPTWG_Results_play$remediation_class)  
sapply(FPTWG_Results_play, class)

# factor vector in place. now lets try to count.

FPTWG_play = count (FPTWG_Results_play, Barrier_Result, remediation_class, wt = NULL, sort = FALSE, name = "count")
view(FPTWG_play)

# counting has occurred lets show it visually w/ bar graph

#quickly lets omit the NA control

FPTWG_play = na.omit(FPTWG_play)

FPTWG.bar.plot = ggplot(data = FPTWG_play, aes(x= remediation_class, y = count, fill = Barrier_Result))+geom_bar(stat = "identity", position = position_dodge()) +geom_text(aes(label=count), vjust=1.6, color="white",
                                                                                                                                                                          position = position_dodge(0.9), size=3.5)
FPTWG.bar.plot + theme_classic()

# plot looks half decent. I think we can call this a day... 

# lets apply compliance assessment to our data with this we will be working with the full data set. 
# could get clunky and will have to do some mutates but we can get it done.

# lets load full clean data set from earlier down here.

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

# classes look good. can proceed. 
sapply(Compliance_Master2022_clean, class)

# first thing is to do the length assesment 
# score crossing based on FPTWG rating for length. <15 m = 0, 15-30m = 3, >30 = 6.

BP.Asessment = mutate(Compliance_Master2022_clean, Length.Score = ifelse(Length_stream < 1500,"2",                                                                             
                                                                          ifelse(Length_stream %in% 1500:3000, "1","0")))

# now want to do bridge footing width. First need to average the footing widths as some were not uniform US and DS, and get stream width. 
# assesment rooted in footings not encroaching on stream width.

BP.Asessment = mutate(BP.Asessment, Width.avg = (Width_Structure_inlet + Width_Structure_outlet)/2)

#lets call on SWR df from earlier where we calculated Bfull 

view(SWR)

# bfull is here, lets add this df to the BP.Assessment one.

BP.Asessment = left_join(BP.Asessment, SWR, by = "Site")

# a crossing bridge will encroach if the footing width is < width of stream bfull, lets divide bfull by footing width  

BP.Asessment = mutate(BP.Asessment, ratio.bfull.footing = bfull_stream/Width.avg)
view(BP.Asessment)

# have some duplicated rows? Cache creek and Fortune... want to remove those rows... # duplicated rows no longer exist when runnning code so thats weird...

# duplicates are gone, lets do the analysis

BP.Asessment = mutate(BP.Asessment, footing.score = ifelse(ratio.bfull.footing < 1, "1", "0"))


view(BP.Asessment)                                                                         

# looks good. need to ensure I remember to count correctly by OBS and what specific sites are for the requirements. 

# analysis for rip rap bank slope. 0 if slope> 0.50, 1 if slope less that 0.5


BP.Asessment = mutate(BP.Asessment, ripslope = ifelse(rise_run_bank1 >= .5 & rise_run_bank2 >= .5 & rise_run_bank3 >= .5 & rise_run_bank4 >= .5, "0", "1"))


ripslope = dplyr::select(Compliance_Master2022_clean, Site,rise_run_bank1, rise_run_bank2, rise_run_bank3 ,rise_run_bank4)

view(ripslope)

ripslope = ripslope %>% replace(is.na(.), 0)
ripslope = mutate(ripslope, ripslopeassess = ifelse(rise_run_bank1 == 0 & rise_run_bank2 == 0 & rise_run_bank3 == 0 & rise_run_bank4 == 0, "NA",
                                                    ifelse(rise_run_bank1 >0.5, "0",
                                                           ifelse(rise_run_bank2 > 0.5, "0",
                                                                  ifelse(rise_run_bank3 > 0.5, "0",
                                                                         ifelse(rise_run_bank4 > 0.5, "0","1"))))))

view (ripslope)

#lets do the assesmment now 

r

view(ripslope)

# This now works for ripslope and inputs NA for sites with no riprap --> great work 

# now time to assess riprap bank length. 0 = length of bank is < width of disturbed area (crossing width) or less than 20% bfull.
# 1 = length of bank is > or equal to width of disturbed area or greater than 20% disturbed area.

# i need site, width structure inlet and outlet for disturbed width. bfull from SWR and rip_bank_Length_1 etc.

rip.length = dplyr::select(BP.Asessment,Site, Width.avg, bfull_stream, rip_bank_Length_1,rip_bank_Length_2,rip_bank_Length_3,rip_bank_Length_4 )
view(rip.length)

# lets get 20% bfull width column

rip.length = mutate(rip.length, lengthbfull = bfull_stream *.20 )


# i think the 0 sites are causing issues. Lets change 0 to na
rip.length[rip.length == 0] = NA

# ready to do compliance assesment.


rip.length = mutate(rip.length, rip.length.assess = (ifelse(rip_bank_Length_1 == 0 & rip_bank_Length_2 == 0 & rip_bank_Length_3 == 0 & rip_bank_Length_4 == 0, "NA",
                                                            ifelse(rip_bank_Length_1 < Width.avg & rip_bank_Length_1 < lengthbfull & rip_bank_Length_2 < Width.avg & rip_bank_Length_2 < lengthbfull & rip_bank_Length_3 < Width.avg & rip_bank_Length_3 < lengthbfull & rip_bank_Length_4 < Width.avg & rip_bank_Length_4 < lengthbfull, "0","1"))))

rip.length = mutate(rip.length, rip.length.assess = (ifelse(rip_bank_Length_1 < Width.avg, "0", 
                                                                   ifelse(rip_bank_Length_2 < Width.avg, "0",
                                                                           ifelse(rip_bank_Length_3 < Width.avg, "0",
                                                                           ifelse(rip_bank_Length_4 < Width.avg, "0","1"))))))
                                                                    

ifelse(rip_bank_Length_1 == 0 & rip_bank_Length_2 == 0 & rip_bank_Length_3 == 0 & rip_bank_Length_4 == 0, "NA",
                                                                                  
                                                                                  
                                                            ifelse(rip_bank_Length_1 < lengthbfull, "0",
                                                                ifelse(rip_bank_Length_2 < lengthbfull, "0",
                                                                   ifelse(rip_bank_Length_3 < lengthbfull, "0",
                                                                   ifelse(rip_bank_Length_4 < lengthbfull, "0", "1")))))))))))
                                                             
view(rip.length)   

# cannot figure out this one rn. Moving on to next attribute.

# next assessment is of riprap rock size. Avg rock size is non compliant with stream velocity conditions = 0, rock size is compliant = 1. 

rock.asess = dplyr:: select(BP.Asessment, Site, Inlet_Velocity, Remediation_Velocity, Outlet_Velocity, Rip_Rap_Rock_Length_1, Rip_Rap_Rock_Length_2, Rip_Rap_Rock_Length_3, Rip_Rap_Rock_Length_4, Rip_Rap_Rock_Length_5, Rip_Rap_Rock_Length_6,Rip_Rap_Rock_Length_7,Rip_Rap_Rock_Length_8,Rip_Rap_Rock_Length_9,Rip_Rap_Rock_Length_10)
view(rock.asess)


# average stream flow velocity, and rock size ( convert to mm)

# NEED TO account for NA in sites where stream velo was unmeasureed within the crossing.

# switch na with 0 here.

rock.asess$Remediation_Velocity[is.na(rock.asess$Remediation_Velocity)] <- 0


rock.asess = rock.asess %>% mutate(avg.velo = (Inlet_Velocity + Remediation_Velocity + Outlet_Velocity)/3) %>% mutate(avg.rock.size.mm = (Rip_Rap_Rock_Length_1+Rip_Rap_Rock_Length_2+Rip_Rap_Rock_Length_3+Rip_Rap_Rock_Length_4+Rip_Rap_Rock_Length_5+Rip_Rap_Rock_Length_6+Rip_Rap_Rock_Length_7+Rip_Rap_Rock_Length_8+Rip_Rap_Rock_Length_9+Rip_Rap_Rock_Length_10)*10)
view(rock.asess)

# condense Df to values of interest.

rock.asess = dplyr::select(rock.asess, Site, avg.velo, avg.rock.size.mm )
view(rock.asess)
rock.asess=  mutate(rock.asess, rock.assessment = (ifelse(avg.rock.size.mm <0 , "0",
  ifelse(avg.velo >0, "1", "0"))))
view(rock.asess)

# looks great to me. on to next assesment of drainage.

drainage.asess = dplyr:: select(BP.Asessment, Site, Drainage)
view(drainage.asess)

# convert Y to 1 and N to 0
sapply(drainage.asess ,class)

drainage.asess[drainage.asess == "Y"] <- "1"
drainage.asess[drainage.asess == "N"] <- "0"


view(drainage.asess)


drainage.asess = mutate(drainage.asess, drainage.asessment = (ifelse(Drainage == 1, "1", "0")))
view(drainage.asess)


# looks good, on to the next assessment, stream constriction. Compare bfull and crossing bfull to see if constricted...
view(SWR)
constrict = mutate(SWR, constriction.asessment = (ifelse(swr > 1, "0", "1")))
view(constrict)

# looks good. every stream is constricted basically. on to the next attribute stream crossing slope.

#use prior df. LF.Slope 

slope.asessment = mutate(LF.Slope, slope.asess = ifelse(Structure_Slope == Stream_slope_avg, "1", "0"))

view(slope.asessment)
# looks good. ONLY 1 site where stream slope matches the culvert crossing slope.

# new assesment of backwatering. ONlyy forr sites with weir. 

backwater = dplyr::select(Compliance_Master2022_clean, Site, Backwatered, remediation_type)
view(backwater)

backwater = mutate(backwater, backwater.asess = ifelse(Backwatered == 1, "1","0"))
view(backwater)                   


#looks good to me. Carry on to the next asessment.

# going to skip water velocity for now... need to ensure I know what Imma do there..


# lets go to baffles. Compare baffles present to those not.

view(BP.Asessment)

baffle = dplyr::select(BP.Asessment, Site, Expected_Baffles, Actual_baffles)
view(baffle)

baffle = mutate(baffle, baffle.asess = ifelse(Expected_Baffles == Actual_baffles, "1", "0"))
view(baffle)

# loooks great. only 1 site where we were missing baffles.
# on to the next, streambed material retention.

material.reten = dplyr::select(BP.Asessment, Site, Expected_Baffles, Actual_baffles, Percent_Coverage_Natural_streambed)
view(material.reten)

material.reten = mutate(material.reten, retention.asess = ifelse(Actual_baffles >0 & Percent_Coverage_Natural_streambed == 100, "1",
                                                                 ifelse(Actual_baffles >1, "0", "NA")))
view(material.reten)

#works for me, yayyyyyyy,

# lets finish off by doing alignment, and perch.

# alignment analysis

align = dplyr:: select(Compliance_Master2022_clean, Site, Crossing_Stream_Angle)
view(align)

align = mutate(align, align.aess = ifelse(Crossing_Stream_Angle == 90, "1", "0"))
view(align)

# crushed it, on to perching

perch = dplyr::select(Compliance_Master2022_clean, Site, Height_perch)

perch = mutate(perch, perch.asess = ifelse(Height_perch == 0, "1", "0"))

view(perch)

# awesome, good work today. fish velocity and embeddeness remain for attribute to assess.