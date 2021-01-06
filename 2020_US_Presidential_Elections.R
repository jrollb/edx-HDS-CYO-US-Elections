######################################################################
# 'CYO' Project: "2020 US Presidential Elections: The Swing States"
# edX / HarvardX PH125.9x 'Data Science: Capstone' project
# Author: jrollb
# 6 Jan, 2021
######################################################################

#------------------------------------------------------------------------
# Install and load libraries
#------------------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(maps)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(ggrepel)
library(knitr)

# Get state map
state_map <- map_data("state")

# Suppress information output from dplyr's summarize()
options(dplyr.summarise.inform = FALSE)

#########################################################################
# Part 1: Data preparation
#########################################################################

#------------------------------------------------------------------------
# Load and merge various data sets on the 2020 US presidential elections
#------------------------------------------------------------------------

# Data set "Election, COVID, and Demographic Data by County"
# Source: Kaggle
# Link: https://www.kaggle.com/etsc9287/2020-general-election-polls
# Copyright label: "CC0: Public Domain"
CountyStatistics <- read.csv("Data/county_statistics.csv")
names(CountyStatistics)[1] <- "countyID"

# Data set "US ELection 2020"
# Source: Kaggle
# Link: https://www.kaggle.com/unanimad/us-election-2020
# Copyright label: "CC0: Public Domain"
# Version: 113
MoreUpToDateVotes <- read.csv("Data/president_county_candidate.csv")

# Data set with votes 2000-2016, but using only 2016
# Source: MIT Election Data + Science Lab (https://electionlab.mit.edu/data)
# Link: "https://doi.org/10.7910/DVN/VOQCHQ"
filename16 <- "Data/countypres_2000-2016.csv"
Other2016Votes <- read.csv(filename16) %>%
  filter(year == 2016)

#------------------------------------------------------------------------
# Exploration
#------------------------------------------------------------------------
# I have removed the initial data exploration steps as this blows up the file.
# However, in the data cleaning steps below it becomes clear how these
# exploration steps looked like

#------------------------------------------------------------------------
# Clean up state names
#------------------------------------------------------------------------

# Mapping table between state abbreviations and state names
mapping.StateNames <- data.frame(state = state.abb,state.name) %>%
  bind_rows(c(state = "DC", state.name = "District of Columbia"))
nrow(mapping.StateNames)
head(mapping.StateNames)

# Rename columns (state for abbreviation, state.name for the name)
Other2016Votes.modif <- Other2016Votes %>%
  rename(state.name = state, state = state_po)

# Rename state name column and add abbreviation
MoreUpToDateVotes.modif <- MoreUpToDateVotes %>%
  rename(state.name = state) %>%
  left_join(mapping.StateNames,by = "state.name")


#------------------------------------------------------------------------
# Clean up county names
#------------------------------------------------------------------------

# Clean "Out of [state]" and "Unassigned" in "CountyStatistics" as there is no data:
CountyStatistics.modif <- CountyStatistics %>%
  filter(!str_detect(county,"Out of") & !str_detect(county,"Unassigned"))

# Unify the column names to enhance the matching rate:
AddCleanCountyName <- function(input_data){
  input_data %>%
    mutate(clean_county_name = str_to_lower(county),
           clean_county_name = str_replace(clean_county_name,
                                           pattern = " county",replacement = ""),
           clean_county_name = str_replace(clean_county_name,
                                           pattern = " parish",replacement = ""),
           clean_county_name = str_replace(clean_county_name,
                                           pattern = " cty twnships",replacement = ""),
           clean_county_name = str_replace(clean_county_name,
                                           pattern = "saint ",replacement = "st. "),
           clean_county_name = if_else(state == "AK",
                                       str_replace(clean_county_name,pattern = "district ",
                                                   replacement = "ed "),clean_county_name))
}

# Apply the above function
CountyStatistics.modif <- CountyStatistics.modif %>% AddCleanCountyName()
MoreUpToDateVotes.modif <- MoreUpToDateVotes.modif %>% AddCleanCountyName()
Other2016Votes.modif <- Other2016Votes.modif %>% AddCleanCountyName()

# A few examples
head(Other2016Votes.modif %>% filter(state == "AK"))
head(MoreUpToDateVotes.modif %>% filter(state == "AK"))
head(CountyStatistics.modif %>% filter(state == "AK"))


#------------------------------------------------------------------------
# Transform "MoreUpToDateVotes.modif" into the format of "CountyStatistics"
#------------------------------------------------------------------------

# Use "base" mapping of US state between state names and abbreviations 
mapping.StateNames <- data.frame(state = state.abb,state.name) %>%
  bind_rows(c(state = "DC", state.name = "District of Columbia"))
nrow(mapping.StateNames) # Should be 51
head(mapping.StateNames) # Should look familiar

# Perform modifications
MoreUpToDateVotes.ByCounty <- MoreUpToDateVotes.modif  %>%
  mutate(mainParty = if_else(party %in% c("DEM","REP"),party,"Other")) %>%
  pivot_wider(names_from = mainParty,values_from = total_votes) %>%
  group_by(state,county,clean_county_name) %>%
  summarize(DEM.20 = sum(DEM,na.rm = TRUE),
            REP.20 = sum(REP,na.rm = TRUE),
            Other.20 = sum(Other,na.rm = TRUE)) %>%
  mutate(total_votes.20 = DEM.20 + REP.20 + Other.20) %>%
  rename(county.20 = county)
# other " Cty Townshps", "Indian Township Vtng Dst"
# do not use " Plt.", it makes things worse

# Inspect
head(MoreUpToDateVotes.ByCounty)


#------------------------------------------------------------------------
# Transform "Other2016Votes.modif" into the format of "CountyStatistics"
#------------------------------------------------------------------------

# Perform modifications
Other2016Votes.ByCounty <- Other2016Votes.modif %>%
  mutate(mainParty = if_else(
    candidate == "Hillary Clinton","DEM",if_else(
      candidate == "Donald Trump","REP","Other"))) %>%
  pivot_wider(names_from = mainParty,values_from = candidatevotes) %>%
  group_by(state,county,clean_county_name) %>%
  summarize(DEM.16 = sum(DEM,na.rm = TRUE),
            REP.16 = sum(REP,na.rm = TRUE),
            Other.16 = sum(Other,na.rm = TRUE)) %>%
  mutate(total_votes.16 = DEM.16 + REP.16 + Other.16) %>%
  rename(county.16 = county)

# Inspect
head(Other2016Votes.ByCounty)

#------------------------------------------------------------------------
# Join the data sets
#------------------------------------------------------------------------

CountyStatistics.expanded <- CountyStatistics.modif %>%
  left_join(MoreUpToDateVotes.ByCounty,by = c("state","clean_county_name"))

CountyStatistics.expanded.with16 <- CountyStatistics.expanded %>%
  left_join(Other2016Votes.ByCounty,by = c("state","clean_county_name"))

# In the following, I check the object "CountyStatistics.expanded.with16"
# and clean it up as good as possible


#------------------------------------------------------------------------
# Check and consolidate
#------------------------------------------------------------------------

# Get overview over mapping issues by counting NAs
data_quality_overview <- CountyStatistics.expanded.with16 %>%
  group_by(state) %>%
  summarize(n = n(),
            n_NA_TotPop = sum(is.na(TotalPop)),
            n_NA_20 = sum(is.na(total_votes20)),
            n_NA_16 = sum(is.na(total_votes16)),
            n_NA.20 = sum(is.na(total_votes.20)),
            n_NA.16 = sum(is.na(total_votes.16)),
            n_max = max(n_NA_TotPop, n_NA_20, n_NA_16, n_NA.20, n_NA.16 )) %>%
  arrange(desc(n_max))

# The following shows that there are various states with severe matching errors
# This comes from issues like counties versus administrative districts etc.
head(data_quality_overview,10)

# I add a column with the likely best data quality available
CountyStatistics.clean <- CountyStatistics.expanded.with16 %>%
  mutate(total_votes20_best = ifelse(is.na(total_votes.20),total_votes20,total_votes.20),
         total_votes16_best = ifelse(is.na(total_votes.16) | state == "VA",total_votes16,total_votes.16))

# A more compact data quality overview:
data_quality_overview_best <- CountyStatistics.clean %>%
  group_by(state) %>%
  summarize(n_county = n(),
            n_NA_TotPop = sum(is.na(TotalPop)),
            n_NA_20 = sum(is.na(total_votes20_best)),
            n_NA_16 = sum(is.na(total_votes16_best)),
            n_max = max(n_NA_20, n_NA_16)) %>%
  arrange(desc(n_max))

data_quality_overview_best # %>% View()

# I now try to repair errors due to mismatches in county names
# Not all can be repaired successfully, they will be excluded in the later analysis
# I keep the attempts for future work

#------------------------------------------------------------------------
# Repair Washington DC
#------------------------------------------------------------------------

CountyStatistics.clean %>% filter(state == "DC")

# It seems like all the 2020 election data is distributed over Wards
tmpColumns2020DC <- c("total_votes20","votes20_Donald_Trump","votes20_Joe_Biden",
                      "DEM.20","REP.20","Other.20","total_votes.20","total_votes20_best")

# Sums over all Wards:
tmpSums <- CountyStatistics.clean %>%
  filter(state == "DC") %>%
  summarize_at(tmpColumns2020DC,sum)

# Replace vote counts in row with county "District of Columbia" with sum over Wards
tmpRow <- which(CountyStatistics.clean$county == "District of Columbia")
CountyStatistics.clean[tmpRow,tmpColumns2020DC] <- tmpSums

# Remove the rows with Wards 2-8:
CountyStatistics.clean <- CountyStatistics.clean %>%
  filter(!(state == "DC" & str_detect(county,"Ward")))

# Check result:
CountyStatistics.clean %>% filter(state == "DC")


#------------------------------------------------------------------------
# Repair Utah
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "UT")

# The 29 counties with data are the 29 found on the Utah Secretary of State pages
# The following regions are not counties and can be filtered out:
tmp <- c("Bear River","Central Utah","Southeast Utah","Southwest Utah","TriCounty","Weber-Morgan")
CountyStatistics.clean <- CountyStatistics.clean %>%
  filter(!((state == "UT") & (county %in% tmp)))


#------------------------------------------------------------------------
# Check MI
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "MI") # %>% View()

# The correctional institutions are not counties
tmp <- c("Federal Correctional Institution (FCI)","Michigan Department of Corrections (MDOC)")
CountyStatistics.clean <- CountyStatistics.clean %>%
  filter(!((state == "MI") & (county %in% tmp)))


#------------------------------------------------------------------------
# Repair RI (not entirely successful!)
#------------------------------------------------------------------------

CountyStatistics.clean %>% filter(state == "RI") # %>% View()

# # Only those rows with "Total Pop" filled with numbers are counties! The remaining ones are "towns".
# CountyStatistics.clean <- CountyStatistics.clean %>%
#   filter(!((state == "RI") & is.na(TotalPop)))
# 
# CountyStatistics.clean %>% filter(state == "RI") # %>% View()
# # Counties "Kent" and "Washington" don't have data for 2020. The election results are probably by "town" and have not been aggregated to county level.

# To repair this state, the mapping from Towns (?) to county districts is required


#------------------------------------------------------------------------
# Check MA
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "MA") # %>% View()

# The counties are:
tmp <- c("barnstable","berkshire","bristol","dukes","essex","franklin","hampden","hampshire","middlesex","nantucket","norfolk","plymouth","suffolk","worcester")
length(tmp)

# CountyStatistics.clean <- CountyStatistics.clean %>%
#   filter(!(state == "MA" & !(clean_county_name %in% tmp)))

# Be careful! 2020 results are on a different granularity


#------------------------------------------------------------------------
# Check MO
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "MO") # %>% View()

# St. Louis City and Kansas City are the two problematic data rows
# St. Louis City could be repaired by merging data "St. Louis City" and "St. Louis city"
# But Kansas City seems more difficult.

tmpRows_MO_city <- with(CountyStatistics.clean,which(state == "MO" & str_detect(county," city")))
for(i in tmpRows_MO_city){
  thisCounty <- CountyStatistics.clean$county[i]
  print(i)
  print(thisCounty)
  
  thisCounty_City <- str_replace(thisCounty," city"," City")
  partnerRow_City <- with(CountyStatistics.clean,
                          which(state == "MO" & county == thisCounty_City))
  if(length(partnerRow_City)>0){
    
    print(thisCounty_City)
    
    # Where are the NAs in the first row?
    tmpNAs <- which(is.na(CountyStatistics.clean[i,]))
    
    # Fill in the NA values of the first line with entries from the second line:
    CountyStatistics.clean[i,tmpNAs] <-
      CountyStatistics.clean[partnerRow_City,tmpNAs]
    
    # Mark partner row for deletion
    CountyStatistics.clean[partnerRow_City,"county"] <- "delete"
    
  }
}

# Kansas City belongs to Jackson county, 
tmpRow <- with(CountyStatistics.clean,
               which(state == "MO" & county %in% c("Jackson","Kansas City")))
CountyStatistics.clean[tmpRow,]

# For these columns the data has to be added to Jackson county
tmpCols <- c("cases","deaths","DEM.16","REP.16","Other.16","total_votes.16","total_votes16_best")
CountyStatistics.clean[tmpRow[1],tmpCols] <- CountyStatistics.clean[tmpRow[1],tmpCols] +
  CountyStatistics.clean[tmpRow[2],tmpCols]

# Now that Kansas city has been combined with Jackson county, we can remove the row
CountyStatistics.clean[tmpRow[2],"county"] <- "delete"

# Clean up
CountyStatistics.clean <- CountyStatistics.clean %>%
  filter(!(county == "delete"))


#------------------------------------------------------------------------
# Check LA
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "LA") # %>% View()

# "La Salle" versus "LaSalle"

CountyStatistics.clean %>% filter(state == "LA" & county %in% c("La Salle","LaSalle"))

tmpCountyID.1 <- CountyStatistics.clean %>%
  filter(state == "LA" & county == "La Salle") %>%
  pull(countyID)
tmpCountyID.2 <- CountyStatistics.clean %>%
  filter(state == "LA" & county == "LaSalle") %>%
  pull(countyID)

tmpRowNumber.1 <- which(CountyStatistics.clean$countyID == tmpCountyID.1)
tmpRowNumber.2 <- which(CountyStatistics.clean$countyID == tmpCountyID.2)

tmpNAs.1 <- which(is.na(CountyStatistics.clean[tmpRowNumber.1,]))
tmpNAs.2 <- which(is.na(CountyStatistics.clean[tmpRowNumber.2,]))

# Fill in the NA values of the first line with entries from the second line:
CountyStatistics.clean[tmpRowNumber.1,tmpNAs.1] <- CountyStatistics.clean[tmpRowNumber.2,tmpNAs.1]
CountyStatistics.clean[tmpRowNumber.1,]

# Then remove the second line:
CountyStatistics.clean <- CountyStatistics.clean[-tmpRowNumber.2,]
CountyStatistics.clean %>% filter(state == "LA" & county %in% c("La Salle","LaSalle"))


#------------------------------------------------------------------------
# Check IL
#------------------------------------------------------------------------

CountyStatistics.clean %>% filter(state == "IL") # %>% View()

# Exclude "Cook Suburbs" from counties:
tmpCountyID <- CountyStatistics.clean %>%
  filter(state == "IL" & county == "Cook Suburbs") %>%
  pull(countyID)
tmpRowNumber <- which(CountyStatistics.clean$countyID == tmpCountyID)
CountyStatistics.clean <- CountyStatistics.clean[-tmpRowNumber,]


#------------------------------------------------------------------------
# Check HI
#------------------------------------------------------------------------

CountyStatistics.clean %>% filter(state == "HI") # %>% View()

# In the current input data, there are no votes recorded for Kalawao county

# Identify and remove the row
tmpCountyID <- CountyStatistics.clean %>%
  filter(state == "HI" & county == "Kalawao") %>%
  pull(countyID)
tmpRowNumber <- which(CountyStatistics.clean$countyID == tmpCountyID)
CountyStatistics.clean <- CountyStatistics.clean[-tmpRowNumber,]


#------------------------------------------------------------------------
# Repair "NM"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "NM") # %>% View()

# The county "Dona Ana" occurs three times
CountyStatistics.clean %>% filter(state == "NM" & str_detect(county, "Do")) # %>% View

# Each column has only one value != NA in these three rows

# Identify the countyIDs and row numbers:
tmpCountyID <- CountyStatistics.clean %>% filter(state == "NM" & str_detect(county, "Do")) %>%
  pull(countyID)
tmpRowNumber <- which(CountyStatistics.clean$countyID %in% tmpCountyID)

# Where are the NAs?
tmpNAs <- is.na(CountyStatistics.clean[tmpRowNumber,])

# Fill in the NA values of the first line with entries from the second line:
CountyStatistics.clean[tmpRowNumber[1],tmpNAs[1,]] <- 
  CountyStatistics.clean[tmpRowNumber[2],tmpNAs[1,]]

# Update NA table
tmpNAs <- is.na(CountyStatistics.clean[tmpRowNumber,])

# Fill in the NA values of the first line with entries from the third line:
CountyStatistics.clean[tmpRowNumber[1],tmpNAs[1,]] <- 
  CountyStatistics.clean[tmpRowNumber[3],tmpNAs[1,]]

# Then remove the second and third line:
CountyStatistics.clean <- CountyStatistics.clean[-tmpRowNumber[c(2,3)],]
CountyStatistics.clean %>% filter(state == "NM" & str_detect(county, "Do"))


#------------------------------------------------------------------------
# Check "MD"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "MD") # %>% View()

# The county "Baltimore City" (differs from "Baltimore"!) has two entries
tmpProblems <- c("Baltimore city", "Baltimore City")

# Identify the countyIDs and row numbers:
tmpCountyID <- CountyStatistics.clean %>% filter(state == "MD" & county %in% tmpProblems) %>%
  pull(countyID)
tmpRowNumber <- which(CountyStatistics.clean$countyID %in% tmpCountyID)

# Where are the NAs in the first row?
tmpNAs <- which(is.na(CountyStatistics.clean[tmpRowNumber[1],]))

# Fill in the NA values of the first line with entries from the second line:
CountyStatistics.clean[tmpRowNumber[1],tmpNAs] <- 
  CountyStatistics.clean[tmpRowNumber[2],tmpNAs]

# Then remove the second and third line:
CountyStatistics.clean <- CountyStatistics.clean[-tmpRowNumber[2],]
CountyStatistics.clean %>% filter(state == "MD" & str_detect(county, "Bal"))


#------------------------------------------------------------------------
# Check "VA"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "VA") %>% 
  arrange(county) # %>% View()

# There are some mapping errors ("city" versus "City" or versus "" etc.)
# It seems safest to clean this up in a loop - safety over elegance here!

# Plot outliers
CountyStatistics.clean %>%
  filter(state == "VA") %>%
  ggplot(aes(total_votes16,total_votes.16)) +
  geom_point()

CountyStatistics.clean %>%
  filter(state == "VA" & abs(total_votes16/total_votes.16-1) > 0.1) %>%
  ggplot(aes(total_votes16,total_votes.16)) +
  geom_point()

# The plots show that it is not okay yet

# Example: Richmont City

tmpRows_VA_city <- with(CountyStatistics.clean,which(state == "VA" & str_detect(county," city")))
for(i in tmpRows_VA_city){
  thisCounty <- CountyStatistics.clean$county[i]
  print(i)
  print(thisCounty)
  
  thisCounty_City <- str_replace(thisCounty," city"," City")
  partnerRow_City <- with(CountyStatistics.clean,
                          which(state == "VA" & county == thisCounty_City))
  if(length(partnerRow_City)>0){
    
    print(thisCounty_City)
    
    # Where are the NAs in the first row?
    tmpNAs <- which(is.na(CountyStatistics.clean[i,]))
    
    # Fill in the NA values of the first line with entries from the second line:
    CountyStatistics.clean[i,tmpNAs] <- 
      CountyStatistics.clean[partnerRow_City,tmpNAs]
    
    # Mark partner row for deletion
    print(paste0("Marking ",CountyStatistics.clean[partnerRow_City,"county"],
                 " with countyID ",partnerRow_City, " for deletion."))
    CountyStatistics.clean[partnerRow_City,"county"] <- "delete"
    
  } else {
    thisCounty_bare <- str_replace(thisCounty," city","")
    partnerRow_bare <- with(CountyStatistics.clean,
                            which(state == "VA" & county == thisCounty_bare))
    if(length(partnerRow_bare)>0){
      
      print(thisCounty_bare)
      
      # Where are the NAs in the first row?
      tmpNAs <- which(is.na(CountyStatistics.clean[i,]))
      
      # Fill in the NA values of the first line with entries from the second line:
      CountyStatistics.clean[i,tmpNAs] <- 
        CountyStatistics.clean[partnerRow_bare,tmpNAs]
      
      # Mark partner row for deletion
      CountyStatistics.clean[partnerRow_bare,"county"] <- "delete"
      
    }
  }
}

# Clean up
CountyStatistics.clean <- CountyStatistics.clean %>%
  filter(!(county == "delete"))

CountyStatistics.clean %>%
  filter(state == "VA" & str_detect(county,"Ri"))


#------------------------------------------------------------------------
# Check "CT"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "CT") %>% 
  arrange(county) # %>% View()

# The counties are
CT.counties <- c("Fairfield","Hartford","Litchfield","Middlesex",
                 "New Haven","New London","Tolland","Windham")

# But 2020 votes are per townships!!!

if(F){
  # Remove everything else
  CountyStatistics.clean <- CountyStatistics.clean %>%
    filter(!(state == "CT" & !(county %in% CT.counties)))
}

# But this does not look right! The data of this state cannot be used.

# To repair this state, the mapping from Townships to county districts is required


#------------------------------------------------------------------------
# Check "NH"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "NH") %>% 
  arrange(county) # %>% View()
NH.counties <- c("Belknap","Carroll","Cheshire","Coos","Grafton","Hillsborough",
                 "Merrimack","Rockingham","Strafford","Sullivan")

# In principle, these should be filtered out, but they contain the 2020 election data!
CountyStatistics.clean %>%
  filter(state == "NH" & !(county %in% NH.counties))

# To repair this state, the mapping from Towns (?) to county districts is required


#------------------------------------------------------------------------
# Check "ME"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "ME") %>% 
  arrange(county) # %>% View()

# To repair this state, the mapping from Towns / Townships / districts to county is required


#------------------------------------------------------------------------
# Check "VT"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "VT") %>% 
  arrange(county) # %>% View()

# To repair this state, the mapping from Towns / Townships / districts to county is required


#------------------------------------------------------------------------
# Check "AK"
#------------------------------------------------------------------------
CountyStatistics.clean %>% filter(state == "AK") %>% 
  arrange(county) # %>% View()

# To repair this state, the mapping from Towns / Townships / districts to county is required


#------------------------------------------------------------------------
# Final data quality check
#------------------------------------------------------------------------

# Show states, where there are counties with missing data
data_quality_overview_best <- CountyStatistics.clean %>%
  group_by(state) %>%
  summarize(n_county = n(),
            TotPop = sum(TotalPop,na.rm = TRUE),
            n_NA_TotPop = sum(is.na(TotalPop)),
            n_NA_20 = sum(is.na(total_votes20_best)),
            n_NA_16 = sum(is.na(total_votes16_best)),
            n_max = max(n_NA_TotPop, n_NA_20, n_NA_16)) %>%
  arrange(desc(n_max))

gap_states_of_merged_set <- data_quality_overview_best %>%
  filter(n_max>0) %>%
  pull(state)

# data_quality_overview_best %>% View()

if(T){
  # Get the gap states directly:
  gap_states <- CountyStatistics.clean %>%
    filter(is.na(TotalPop) | is.na(total_votes20_best) | is.na(total_votes16_best)) %>%
    pull(state) %>% unique() %>% sort()
  
  cutoff_percentage <- 0.5
  CountyStatistics.clean %>%
    filter(!(state %in% gap_states)) %>%
    ggplot(aes(total_votes16,total_votes20)) +
    geom_point() +
    geom_abline(slope = 1,intercept = 0) +
    geom_abline(slope = cutoff_percentage,intercept = 0,color="grey",lty=5)
}


#------------------------------------------------------------------------
# Select the most up to date colums for the votes
#------------------------------------------------------------------------

Election.US.16.20 <- CountyStatistics.clean %>%
  rename(pct.DEM.16 = percentage16_Hillary_Clinton,
         pct.REP.16 = percentage16_Donald_Trump,
         votes.TOT.16 = total_votes16,
         votes.DEM.16 = votes16_Hillary_Clinton,
         votes.REP.16 = votes16_Donald_Trump,
         pct.DEM.20 = percentage20_Joe_Biden,
         pct.REP.20 = percentage20_Donald_Trump,
         votes.TOT.20 = total_votes20,
         votes.DEM.20 = votes20_Joe_Biden,
         votes.REP.20 = votes20_Donald_Trump) %>%
  mutate(pct.DEM.16 = votes.DEM.16/votes.TOT.16,
         pct.REP.16 = votes.REP.16/votes.TOT.16,
         votes.TOT.16 = total_votes16_best,
         votes.DEM.16 = ifelse(is.na(DEM.16) | state == "VA",votes.DEM.16,DEM.16),
         votes.REP.16 = ifelse(is.na(REP.16) | state == "VA",votes.REP.16,REP.16),
         pct.DEM.20 = votes.DEM.20/votes.TOT.20,
         pct.REP.20 = votes.REP.20/votes.TOT.20,
         votes.TOT.20 = total_votes20_best,
         votes.DEM.20 = ifelse(is.na(DEM.20),votes.DEM.20,DEM.20),
         votes.REP.20 = ifelse(is.na(REP.20),votes.REP.20,REP.20),
         total_votes20_best = NULL,
         total_votes16_best = NULL)

# Inspect
Election.US.16.20 %>%
  filter(!(state %in% gap_states) & votes.TOT.20 <= 0.5 * votes.TOT.16)
Election.US.16.20 %>%
  filter(state == "VA" & str_detect(county,"Ri"))
CountyStatistics.clean %>%
  filter(state == "VA" & str_detect(county,"Ri"))
Other2016Votes.modif %>%
  filter(state == "VA" & str_detect(county,"Ri"))
MoreUpToDateVotes.modif %>%
  filter(state == "VA" & str_detect(county,"Ri"))

#------------------------------------------------------------------------
# Save final data set as rds file
#------------------------------------------------------------------------

# Check if RDS directory exists, otherwise create it
if(!dir.exists("RDS")){
  dir.create("RDS")
}

# Save as .rds file
saveRDS(Election.US.16.20,"RDS/Election.US.16.20.rds")

# NOTE: This is the rds file I read in the markdown file (.Rmd), using
# Election.US.16.20 <- readRDS("RDS/Election.US.16.20.rds")

#------------------------------------------------------------------------
# Clean up
#------------------------------------------------------------------------
remove(CountyStatistics,
       CountyStatistics.clean,
       CountyStatistics.expanded,
       CountyStatistics.expanded.with16,
       CountyStatistics.modif)

remove(MoreUpToDateVotes,
       MoreUpToDateVotes.ByCounty,
       MoreUpToDateVotes.modif)

remove(Other2016Votes,
       Other2016Votes.ByCounty,
       Other2016Votes.modif)


#########################################################################
# Part 2: Analysis of the data
#########################################################################

#------------------------------------------------------------------------
# Define hold-out sample of states
#------------------------------------------------------------------------

# Hold-out sample of 'battleground' states
states_validation <- c("MI","WI","GA","PA","AZ","NC","IA","OH","FL","WV")

#------------------------------------------------------------------------
# Remove states with data gaps and add columns for later use
#------------------------------------------------------------------------

# Identify gap states
gap_states <- Election.US.16.20 %>%
  filter(is.na(TotalPop) | is.na(votes.TOT.16) | is.na(votes.TOT.20)) %>%
  pull(state) %>% unique() %>% sort()
gap_states

# Exclude them
Election.US.16.20 <- Election.US.16.20 %>%
  filter(!(state %in% gap_states))

# Add some columns needed later
Election.US.16.20 <- Election.US.16.20 %>%
  mutate(spread20 = pct.DEM.20 - pct.REP.20,
         spread16 = pct.DEM.16 - pct.REP.16,
         delta.spread = spread20 - spread16,
         delta.pct.total_votes = votes.TOT.20/votes.TOT.16 - 1,
         pct.Employed = Employed/TotalPop,
         pct.cases = cases/TotalPop)

#------------------------------------------------------------------------
# Split into 'validation' set and call the training data 'election'
#------------------------------------------------------------------------

# Split out the hold-out sample as 'validation'
validation <- Election.US.16.20 %>%
  filter(state %in% states_validation)

# And the rest for training into 'election'
election <- Election.US.16.20 %>%
  filter(!(state %in% states_validation))

#------------------------------------------------------------------------
# Check data quality: outliers with very different vote numbers?
#------------------------------------------------------------------------

# Plot total vote numbers by county, 2020 versus 2016
election %>%
  ggplot(aes(votes.TOT.16,votes.TOT.20)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Total votes in 2016 (by county)") +
  ylab("Total votes in 2020 (by county)")


#------------------------------------------------------------------------
# Visualization of the main features
#------------------------------------------------------------------------

# Plot spread of the 2020 election
election %>%
  filter(!(state %in% c("AK","HI")) & long != 0 & lat != 0) %>%
  ggplot() +
  geom_polygon(data = state_map,aes(long,lat,group = group),fill="grey",color="black") +
  geom_point(aes(long,lat,color=spread20,size=TotalPop)) +
  scale_color_gradient2(low="red",midpoint = 0.0,high = "blue") + # coord_fixed() +
  ggtitle("Spreads in the 2020 US presidential election (excl. hold-out sample)") +
  xlab("Longitude (in degrees)") +
  ylab("Latitude (in degrees)")

# Plot spread of the 2016 election
election %>%
  filter(!(state %in% c("AK","HI")) & long != 0 & lat != 0) %>%
  ggplot() +
  geom_polygon(data = state_map,aes(long,lat,group = group),fill="grey",color="black") +
  geom_point(aes(long,lat,color=spread16,size=TotalPop)) +
  scale_color_gradient2(low="red",midpoint = 0.0,high = "blue") + # coord_fixed() +
  ggtitle("Spreads in the 2016 US presidential election (excl. hold-out sample)") +
  xlab("Longitude (in degrees)") +
  ylab("Latitude (in degrees)")

# Plot increase in voter turnout
election %>%
  filter(!(state %in% c("AK","HI")) & long != 0 & lat != 0) %>%
  rename(increase.voterTurnout = delta.pct.total_votes) %>%
  ggplot() +
  geom_polygon(data = state_map,aes(long,lat,group = group),fill="grey",color="black") +
  geom_point(aes(long,lat,color=increase.voterTurnout,size=TotalPop)) +
  scale_color_gradient2(low="magenta",midpoint = 0.0,high = "green") + # coord_fixed() +
  ggtitle("Increase in voter turnout (%) between 2016 and 2020") +
  xlab("Longitude (in degrees)") +
  ylab("Latitude (in degrees)")

# Plot increase of spread
election %>%
  filter(!(state %in% c("AK","HI")) & long != 0 & lat != 0) %>%
  ggplot() +
  geom_polygon(data = state_map,aes(long,lat,group = group),fill="grey",color="black") +
  geom_point(aes(long,lat,color=delta.spread,size=TotalPop)) +
  scale_color_gradient2(low="red",midpoint = 0.0,high = "blue") + # coord_fixed() +
  ggtitle("Increase in spread (%) between 2016 and 2020") +
  xlab("Longitude (in degrees)") +
  ylab("Latitude (in degrees)")

#------------------------------------------------------------------------
# Check large changes in Texas
#------------------------------------------------------------------------

# Show ten largest changes in spread
election %>%
  select(state,county,delta.spread,votes.DEM.20,votes.REP.20,votes.DEM.16,votes.REP.16) %>%
  arrange(desc(abs(delta.spread))) %>%
  head(10)

# Try to explain of demographics
election %>%
  select(state,county,delta.spread,Hispanic) %>%
  arrange(desc(abs(delta.spread))) %>%
  head(10)

# Compare to urban counties
election %>%
  filter(state == "TX") %>%
  select(state,county,delta.spread,TotalPop,Hispanic) %>%
  arrange(desc(TotalPop)) %>%
  head()

#------------------------------------------------------------------------
# Further visualization
#------------------------------------------------------------------------

# Demonstrate prevalence issue
election %>%
  arrange(desc(TotalPop)) %>%
  mutate(rank = 1:nrow(.)) %>%
  ggplot(aes(rank,TotalPop)) +
  geom_point() +
  xlab("Rank") +
  ylab("County population") +
  ggtitle("Large differences of county population (shown in descending order)")

# Define the relative spread normalized to population rather than total votes
election <- election %>%
  mutate(Delta.20.rel = (votes.DEM.20 - votes.REP.20)/TotalPop,
         Delta.16.rel = (votes.DEM.16 - votes.REP.16)/TotalPop)

# Show similarity between 2016 and 2020
election %>%
  ggplot(aes(Delta.16.rel,Delta.20.rel,size=TotalPop,color=Hispanic)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  # geom_smooth(method = "lm", se = FALSE,color="orange") +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.3, method.args = list(degree=1), color="orange") +
  xlab("Spread 2016 normalized by population number") +
  ylab("Spread 2020 normalized by population number") +
  ggtitle("Spreads 2020 versus 2016")

#------------------------------------------------------------------------
# Determine and subtract the 'loess' predictions
#------------------------------------------------------------------------

# Fit 'loess' as baseline model
fitLoessBaseline <- loess(formula = Delta.20.rel ~ Delta.16.rel,data = election, span = 0.3, method.args = list(degree=1))

# Add baseline and residual to dataset
election <- election %>%
  mutate(Delta.20.rel.baseline = predict(fitLoessBaseline),
         residual.16.20 = Delta.20.rel - Delta.20.rel.baseline)

# Plot the residual
election %>%
  ggplot(aes(Delta.16.rel,residual.16.20,size=TotalPop,color=Hispanic)) +
  geom_point() +
  geom_hline(yintercept = 0,color="red") +
  geom_abline(slope = -1,intercept = 0,color="blue") +
  xlab("Spread 2016 normalized by population number") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread after subtracting 'loess' prediction")

#------------------------------------------------------------------------
# Look for features
#------------------------------------------------------------------------

# Show dependency between the residuals and the population
election %>%
  ggplot(aes(TotalPop,residual.16.20)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.5, method.args = list(degree=1), color="orange") +
  geom_hline(yintercept = 0,color="red") +
  geom_vline(xintercept = 45000,color="red") +
  xlab("Population number") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread versus county population number")

# Employment rate
election %>%
  ggplot(aes(pct.Employed,residual.16.20,size=TotalPop)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.5, method.args = list(degree=1), color="orange") +
  xlab("Employed rate in percent") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread versus county employment rate")

# Geographic dependency: Longitude
election %>%
  filter(state != "HI") %>%
  ggplot(aes(long,residual.16.20,size=TotalPop)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.5, method.args = list(degree=1), color="orange") +
  xlab("Longitude") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread versus longitude")

# Geographic dependency: Latitude
election %>%
  filter(state != "HI") %>%
  ggplot(aes(lat,residual.16.20,size=TotalPop)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.5, method.args = list(degree=1), color="orange") +
  xlab("Latitude") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread versus latitude")

# Previous voter turnout rate
election %>%
  filter(state != "HI") %>%
  mutate(TurnoutRate.16 = votes.TOT.16/TotalPop) %>%
  ggplot(aes(TurnoutRate.16,residual.16.20,size=TotalPop)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.5, method.args = list(degree=1), color="orange") +
  xlab("Voter turnout rate in 2016 in percent") +
  ylab("Residual spread 2020") +
  ggtitle("Residual spread versus 2016 voter turnout")

# Look for confounders: employment rate versus latitude
election %>%
  filter(!is.na(pct.Employed) & state != "HI") %>%
  ggplot(aes(lat,pct.Employed,color=White,size=TotalPop)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.3, method.args = list(degree=1)) +
  xlab("Latitude") +
  ylab("Employed rate in percent") +
  ggtitle("County employment rate versus latitude")


#########################################################################
# Part 3: Machine learning
#########################################################################

#------------------------------------------------------------------------
# Define RMSE function
#------------------------------------------------------------------------

# RMSE without weighting
RMSE <- function(yPred,yAct){
  sqrt(mean((yPred - yAct)^2))
}

# RMSE with weighting
RMSE.weighted <- function(yPred,yAct,weight){
  sqrt(weighted.mean((yPred - yAct)^2,w = weight))
}

#------------------------------------------------------------------------
# Prepare data for machine learning
#------------------------------------------------------------------------

# Dataset for machine learning
change.16.20 <- election %>%
  filter(!is.na(lat) & !is.na(long) & !(state == "HI")) %>%
  mutate(y = residual.16.20,
         White.pct = White/100,
         Black.pct = Black/100,
         Hispanic.pct = Hispanic/100,
         Asian.pct = Asian/100,
         Native.pct = Native/100,
         Pacific.pct = Pacific/100,
         Women.pct = Women/TotalPop,
         Employment.pct = Employed/TotalPop,
         Construction.pct = Construction/TotalPop,
         TurnoutRate.16 = votes.TOT.16/TotalPop) %>%
  select(countyID,
         county,
         state,
         y,
         lat,
         long,
         TotalPop,
         TurnoutRate.16,
         Delta.16.rel,
         White.pct,
         Black.pct,
         Hispanic.pct,
         Asian.pct,
         Native.pct,
         Pacific.pct,
         Women.pct,
         Employment.pct,
         Construction.pct)

#------------------------------------------------------------------------
# Prepare training and test set
#------------------------------------------------------------------------

# Set seed (for reproducibility)
set.seed(123)

# Partition for total vote number percentage change
index_test <- createDataPartition(change.16.20$y,times = 1,p = 0.3,list = FALSE)
change.16.20.train <- change.16.20[-index_test,]
change.16.20.test <- change.16.20[index_test,]


#------------------------------------------------------------------------
# Model overview
#------------------------------------------------------------------------

# Baseline model: average
# Linear regression
# CART model
# Random forest
# xgboost
# KNN

#------------------------------------------------------------------------
# Baseline model
#------------------------------------------------------------------------

# Average changes 
mu <- mean(change.16.20.train$y)
print(paste0("Average residual spread: ",round(mu*100,1),"%"))

# Add the predictions as extra columns to the test set
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_baseline = mu)

# Collect the RMSE results into a data.frame
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_baseline,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, 
                          RMSE.weighted(yPred = y_hat_baseline,yAct = y,weight = TotalPop))
Results <- data.frame(model = c("baseline"),
                      RMSE = thisRMSE,
                      RMSE.weighted = thisRMSE.weighted)

Results

#------------------------------------------------------------------------
# Linear regression
#------------------------------------------------------------------------

# Define the set of features to be investigated
featureList.1 <- c("y","lat","long","TotalPop", "TurnoutRate.16", "Delta.16.rel",
                   "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                   "Women.pct","Employment.pct","Construction.pct")
featureList.2 <- c("y","lat","long","TotalPop",
                   "Black.pct","Hispanic.pct",
                   "Employment.pct","Construction.pct","TurnoutRate.16")
featureList.3 <- c("y","TotalPop",
                   "Black.pct","Hispanic.pct",
                   "Employment.pct","Construction.pct","TurnoutRate.16")

# weightVec.Train <- change.16.20.train$TotalPop
weightVec.Test <- change.16.20.test$TotalPop

list.featureList <- list(featureList.1,featureList.2,featureList.3)

for(i in 1:length(list.featureList)){
  
  # The current feature list is
  featureList <- list.featureList[[i]]
  # print(featureList)
  
  # Select these features
  tmpData <- change.16.20.train %>%
    select(starts_with(featureList))
  
  # Fit the model
  fit <- lm(y ~ .,data = tmpData)
  # fit.weighted <- lm(y ~ .,data = tmpData,weights = weightVec.Train)
  
  # Print the summary
  print("#############################################################")
  print(paste0(i,".) Model with feature list ",i))
  print("#############################################################")
  print(featureList)
  print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
  print("Summary:")
  print(summary(fit))
  
  # Predict
  change.16.20.test <- change.16.20.test %>%
    mutate(y_hat_lm = predict(fit, newdata = change.16.20.test))
  
  ## Calculate RMSE
  thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_lm,yAct = y))
  thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_lm,yAct = y,weight = weightVec.Test))
  
  ## Results
  Results <- Results %>% bind_rows(data.frame(model = paste0("lm.ftl.",i),
                                              RMSE = thisRMSE,
                                              RMSE.weighted = thisRMSE.weighted))
}

Results

## Add cross validation (for "lm" it will yield the same result as without)

# Define the set of features to be investigated
featureList <- c("y","TotalPop",
                 "Black.pct","Hispanic.pct",
                 "Employment.pct","Construction.pct","TurnoutRate.16")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Select weights
# weightVec.Train <- change.16.20.train$TotalPop
weightVec.Test <- change.16.20.test$TotalPop

# Train the model
set.seed(2)
# fit.weighted <- train(y ~ .,weights = weightVec.Train, method = "lm", data = tmpData)
fit <- train(y ~ ., method = "lm", data = tmpData)

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_lm.cv = predict(fit, newdata = change.16.20.test))

# Calculate RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_lm.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_lm.cv,yAct = y,weight = weightVec.Test))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("lm.cv"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))
# Show results
Results


#------------------------------------------------------------------------
# CART model (here: regression tree)
#------------------------------------------------------------------------

# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")


# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Fit the model
# fit.weighted <- rpart(y ~ .,data = tmpData,weights = TotalPop)
fit <- rpart(y ~ .,data = tmpData)

# Plot (using package 'rpart.plot')
prp(fit)

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_rpart = predict(fit, newdata = change.16.20.test))

# Calculate RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_rpart,yAct = y))
thisRMSE.weighted <- with(change.16.20.test,
                          RMSE.weighted(yPred = y_hat_rpart,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("rpart"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))


## Cross validation. 

featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Train the model
set.seed(10)
fit <- train(y ~ .,method = "rpart", data = tmpData,
             tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)))
# fit.weighted <- train(y ~ .,weights = TotalPop, method = "rpart", data = tmpData,
#              tuneGrid = data.frame(cp = seq(0, 0.2, len = 25)))

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_rpart.cv = predict(fit, newdata = change.16.20.test))


# RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_rpart.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_rpart.cv,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("rpart.cv"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))

# Plot tree for the final model
prp(fit$finalModel)

Results


#------------------------------------------------------------------------
# Random forest
#------------------------------------------------------------------------


# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Fit the model
fit <- randomForest(y~.,data = tmpData)

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_rf = predict(fit, newdata = change.16.20.test))

# RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_rf,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_rf,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("rf"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))

# Variable importance
varImp(fit)

## Repeat with cross validation

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Train the model
set.seed(25)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1:(ncol(tmpData)-1)))
fit <-  train(y ~ ., data = tmpData, method = "rf", ntree = 150,
              trControl = control, tuneGrid = grid)

# Store this model for later use
fit.rf.stored <- fit$finalModel

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_rf.cv = predict(fit, newdata = change.16.20.test))

# RMSE
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_rf.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_rf.cv,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("rf.cv"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))

# Variable importance after cross validation
varImp(fit$finalModel)

# Show results
Results

# Parameter selected by cross validation is
fit$bestTune


#------------------------------------------------------------------------
# Extreme gradient boosting
#------------------------------------------------------------------------

# Tutorial: k <https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html>

# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Training:
dtrain <- xgb.DMatrix(data = as.matrix(tmpData[,-1]), label=tmpData$y)
dtest <- xgb.DMatrix(data = as.matrix(change.16.20.test[,featureList][,-1]), label=change.16.20.test$y)
watchlist <- list(train=dtrain, test=dtest)

# Train the model
set.seed(51)
fit <- xgb.train(data=dtrain, max.depth=3, eta=1, nthread = 2, nrounds=4, watchlist=watchlist, objective = "reg:squarederror",verbose = 0) # "reg:squaredlogerror")

# Predict with the final model
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_xgb.cv = predict(fit, newdata = dtest))

## RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_xgb.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_xgb.cv,yAct = y,weight = TotalPop))

## Results
Results <- Results %>% bind_rows(data.frame(model = c("xgb.cv"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))

# Variable importance:
importance_matrix <- xgb.importance(model = fit)
xgb.plot.importance(importance_matrix = importance_matrix)

# Results 
Results


#------------------------------------------------------------------------
# KNN
#------------------------------------------------------------------------

# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Fit the model
fit <- knnreg(y ~ .,data = tmpData)
# fit.weighted <- knnreg(y ~ .,data = tmpData,weights = tmpData$TotalPop)

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_knnreg = predict(fit, newdata = change.16.20.test))

# RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_knnreg,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_knnreg,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% bind_rows(data.frame(model = c("knnreg"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))

## Repeat with cross validation

# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20.train %>%
  select(starts_with(featureList))

# Fit the model
set.seed(100)
fit <- train(y ~ .,method = "knn", data = tmpData,
             tuneGrid = data.frame(k = seq(15, 71, 2)))
# fit.weighted <- train(y ~ .,weights = tmpData$TotalPop, method = "knn", data = tmpData,
#                    tuneGrid = data.frame(k = seq(15, 71, 2)))

# Predict
change.16.20.test <- change.16.20.test %>%
  mutate(y_hat_knnreg.cv = predict(fit, newdata = change.16.20.test))

## RMSEs
thisRMSE <- with(change.16.20.test, RMSE(yPred = y_hat_knnreg.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20.test, RMSE.weighted(yPred = y_hat_knnreg.cv,yAct = y,weight = TotalPop))

## Results
Results <- Results %>% bind_rows(data.frame(model = c("knn.cv"),
                                            RMSE = thisRMSE,
                                            RMSE.weighted = thisRMSE.weighted))
# Results
Results



#------------------------------------------------------------------------
# Final model choice and fit on the whole set
#------------------------------------------------------------------------

# Define the set of features to be investigated
featureList <- c("y","lat","long", "TotalPop", "Delta.16.rel","TurnoutRate.16",
                 "Black.pct","Hispanic.pct","Asian.pct","Native.pct","Pacific.pct",
                 "Women.pct","Employment.pct","Construction.pct")

# Select these features
tmpData <- change.16.20 %>%
  select(starts_with(featureList) & ends_with(featureList))

# Train the model
set.seed(200)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1:(ncol(tmpData)-1)))
fit <-  train(y ~ ., data = tmpData, method = "rf", ntree = 150,
              trControl = control, tuneGrid = grid)

# Predict
change.16.20 <- change.16.20 %>%
  mutate(y_hat_rf.cv = predict(fit, newdata = change.16.20))

# RMSE
thisRMSE <- with(change.16.20, RMSE(yPred = y_hat_rf.cv,yAct = y))
thisRMSE.weighted <- with(change.16.20, RMSE.weighted(yPred = y_hat_rf.cv,yAct = y,weight = TotalPop))

# Results
Results <- Results %>% 
  bind_rows(data.frame(model = c("final.rf.cv (in sample)"),
                       RMSE = thisRMSE,
                       RMSE.weighted = thisRMSE.weighted))

# Store as the final model fit
fit.final.rf.cv <- fit

# Variable importance
varImp(fit)

# Results
Results


#------------------------------------------------------------------------
# Prediction for the validation set
#------------------------------------------------------------------------

# Add columns needed for the prediction:
validation.extended <- validation %>%
  mutate(Delta.20.rel = (votes.DEM.20 - votes.REP.20)/TotalPop,
         Delta.16.rel = (votes.DEM.16 - votes.REP.16)/TotalPop)

# Predict the nation-wide trend using the 'baseline' fit:
y_hat_baselineVal <- predict(fitLoessBaseline,newdata = validation.extended)

# Extend validation set with extra columns including the 'baseline' fit:
validation.extended <- validation.extended %>%
  mutate(Delta.20.rel.baseline = y_hat_baselineVal,
         residual.16.20 = Delta.20.rel - Delta.20.rel.baseline,
         y = residual.16.20,
         White.pct = White/100,
         Black.pct = Black/100,
         Hispanic.pct = Hispanic/100,
         Asian.pct = Asian/100,
         Native.pct = Native/100,
         Pacific.pct = Pacific/100,
         Women.pct = Women/TotalPop,
         Employment.pct = Employed/TotalPop,
         Construction.pct = Construction/TotalPop,
         TurnoutRate.16 = votes.TOT.16/TotalPop)

# Use the trained model to make the predictions
validation.extended <- validation.extended %>%
  mutate(y_hat.residual.final = predict(fit.final.rf.cv, newdata = validation.extended),
         base.pred.Delta.20.abs = Delta.20.rel.baseline * TotalPop,
         pred.Delta.20.rel = Delta.20.rel.baseline + y_hat.residual.final,
         pred.Delta.20.abs = pred.Delta.20.rel * TotalPop,
         actual.Delta.20.abs = votes.DEM.20 - votes.REP.20)

# Calculate RMSE
thisRMSE <- with(validation.extended, RMSE(yPred = y_hat.residual.final,yAct = y))
thisRMSE.weighted <- with(validation.extended, RMSE.weighted(yPred = y_hat.residual.final,
                                                             yAct = y,weight = TotalPop))

# Add the results tot the result table
Results <- Results %>%
  bind_rows(data.frame(model = c("final (out of sample)"),
                       RMSE = thisRMSE, RMSE.weighted = thisRMSE.weighted))

# Results
Results


#------------------------------------------------------------------------
# Evaluation on the state level
#------------------------------------------------------------------------

StateResults <- validation.extended %>%
  group_by(state) %>%
  summarize(pred.abs.spread.20 = sum(pred.Delta.20.abs),
            actual.abs.spread.20 = sum(votes.DEM.20 - votes.REP.20),
            pred.DEM.won = pred.abs.spread.20 > 0,
            actual.DEM.won = actual.abs.spread.20 > 0)

# The result is
StateResults


#------------------------------------------------------------------------
# Compare to state aggregated regression
#------------------------------------------------------------------------

# Aggregate by state and calculate differences
election_state_aggregation <- election %>%
  group_by(state) %>%
  summarize(DEM.20 = sum(votes.DEM.20),
            REP.20 = sum(votes.REP.20),
            DEM.16 = sum(votes.DEM.16),
            REP.16 = sum(votes.REP.16),
            TotPop = sum(TotalPop)) %>%
  mutate(diff.16 = (DEM.16-REP.16),
         diff.20 = (DEM.20-REP.20))

# Plot
election_state_aggregation %>%
  ggplot(aes(diff.16,diff.20,size=TotPop,weight = TotPop,label=state)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE,color="orange") +
  geom_text_repel() +
  xlab("Absolute vote difference 2016") +
  ylab("Absolute vote difference 2020")

# Apply linear regression
stateRegressionModel <- lm(diff.20 ~ diff.16,data = election_state_aggregation,weights = TotPop)
# summary(stateRegressionModel)

# Prepare aggregated validation set:
validation_state_aggregation <- validation %>%
  group_by(state) %>%
  summarize(DEM.20 = sum(votes.DEM.20),
            REP.20 = sum(votes.REP.20),
            DEM.16 = sum(votes.DEM.16),
            REP.16 = sum(votes.REP.16),
            TotPop = sum(TotalPop)) %>%
  mutate(diff.16 = (DEM.16-REP.16),
         diff.20 = (DEM.20-REP.20))

# Predict outcome of the validation set:
predValidation <- predict(stateRegressionModel,newdata = validation_state_aggregation)

# Append prediction to data set and display
validation_state_aggregation <- validation_state_aggregation %>%
  mutate(pred.diff.20 = predValidation,
         act.diff.20 = diff.20,
         pred.DEM.won = pred.diff.20 > 0,
         actual.DEM.won = act.diff.20 > 0) %>%
  select(state,pred.diff.20, act.diff.20, pred.DEM.won, actual.DEM.won)


# Result (similar to the county analysis) 
validation_state_aggregation


#########################################################################
# Part 4: Analysis of the result
#########################################################################

#------------------------------------------------------------------------
# Actual versus predicted spreads (relative and absolute)
#------------------------------------------------------------------------

# Plot actual versus predicted RELATIVE spreads
validation.extended %>%
  ggplot(aes(pred.Delta.20.rel,Delta.20.rel)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  geom_abline(slope = 1,intercept = qnorm(0.001) * Results$RMSE[14],color="red",lty=3) +
  geom_abline(slope = 1,intercept = qnorm(0.999) * Results$RMSE[14],color="red",lty=3) +
  xlab("Predicted relative spread") +
  ylab("Actual relative spread")

# The lines with the 0.1% and 99.9% quantiles shall indicate the 'expected' range
# If there are too many points outside the dotted line or far outside => 'outlier'

# Plot actual versus predicted ABSOLUTE spreads
validation.extended %>%
  ggplot(aes(pred.Delta.20.abs,actual.Delta.20.abs)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  xlab("Predicted absolute spread") +
  ylab("Actual absolute spread")

#------------------------------------------------------------------------
# Drill-down for the outlier
#------------------------------------------------------------------------

# Determine the index of the outlier
tmpIndex <- validation.extended %>%
  mutate(abs.dev = actual.Delta.20.abs - pred.Delta.20.abs) %>%
  pull(abs.dev) %>%
  which.min()

# Show data for the outlier
validation.extended[tmpIndex,] %>%
  select(state,county,votes.DEM.16,votes.REP.16,votes.DEM.20,votes.REP.20,actual.Delta.20.abs,pred.Delta.20.abs) %>%
  rename(DEM.16 = votes.DEM.16,
         REP.16 = votes.REP.16,
         DEM.20 = votes.DEM.20,
         REP.20 = votes.REP.20,
         actual.Delta = actual.Delta.20.abs,
         predicted.Delta = pred.Delta.20.abs)


#------------------------------------------------------------------------
# Drill-down Florida
#------------------------------------------------------------------------

# Plot Florida county spreads
validation.extended %>%
  filter(state == "FL") %>%
  mutate(Delta.16.abs = votes.DEM.16 - votes.REP.16,
         Delta.20.abs = votes.DEM.20 - votes.REP.20) %>%
  ggplot(aes(Delta.16.abs,Delta.20.abs,size=TotalPop,color=Hispanic,label = county)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  geom_text_repel()


#------------------------------------------------------------------------
# Drill-down Georgia
#------------------------------------------------------------------------

# Plot Georgia county spreads
validation.extended %>%
  filter(state == "GA") %>%
  ggplot(aes(pred.Delta.20.abs,actual.Delta.20.abs,color=Black.pct,size = TotalPop,label=county)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  geom_text_repel()

#------------------------------------------------------------------------
# Drill-down Arizona
#------------------------------------------------------------------------

# Plot Arizona county spreads
validation.extended %>%
  filter(state == "AZ") %>%
  ggplot(aes(pred.Delta.20.abs,actual.Delta.20.abs,size = TotalPop,label=county)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  geom_text_repel()


#------------------------------------------------------------------------
# Drill-down Pennsylvania
#------------------------------------------------------------------------

# Plot Pennsylvania county spreads
validation.extended %>%
  filter(state == "PA") %>%
  ggplot(aes(pred.Delta.20.abs,actual.Delta.20.abs,size = TotalPop,label=county)) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0,color="red") +
  geom_text_repel()


#------------------------------------------------------------------------
# Result overview
#------------------------------------------------------------------------

# Plot changes not predicted by the model
p <- validation.extended %>%
  mutate(unexplained.diff = actual.Delta.20.abs - pred.Delta.20.abs) %>%
  arrange(abs(unexplained.diff)) %>%
  filter(!(state %in% c("AK","HI")) & long != 0 & lat != 0) %>%
  ggplot() +
  geom_polygon(data = state_map,aes(long,lat,group = group),fill="grey",color="black") +
  geom_point(aes(long,lat,color=unexplained.diff,size=TotalPop)) +
  geom_polygon(data = state_map,aes(long,lat,group = group),alpha = 0, color="black") +
  scale_color_gradient2(low="red",midpoint = 0.0,high = "blue") + # coord_fixed() +
  ggtitle("Changes unexplained by the model") +
  xlab("Longitude (in degrees)") +
  ylab("Latitude (in degrees)")

print(p)
