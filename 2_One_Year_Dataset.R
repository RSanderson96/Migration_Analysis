##In this file - 
##selecting 2019, replacing codes with names
##Moves within regions/to neighbours

##Must have a dataset with columns Orig - Dest - Moves - Year, or make in 1_Cleaning_Full_Data.R


#Set the path
Path = getwd()


## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","dplyr","purrr","sf", "stringr","spdep", "rgeos", "stplanr")


## Now load or install&load all
package.check <- lapply(
  Packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


####Cleaning the datsaet - UK context

#Read in the dataset - remove Scot/NI
Moves_2019 = read.csv("Paper_Merged_All_Years.csv")%>%
  select(-1)%>%
  filter(., Year == "2019")%>% 
  select(-"Year")%>%
  filter(!str_detect(.$OutLA, "S"))%>%
  filter(!str_detect(.$OutLA, "N"))%>%
  filter(!str_detect(.$InLA, "S"))%>%
  filter(!str_detect(.$InLA, "N"))


#Get a list of LAD names to check
LAD_List = st_drop_geometry(st_read(dsn = (paste0(getwd(), "/SHAPES")),layer="LAD_Map"))

#Changing LAD codes to LAD names  
Moves_LAD<-Moves_2019%>%
  merge(., LAD_List, by.x = "InLA", by.y = "Code", all = T)%>%
  filter(.,!is.na(Moves))%>%
  mutate(InLA = LAD)%>%
  select(-"LAD")%>%
  merge(., LAD_List, by.x = "OutLA", by.y = "Code", all = T)%>%
  mutate(OutLA = LAD)%>%
  select(-"LAD")%>%
  filter(.,!is.na(Moves))

#Makes the CSV files with either LAD codes or LAD names
write.csv(Moves_LAD, "Paper_Merged_2019.csv")



  



