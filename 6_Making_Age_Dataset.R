
#In this file: Making the age split dataset
#Requires - detailed estimates dataset


#Works from original UK Detailed Estimates across multiple files. 

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr", "dplyr", "stringr", "sf")


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




#Set directory
Path = getwd()


#Import in Detailed Estimate Files (can be plural)
Clean = function(myfiles){ #read in files, categorise by ages, group by these categories to summarise moves between each OD pair, add year
  read.csv(myfiles)%>%
    mutate(., Cat = ifelse(Age %in% 0:15, "Age_0_to_15",
                           ifelse(Age %in% 16:19, "Age_16_to_19",
                                  ifelse(Age %in% 20:24, "Age_20_to_24",
                                         ifelse(Age %in% 25:29, "Age_25_to_29",
                                                ifelse(Age %in% 30:44, "Age_30_to_44",
                                                       ifelse(Age %in% 45:59, "Age_45_to_59",
                                                              ifelse(Age %in% 60:200,"Age_60_Plus",NA)))))))) %>%
    group_by(OutLA,InLA, Cat) %>% 
    summarize(Moves = sum(Moves))
  
}


#Find files
myfiles = list.files(path=(file.path(paste0(Path, "/Final_Data/Detailed_Estimates"))),pattern=".csv", full.names=TRUE)

#Import the data - this is only for 2019
Mig_2019<- ldply(myfiles[c(18,19)],Clean)



#Replace any missing LADs - it is important that the connections with 0 moves are recorded, so these need to be added

Ages = data.frame(unique(Mig_2019$Cat))
LAD_List = data.frame(unique(Mig_2019$OutLA))%>%rename(code = 1)

# For each year, list the unique LADs, and add any, with 0 moves, that are missing 
#This needs to be done for both incoming and outgoing flows
for (y in c(1:nrow(Ages))){
  Y = filter(Mig_2019,Cat == Ages[y,1])
  for (i in c(1:nrow(LAD_List))){
    X = filter(Y,OutLA == LAD_List[i,1])
    
    Missing_In = filter(LAD_List, !(code %in% X$InLA))
    
    if(nrow(Missing_In != 0)){
      replacements = data.frame(OutLA = as.character(LAD_List[i,1]), InLA = Missing_In$code, Moves = 0, Cat = as.character(Ages[y,1]))
      Mig_2019 = rbind(Mig_2019, replacements)
    }
  }
  print(paste( Ages[y,1], "done"))
}



for (y in c(1:nrow(Ages))){
  Y = filter(Mig_2019,Cat == Ages[y,1])
  for (i in c(1:nrow(LAD_List))){
    X = filter(Y,InLA == LAD_List[i,1])
    
    Missing_Out = filter(LAD_List, !(code %in% X$OutLA))
    
    if(nrow(Missing_Out != 0)){
      replacements = data.frame(InLA = as.character(LAD_List[i,1]), OutLA = Missing_Out$code, Moves = 0, Cat = as.character(Ages[y,1]))
      Mig_2019 = rbind(Mig_2019, replacements)
    }
  }
  print(paste( Ages[y,1], "done"))
}

#Remove orig = dest
Mig_2019 = filter(Mig_2019, InLA != OutLA)

rm(list=setdiff(ls(),c("Path", "Mig_2019")))

#Get a list of LAD names to check
LAD_List = sf::st_read(dsn = (paste0(Path, "/SHAPES")),layer="LAD_Map")%>%
  st_drop_geometry()

#Changing LAD codes to LAD names  
Mig_2019<- Mig_2019 %>%
  merge(., LAD_List, by.x = "InLA", by.y = "Code")%>%
  filter(.,!is.na(Moves))%>%
  mutate(InLA = LAD)%>%
  select(-"LAD")%>%
  merge(., LAD_List, by.x = "OutLA", by.y = "Code")%>%
  mutate(OutLA = LAD)%>%
  select(-"LAD")%>%
  filter(.,!is.na(Moves))



write.csv(Mig_2019, "Paper_Merged_Ages_2019.csv")


