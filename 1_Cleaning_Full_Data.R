##In this file:
##-Extracting the data
##-Making all flows
##Saving final CSV


#Set Path

Path = getwd()


## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
Packages = c("plyr","dplyr","stringr")


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




#Select files - create a list of the files that you want to import

myfiles = list.files(path=(file.path(paste0(Path, "/Final_Data/Detailed_Estimates"))),pattern=".csv", full.names=TRUE)


#Import in Detailed Estimate Files (function - group by orig-dest pairs summarise all moves, make year column)
Clean = function(myfiles){
  read.csv(myfiles)%>%
    group_by(OutLA,InLA) %>% 
    summarize(Moves = sum(Moves))%>%
    mutate(Year =  str_extract(gsub(paste0(Path, "/Final_Data/Detailed_Estimates"), "", myfiles), "[0-9+]{1,4}"))
}


#Import and clean the data
Data = ldply(myfiles,Clean)

#Making the merged file - it is important that the connections with 0 moves are recorded, so these need to be added
Years = data.frame(unique(Data$Year))%>%rename(Year = 1)

# For each year, list the unique LADs, and add any, assigning 0 moves, that are missing 
#This needs to be done for both incoming and outgoing flows
for (y in c(1:nrow(Years))){
  Y = filter(Data,Year == Years[y,1])
  LAD_List = data.frame(unique(Y$OutLA))%>%rename(code = 1)
  for (i in c(1:nrow(LAD_List))){
    
    X = filter(Y,OutLA == LAD_List[i,1])
    
    Missing_In = filter(LAD_List, !(code %in% X$InLA))
    
    if(nrow(Missing_In != 0)){
      replacements = data.frame(OutLA = as.character(LAD_List[i,1]), InLA = Missing_In$code, Moves = 0, Year = as.character(Years[y,1]))
      Data = rbind(Data, replacements)
    }
  }
  print(paste( Years[y,1], "done"))
}

for (y in c(1:nrow(Years))){
  Y = filter(Data,Year == Years[y,1])
  LAD_List = data.frame(unique(Y$InLA))%>%rename(code = 1)
  
  for (i in c(1:nrow(LAD_List))){
    X = filter(Y,InLA == LAD_List[i,1])
    
    Missing_Out = filter(LAD_List, !(code %in% X$OutLA))
    
    if(nrow(Missing_Out != 0)){
      replacements = data.frame(InLA = as.character(LAD_List[i,1]), OutLA = Missing_Out$code, Moves = 0, Year = as.character(Years[y,1]))
      Data = rbind(Data, replacements)
    }
  }
  print(paste( Years[y,1], "done"))
}

#Finally, remove any rows where the origin matches the destination
Data = filter(Data, InLA != OutLA)

#Output - Merged file!
write.csv(Data, paste0(Path,"/Paper_Merged_All_Years.csv"))

