##In this file - 
##Are moves within regions/to neighbours

##Must have a dataset with columns Orig - Dest - Moves - Year, or make in 1_Cleaning_Full_Data.R


#Set the path
Path = getwd()


## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","dplyr","sf", "rgeos", "stplanr")
             #, "stringr","spdep", "rgeos", "stplanr")


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




#Network analysis
#Read in Map and Moves
Map = sf::st_read(dsn = (paste0(getwd(), "/SHAPES")),layer="LAD_Map")%>%rename(geo_code = LAD)%>%select(c("geometry", "geo_code"))%>%
  mutate(ID = c(1:(nrow(.))))
Moves = read.csv("Paper_Merged_2019.csv")%>%select(-1)%>%rename(orig = InLA,dest = OutLA)

#Identify polygons in the map that are neighbours. These need rownames and colnames adding. Use ST planer to turn into dataframe
list.nb <- gTouches(as_Spatial(Map), byid = TRUE, returnDense = TRUE)
rownames(list.nb)<-Map$geo_code
colnames(list.nb)<-Map$geo_code
list.nb = odmatrix_to_od(list.nb)


#Add column whether moves are to neighbours/in region/other
Moves = read.csv("Paper_Merged_2019.csv")%>%select(-1)%>%rename(orig = InLA,dest = OutLA)%>%
  merge(.,list.nb, by = c("orig", "dest"))%>%
  rename(Neighbours = flow)%>%
  merge(., read.csv(paste0(Path, "/Final_Data/Region_Lookup.csv"))%>%select(-1), 
        by.x = "orig", by.y= "LAD",all = T)%>%
  select(c("orig", "dest", "Moves", "Neighbours", "Reg"))%>%
  rename(Orig_Reg = Reg)%>%
  merge(., read.csv(paste0(Path, "/Final_Data/Region_Lookup.csv"))%>%select(-1), by.x = "dest", by.y= "LAD",all = T)%>%
  rename(Dest_Reg = Reg)%>%
  select(-"Reg_Code")%>%
  select(c("orig", "dest", "Moves", "Neighbours", "Orig_Reg","Dest_Reg"))%>%
  mutate(.,Reg_Int = ifelse(Orig_Reg == Dest_Reg, "TRUE", "FALSE"),
         Other = ifelse(Neighbours !=TRUE & Reg_Int !=TRUE, "TRUE", "FALSE"))
  



#Code to calculate for specific places
Deets = filter(Moves, dest=="Neath Port Talbot")

#How many moves are to neighbours?
sum(filter(Deets, Neighbours == TRUE)[,"Moves"])/sum(Deets[,"Moves"])*100

#How many moves are in region?
sum(filter(Deets, Reg_Int == TRUE)[,"Moves"])/sum(Deets[,"Moves"])*100

Reg_Int = filter(Moves, Reg_Int == TRUE)%>%
  select(Orig_Reg, Moves)%>%
  group_by(Orig_Reg)%>%
  summarise(Moves = sum(Moves))
  
  
  



