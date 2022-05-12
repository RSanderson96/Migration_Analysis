##In this file:
##-Making missing files
##-Making LAD files

#Set the path
Path = getwd()


## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.
packages = c("plyr", "dplyr","purrr","sf", "stringr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



#Read in Scotland/Northern Ireland
Countries = sf::st_read(dsn = (paste0(Path, "/Final_Data/Countries")),layer="Countries_(December_2020)_UK_BFC")%>%
  select(c(2,3))%>%
  rename(code = 1, name = 2)%>%
  filter(!str_detect(.$code, "E"))%>%
  filter(!str_detect(.$code, "W"))

#This makes the 'missing' parts of the UK
st_write(Countries, dsn = paste0(Path, "/SHAPES/Missing_Map.shp"))

#Read in required LAD boundaris
LAD = sf::st_read(dsn = (paste0(Path, "/Final_Data/LAD")),layer="LAD_2019")%>%
  select(c(2,3))%>%
  rename(Code = 1, LAD = 2)%>%
  filter(!str_detect(.$Code, "S"))%>%
  filter(!str_detect(.$Code, "N"))

#Write included LAD file
st_write(LAD, dsn = paste0(Path,"/SHAPES/", "LAD_Map.shp"))

