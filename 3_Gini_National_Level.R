##In this file - 
##Calculate Gini Index for all years

#Input - table of orig-dest migration for all years (see #1_Cleaning_Full_Data)
#Output - table of gini scores for each year


## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","dplyr","reshape2","sf","stringr", "migration.indices", "kableExtra")



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


Path = getwd()

#Import dataset: All moves, all years
All_Years = read.csv("Paper_Merged_All_Years.csv")%>%select(-1)%>%
  filter(!str_detect(.$OutLA, "S"))%>%
  filter(!str_detect(.$OutLA, "N"))%>%
  filter(!str_detect(.$InLA, "S"))%>%
  filter(!str_detect(.$InLA, "N"))


#Calculating full table index for each year
Years = unique(All_Years$Year)
Gini_Whole = vector()

for (i in 1:length(Years)){
  OD_Matrix = All_Years%>%filter(.,Year == Years[i])%>%select(-"Year")
  #Make a matrix - Rows are out, columns are in
  OD_Matrix<- acast(OD_Matrix, OutLA ~ InLA, sum)
  OD_Matrix[is.na(OD_Matrix)]<-0
  diag(OD_Matrix)<-NA
  Gini_Whole[i] = migration.gini.total(OD_Matrix, corrected = FALSE)
}

#Making the table of yearly gini indices
Yearly_gini = data.frame(Years = Years, Gini = Gini_Whole)%>%
  mutate(Years = ordered(as.numeric(Years)))%>%
  arrange(., Years)%>%
  mutate(Gini = summarise(., Gini = round(as.numeric(Gini), digits = 3)))%>%
  t()%>%
  as.data.frame()
names(Yearly_gini) <- Yearly_gini[1,]
Yearly_gini<- Yearly_gini[-1,]


Table = Yearly_gini
Caption = "Gini Index 2011 - 2020"


Table %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Table),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")


