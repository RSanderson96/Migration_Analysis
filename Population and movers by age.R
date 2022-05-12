
#In this file: Making table to see population and moves information by age
#Requires - O-D movers dataset (see #6 Making_Age_Dataset) and Population 


#Works from original UK Detailed Estimates across multiple files. 

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr", "dplyr", "kableExtra")#, "stringr")


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

#Information: What is the population/number of movers for each group?
Groups = read.csv("Paper_Merged_Ages_2019.csv")%>%group_by(Cat)%>%summarise(Moves = sum(Moves))

#This cleans the population based on the CSV file available - UK data has commas
Clean_Pop = function(x){
  as.numeric(gsub(",","",x))
}



Population = read.csv(paste0(Path, "/Final_Data/Pop_2019.csv"))%>%
  rename(Code = 1, LAD = 2, geography = 3)%>%
  filter(., LAD %in% LAD_List[, "LAD"])%>%
  select(c(5:95))%>%
  lapply(., Clean_Pop)%>%
  data.frame()%>%
  mutate(Age_0_to_15 = select(., X0:X15) %>% rowSums(na.rm = TRUE),
         Age_16_to_19 = select(., X16:X19) %>% rowSums(na.rm = TRUE),
         Age_20_to_24 = select(., X20:X24) %>% rowSums(na.rm = TRUE),
         Age_25_to_29 = select(., X25:X29) %>% rowSums(na.rm = TRUE),
         Age_30_to_44 = select(., X30:X44) %>% rowSums(na.rm = TRUE),
         Age_45_to_59 = select(., X45:X59) %>% rowSums(na.rm = TRUE),
         Age_60_Plus = select(., X60:X90.) %>% rowSums(na.rm = TRUE))%>%
  select(c(92:98))%>%
  mutate(Total = rowSums(.,na.rm = TRUE))%>%
  t()%>%
  data.frame()%>%
  mutate(Population = rowSums(.,na.rm = TRUE))%>%
  select("Population")%>%
  mutate(Cat = row.names(.))%>%
  merge(Groups, .)%>%
  #mutate(Cohort = c("0 - 15 years", "16 - 19 years", "20 - 24 years", "25 - 29 years", "30 - 44 years", "45 - 59 years", "60 + years"))%>%
  mutate(.,Cat = Cat%>% gsub("_"," ",.),
         Moves_Percentage  = round((Moves/sum(Moves)*100),digits = 2),
         Population_Percentage = round((Population/sum(Population)*100),digits = 2))%>%
  select(c("Cohort" = Cat, 
           #"Cohort", 
           "Population", 
           "Population Percentage" = Population_Percentage,
           "Moves",
           "Moves Percentage" = Moves_Percentage
           ))%>%
  mutate("Population (%)" = paste0(Population, " (",`Population Percentage`, "%)"),
         "Migration (%)" = paste0(round(Moves,2), " (",`Moves Percentage`, "%)"))%>%
  select(-c("Population", "Population Percentage", "Moves", "Moves Percentage"))


  
  
  
Caption = "Population data by age group, 2019"
Table = Population

Table %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Table),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")



