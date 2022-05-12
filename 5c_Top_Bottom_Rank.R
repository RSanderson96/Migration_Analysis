#In this file: Visualisations for Final_Gini Dataset

#Requires - CSV of gini index scores for each small area being compared.
#Need shapefiles to make maps

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","tidyverse","dplyr","kableExtra")

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



#Import Gini Index Data
Full_Gini = read.csv("Full_Gini.csv")%>%select(-1)

#Set N - how many LAs you want to see
n = 10


Most_Dispersed_In = Full_Gini%>%
  select(c("LAD", "Gini_In"))%>%
  arrange(-Gini_In)%>%
  mutate(Rank = c(1:nrow(.)))%>%
  filter(., Rank%in%c((nrow(.)-(n-1)):(nrow(.))))%>%
  mutate(Rank = rev(c(1:n)))%>%
  arrange(-Rank)%>%
  rename(In=LAD)%>%
  mutate(In = paste0(In, " (",round(Gini_In,3), ")"))%>%
  select(-"Gini_In")



Most_Dispersed_Out = Full_Gini%>%
  select(c("LAD", "Gini_Out"))%>%
  arrange(-Gini_Out)%>%
  mutate(Rank = c(1:nrow(.)))%>%
  filter(., Rank%in%c((nrow(.)-(n-1)):(nrow(.))))%>%
  #select(-"Gini_Out")%>%
  mutate(Rank = rev(c(1:n)))%>%
  arrange(-Rank)%>%
  rename(Out = LAD)%>%
  mutate(Out = paste0(Out, " (",round(Gini_Out,3), ")"))%>%
  select(-"Gini_Out")

Dispersed = merge(Most_Dispersed_In,Most_Dispersed_Out, by = "Rank")%>%
  arrange(-Rank)
rm(Most_Dispersed_In,Most_Dispersed_Out)


Most_Focused_In = Full_Gini%>%
  select(c("LAD", "Gini_In"))%>%
  arrange(Gini_In)%>%
  mutate(Rank = rev(c(1:nrow(.))))%>%
  filter(., Rank%in%c(1:n))%>%
  arrange(Rank)%>%
  rename(In = LAD)%>%
  mutate(In = paste0(In, " (",round(Gini_In,3), ")"))%>%
  select(-"Gini_In")

Most_Focused_Out = Full_Gini%>%
  select(c("LAD", "Gini_Out"))%>%
  arrange(Gini_Out)%>%
  mutate(Rank = rev(c(1:nrow(.))))%>%
  filter(., Rank%in%c(1:n))%>%
  arrange(Rank)%>%
  rename(Out = LAD)%>%
  mutate(Out = paste0(Out, " (",round(Gini_Out,3), ")"))%>%
  select(-"Gini_Out")

Focused = merge(Most_Focused_In,Most_Focused_Out, by = "Rank")
rm(Most_Focused_In,Most_Focused_Out)


Rankings = Focused%>%
  bind_rows(set_names(rep(NA, ncol(.)), colnames(.)))%>%
  rbind(.,Dispersed)%>%
  mutate(" " = " ")%>%
  mutate(Category = " ")%>%
  select(c("Category", "Rank", "In", " ", "Out"))%>%
  replace(is.na(.), " ")


Rankings[c(1,((2*n)+1)), "Category"]<-c("Most Focused", "Most Dispersed")

Caption = "Most Focused and Most Dispersed Migration Fields, 2019"
Table = Rankings

Table %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Table),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")








