#In this file: Decomposition at national level
#Need O-D information by sub-population group (made in #6_Making_Age_Dataset)

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","dplyr","reshape2","migration.indices", "kableExtra")


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



#Read in the dataset - the seperated by ages file for 2019
Age_Data = read.csv("Paper_Merged_Ages_2019.csv")%>%
  select(-1)%>%
  rename(Out = 1, In = 2, Cat = 3, Moves = 4)


#Function: Making a table of the decomposition. 
#Input: Table with columns Out, In, Cat, Moves (as made above)

Decomposition_Table = function(DF, Category){
  Categories = sort(unique(DF[,Category]))
  
  ##Decomposition table
  All_Moves = DF%>%
    group_by(Out, In)%>%
    summarise(All_Moves = sum(Moves))%>%
    arrange(., by_group = All_Moves)
  All_Moves$All_Rank = rank(All_Moves$All_Moves)
  
  Matrix = All_Moves%>%select(c("In", "Out", "All_Moves"))%>%acast(., Out ~ In, sum)
  Matrix[is.na(Matrix)]<-0
  #diag(Matrix)<-0
  G_Total= migration.gini.total(Matrix, corrected = FALSE)
  
  Decompose = function(Categories){
    DF_Cut = DF%>%filter(.,Cat == Categories)%>%group_by(Out, In)%>%arrange(., by_group = Moves)%>%rename(Cat_Moves = Moves)
    #calculate S_k  
    S_k = sum(DF_Cut$Cat_Moves)/sum(All_Moves$All_Moves)
    
    #calculate RK
    DF_Cut$Cat_Rank = rank(DF_Cut$Cat_Moves)
    DF_Cut = merge(DF_Cut, All_Moves, by = c("In", "Out"), all = T)
    R_k = cov(DF_Cut$Cat_Moves, DF_Cut$All_Rank)/cov(DF_Cut$Cat_Moves, DF_Cut$Cat_Rank)
    
    #Calculate Gk
    OD_Matrix = DF%>%filter(.,Cat == Categories)%>%select(-c("Cat"))%>% acast(., Out~ In, sum)
    OD_Matrix[is.na(OD_Matrix)]<-0

    #Calculate overall gini stats
    G_k = migration.gini.total(OD_Matrix, corrected = FALSE)
    
    #Multiply to get the total score
    T_k = (R_k*G_k*S_k)
    
    #Score as a proportion of the total Gini Index
    Share = (R_k*G_k*S_k)/G_Total
    
    Decom = c(S_k, R_k, G_k, T_k,Share)
    
    return(Decom)
  }  
  
  
  Decom_Results = data.frame(Index = c("S_k", "R_k", "G_k","T_k","Share"))  
  Decom_Results[,c(2:(1+(length(Categories))))] = lapply(Categories,Decompose)
  rownames(Decom_Results) <- Decom_Results[,1]
  Decom_Results = Decom_Results[,-1]%>%
    round(., digits  = 3)
  names(Decom_Results)<-Categories
  
  return(Decom_Results)
}
 
Table = Decomposition_Table(Age_Data, "Cat")


#Aesthetic improvements - eg cohort names
Table_Cleaned = Table[-4,]%>%
  rename(., "Age 0 - 15" = 1,
         "Age 16 - 19" = 2,
         "Age 20 - 24" = 3,
         "Age 25 - 29" = 4,
         "Age 30 - 44" = 5,
         "Age 45 - 59" = 6,
         "Age 60+" = 7)%>%
  t()%>%
  data.frame()%>%
  select(c("S_k", "G_k", "R_k", "Share"))%>%
  rename(Income_Share = S_k, Correlation_with_Total_Rank = R_k, Gini_of_Source = G_k, Share_of_Inequality = Share)%>%
  mutate(Relative_Income_Inequality = Share_of_Inequality/Income_Share,
         Relative_Marginal_Effect =Share_of_Inequality - Income_Share,
         Income_Share = Income_Share*100,
         Share_of_Inequality = Share_of_Inequality*100)%>%
  round(., digits  = 3)%>%
  rename(., "Share of Migrants (Sk, %)" = 1,
         "Gini Correlation Coefficient (Rk)" = 2,
         "Gini Index (Gk)" = 3,
         "Focusing Share (Fk, %)" = 4,
         "Relative Inequality" = 5,
         "Relative Marginal Effect" = 6)
  
         

Caption = "National Gini Decomposition by Age: 2019"


Table_Cleaned %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Table_Cleaned),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")



