
#In this file: Decomposition at local level
#Requires - table of O-D by age cohort (#6_Making_Age_Dataset),
# and Full Gini Scores (Gini National_2019)

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","dplyr","reshape2","migration.indices", "tidyr", "stringr", "matrixStats", "tibble", "kableExtra")


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



#Dataframe with categories Out, In, Cat and Moves
Age_In_Out = read.csv("Paper_Merged_Ages_2019.csv")%>%
  select(-1)%>%
  #mutate(Cat = factor(Cat, levels =c("School", "University", "Early_Work", "Family_Starters", "Family_Raising","Older_Working", "Retirement")))%>%
  rename(Out = 1, In = 2, Cat = 3, Moves = 4)

#Dataframe with full gini scores - LAD, In, Out
Full_Gini = read.csv("Full_Gini.csv")%>%select(-1)%>%rename(In = 2, Out = 3)


#####Calculating the inequality of flows IN TO an LAD

#Sk - how many of each age group, as a proportion of the LAD
  #Calculate Sk_IN

#Inwards: How many migrants are received by each place in total
Sk_In = DF%>%group_by(In, Cat) %>%
  summarise(Cat_Moves = sum(Moves))%>%
  spread(., key = Cat, value = Cat_Moves)%>%
  as.data.frame()
rownames(Sk_In)<-Sk_In[,1]
Sk_In = Sk_In[,-1]

Sk_In$Sums = rowSums(Sk_In)

#Proportion - number of migrants arriving from each category, divided by number overall for that origin
for (i in c(1:ncol(Sk_In))){
  Sk_In[,i] = Sk_In[,i]/Sk_In$Sums
}

Sk_In = Sk_In %>% select(-Sums)
#Sk_In$Sums = rowSums(Sk_In[,1:7])
colnames(Sk_In) <- paste("In", colnames(Sk_In), sep = "_")
Sk_In <- tibble::rownames_to_column(Sk_In, "LAD")

#Sk_Out - group by number of people leaving the area
Sk_Out = DF%>%group_by(Out, Cat) %>%
  summarise(Cat_Moves = sum(Moves))%>%
  spread(., key = Cat, value = Cat_Moves)%>%
  as.data.frame()
rownames(Sk_Out)<-Sk_Out[,1]
Sk_Out = Sk_Out[,-1]

Sk_Out$Sums = rowSums(Sk_Out)

for (i in c(1:ncol(Sk_Out))){
  Sk_Out[,i] = Sk_Out[,i]/Sk_Out$Sums
}
Sk_Out = Sk_Out %>% select(-Sums)


colnames(Sk_Out) <- paste("Out", colnames(Sk_Out), sep = "_")
Sk_Out <- tibble::rownames_to_column(Sk_Out, "LAD")


Sk_All = merge(Sk_In, Sk_Out, by = "LAD")


#Gk - inequality for each age group in each LAD

Categories = unique(DF$Cat)


Gk = data.frame(LAD = unique(DF$In))
Gk_Function = function(Categories){
  OD_Matrix = DF%>%filter(.,Cat == Categories)%>%select(-c("Cat"))%>% acast(., Out~ In, mean)
  OD_Matrix[is.na(OD_Matrix)]<-0
  print(paste("Matrix Made"))
  Gini = migration.gini(OD_Matrix, corrected = FALSE)
  In = data.frame(LAD = rownames(OD_Matrix), In = unlist(Gini[5]))
  Out = data.frame(LAD = rownames(OD_Matrix), Out = unlist(Gini[6]))
  Res =  merge(In, Out, by = "LAD")
  colnames(Res)[c(2:3)] <- paste(colnames(Res[,c(2:3)]), Categories, sep = "_")
  Gk = merge(Gk, Res, by = "LAD")
  print(paste(Categories, "done"))
  return(Gk)
}
Gk = lapply(Categories, Gk_Function)
Gk_All =Gk[[1]] 
for (i in c(2:length(Gk))){
  Gk_All = merge(Gk_All,Gk[[i]], by = "LAD")
}


#Rk
Location_List = unique(DF$Out)
Location_Categories = data.frame(Categories = unique(DF$Cat))
Categories = unique(DF$Cat)



Rk_In_Function = function(Location_List){

  #Filter the data frame to choose one place's incoming flows
  print(paste(Location_List, "Started"))
  DF_Location = DF%>%filter(., DF[,"In"] == Location_List)
  #Rank that origin's incoming flows
  All_Rank = DF_Location%>%select(-"Cat")%>%
    group_by(Out, In)%>%
    summarise(All_Moves = sum(Moves))%>%
    arrange(., by_group = All_Moves)
  All_Rank$Rank = rank(All_Rank$All_Moves)
  
  #Output - a DF with in/out/number of moves/Overall rank for that set of data
  
  Rk_Cat_Function = function(Categories){
    #print(paste(Location_List, Categories, "Started"))
    
    #Make a DF with just chosen category - this will have in/out/moves for category
    DF_Cat = DF_Location%>% filter(., Cat == Categories)%>%
      group_by(Out, In)%>%
      summarise(Cat_Moves = sum(Moves))%>%
      arrange(., by_group = Cat_Moves)
    #rank by category
    DF_Cat$Cat_Rank = rank(DF_Cat$Cat_Moves)
    #merge with all
    DF_Cat = merge(DF_Cat, All_Rank, by = c("In", "Out"))
    #make Rk
    Rk = cov(DF_Cat$Cat_Moves, DF_Cat$Rank)/cov(DF_Cat$Cat_Moves, DF_Cat$Cat_Rank)
    #print(paste(Location_List, Categories, "Finished"))
    return(Rk) #Returns a single value - Rk for that origin, for that category
  }
  
  Rk_Location = data.frame(Categories = Categories, Score = unlist(lapply(Categories, Rk_Cat_Function)))
  colnames(Rk_Location)[2] <- Location_List
  
  
  return(Rk_Location)
  print(paste(Location_List, "Finished"))

}

Rk_Output = lapply(Location_List, Rk_In_Function)

Rk_In =Rk_Output[[1]] 
for (i in c(2:length(Rk_Output))){
  Rk_In = merge(Rk_In,Rk_Output[[i]], by = "Categories")
}

rownames(Rk_In)<-Rk_In[,1]
Rk_In = Rk_In[,-1]%>%t()
colnames(Rk_In) <- paste("In", colnames(Rk_In), sep = "_")
Rk_In <- tibble::rownames_to_column(data.frame(Rk_In), "LAD")



Rk_Out_Function = function(Location_List){
  
  #Filter the data frame to choose one place's incoming flows
  print(paste(Location_List, "Started"))
  DF_Location = DF%>%filter(., DF[,"Out"] == Location_List)
  #Rank that origin's incoming flows
  All_Rank = DF_Location%>%select(-"Cat")%>%
    group_by(Out, In)%>%
    summarise(All_Moves = sum(Moves))%>%
    arrange(., by_group = All_Moves)
  All_Rank$Rank = rank(All_Rank$All_Moves)
  
  #Output - a DF with in/out/number of moves/Overall rank for that set of data
  
  #Get list of categories to loop over
  #Categories = unique(DF_Location$Cat)
  
  
  Rk_Cat_Function = function(Categories){
    #print(paste(Location_List, Categories, "Started"))
    
    #Make a DF with just chosen category - this will have in/out/moves for category
    DF_Cat = DF_Location%>% filter(., Cat == Categories)%>%
      group_by(Out, In)%>%
      summarise(Cat_Moves = sum(Moves))%>%
      arrange(., by_group = Cat_Moves)
    #rank by category
    DF_Cat$Cat_Rank = rank(DF_Cat$Cat_Moves)
    #merge with all
    DF_Cat = merge(DF_Cat, All_Rank, by = c("In", "Out"))
    #make Rk
    Rk = cov(DF_Cat$Cat_Moves, DF_Cat$Rank)/cov(DF_Cat$Cat_Moves, DF_Cat$Cat_Rank)
    #print(paste(Location_List, Categories, "Finished"))
    return(Rk) #Returns a single value - Rk for that origin, for that category
  }
  
  Rk_Location = data.frame(Categories = Categories, Score = unlist(lapply(Categories, Rk_Cat_Function)))
  colnames(Rk_Location)[2] <- Location_List
  
  
  return(Rk_Location)
  print(paste(Location_List, "Finished"))
  #Location_Categories[,Location_List] = unlist(lapply(Categories, Rk_Cat_Function))
  #Location_Categories = Location_Categories%>%rename(Location_List = New)
  #colnames(Location_Categories)[which( colnames(Location_Categories)=="New" )] <- paste(Location_List)
}

Rk_Output = lapply(Location_List, Rk_Out_Function)

Rk_Out =Rk_Output[[1]] 
for (i in c(2:length(Rk_Output))){
  Rk_Out = merge(Rk_Out,Rk_Output[[i]], by = "Categories")
}

rownames(Rk_Out)<-Rk_Out[,1]
Rk_Out = Rk_Out[,-1]%>%t()
colnames(Rk_Out) <- paste("Out", colnames(Rk_Out), sep = "_")
Rk_Out <- tibble::rownames_to_column(data.frame(Rk_Out), "LAD")


Rk_All = merge(Rk_In, Rk_Out, by = "LAD")


Range = data.frame(Sk_In$LAD)%>%
  rename(LAD = 1)%>%
  mutate(Sk_Out = rowMaxs(as.matrix(Sk_Out[,c(-1)])) - rowMins(as.matrix(Sk_Out[,c(-1)])),
         Sk_In = rowMaxs(as.matrix(Sk_In[,c(-1)]))-rowMins(as.matrix(Sk_In[,c(-1)])),
         Gk_In = rowMaxs(as.matrix(Gk_All[,c(2,4,6,8,10,12,14)])) - rowMins(as.matrix(Gk_All[,c(2,4,6,8,10,12,14)])),
         Gk_Out = rowMaxs(as.matrix(Gk_All[,c(3,5,7,9,11,13,15)]))-rowMins(as.matrix(Gk_All[,c(3,5,7,9,11,13,15)])),
         Rk_In = rowMaxs(as.matrix(Rk_In[,c(-1)]))-rowMins(as.matrix(Rk_In[,c(-1)])),
         Rk_Out = rowMaxs(as.matrix(Rk_Out[,c(-1)]))-rowMins(as.matrix(Rk_Out[,c(-1)])))


#Final Share by category
rm(list=setdiff(ls(), c("Rk_All", "Sk_All", "Gk_All", "Full_Gini", "DF", "Range")))


Categories = colnames(Gk_All[2:ncol(Gk_All)])

colnames(Rk_All)[2:ncol(Rk_All)] <- paste("Rk", colnames(Rk_All)[2:ncol(Rk_All)], sep = "_")
colnames(Gk_All)[2:ncol(Gk_All)] <- paste("Gk", colnames(Gk_All)[2:ncol(Gk_All)], sep = "_")
colnames(Sk_All)[2:ncol(Sk_All)] <- paste("Sk", colnames(Sk_All)[2:ncol(Sk_All)], sep = "_")

#colnames(Gk_All)[2:ncol(Gk_All)] <-sub(pattern = "Gk_*", replacement = "", x = colnames(Gk_All[2:ncol(Gk_All)]), perl = TRUE)


Share_Function = function(Categories){
  Direction = str_split_fixed(Categories, "_", n = 2)[1]
  
  Share_Calc = merge(Sk_All[,c("LAD", paste0("Sk_",Categories))],Gk_All[,c("LAD", paste0("Gk_",Categories))], by = "LAD")%>%
    merge(.,Rk_All[,c("LAD", paste0("Rk_",Categories))], by = "LAD")%>%merge(.,Full_Gini[, c("LAD", Direction)])
  
  Share_Calc$Tk = Share_Calc[,2]*Share_Calc[,3]*Share_Calc[,4]
  
  Share_Calc$Share = Share_Calc$Tk/Share_Calc[,5]
  
  Share_Calc = Share_Calc[,c("LAD", "Share")]
  
  colnames(Share_Calc)<-c("LAD", paste0("Share_", Categories))
  
  print(paste(Categories, "Finished"))
  
  return(Share_Calc)
}

Shares = lapply(Categories,Share_Function)

Shares_Total =Shares[[1]] 
for (i in c(2:length(Shares))){
  Shares_Total = merge(Shares_Total,Shares[[i]], by = "LAD")
}

Shares_In = select(Shares_Total,contains("_In_"))%>%cbind(Shares_Total$LAD, .)%>%rename(LAD =1)
Shares_In$Sum = rowSums(Shares_In[,c(2:ncol(Shares_In))]) #Sum to 1 - validates
Shares_In%>%select(-"Sum")%>%write.csv(., "Shares_In.csv")
Shares_Out = select(Shares_Total,contains("_Out_"))%>%cbind(Shares_Total$LAD, .)%>%rename(LAD =1)
Shares_Out$Sum = rowSums(Shares_Out[,c(2:ncol(Shares_Out))]) #Sum to 1 - validates
Shares_Out%>%select(-"Sum")%>%write.csv(., "Shares_Out.csv")

Shares_Out = Shares_Out%>%select(-"Sum")
Shares_In = Shares_In%>%select(-"Sum")

Range = Range%>%
  mutate(Shares_Out = rowMaxs(as.matrix(Shares_Out[,c(-1)])) - rowMins(as.matrix(Shares_Out[,c(-1)])),
         Shares_In = rowMaxs(as.matrix(Shares_In[,c(-1)])) - rowMins(as.matrix(Shares_In[,c(-1)]))
  )

Shares

LAD_Selective = c("Knowsley", "Cornwall", "Nottingham", "Manchester", "Charnwood", "City of London")


Selective_RK_In = filter(Rk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_In_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Rk_In_*", replacement = "", x = Cat, perl = TRUE))



Selective_SK_In = filter(Sk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_In_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Sk_In_*", replacement = "", x = Cat, perl = TRUE))
  
Selective_GK_In = filter(Gk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD",matches("_In_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Gk_In_*", replacement = "", x = Cat, perl = TRUE))

Selective_RK_In = filter(Rk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_In_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Rk_In_*", replacement = "", x = Cat, perl = TRUE))

Selective_Share_In = filter(Shares_Total, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_In_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Share_In_*", replacement = "", x = Cat, perl = TRUE))



LAD_Selective = c("Knowsley", "Cornwall", "Neath.Port.Talbot", "Central.Bedfordshire", "Nottingham")
#LAD_Selective = LAD_Selective[3]


LAD_Selective = LAD_Selective[1]
Select = function(LAD_Selective){
  Table = data.frame(LAD = c(LAD_Selective,"","","","","",""),Cat = Selective_SK_In[,"Cat"])%>%
    merge(., Selective_SK_In[, c("Cat", LAD_Selective)])%>%
    rename(Sk =LAD_Selective )%>%
    merge(., Selective_GK_In[, c("Cat", LAD_Selective)])%>%
    rename(Gk =LAD_Selective )%>%
    merge(., Selective_RK_In[, c("Cat", LAD_Selective)])%>%
    rename(Rk =LAD_Selective )%>%
    merge(., Selective_Share_In[, c("Cat", LAD_Selective)])%>%
    rename(Share =LAD_Selective )
  return(Table)
}
Test = lapply(LAD_Selective,Select)

In = rbind(Test[[1]], Test[[2]])%>%
  rbind(., Test[[3]])%>%
  rbind(., Test[[4]])%>%
  rbind(., Test[[5]])%>%
  mutate(#Group = sub(pattern = "Sk_In_*", replacement = "", x = Group, perl = TRUE),
         Sk = (round(as.numeric(Sk), 3)),
         Gk = round(as.numeric(Gk), 3),
         Rk = round(as.numeric(Rk), 3),
         #TK = Sk*Gk*Rk,
         Share = (round(as.numeric(Share), 3)),
         Relative_Inequality = round(Share/Sk,3),
         Relative_Effect = round(Share - Sk,3),
         Cat = str_replace(Cat, "^\\S_", "")
         )%>%
  mutate(Cat = gsub("_"," ", Cat))%>%
  rename(Category = 1,
         "Local Authority" = 2,
         "Share of Migrants" = 3,
         "Gini Index" = 4,
         #TK = 5,
         "Gini Correlation" = 5,
         "Gini Share" = 6,
         "Relative Inequality" = 7,
         "Relative Effect" = 8)%>%
  select(c(2,1,3,4,5,6,7,8))



Caption = "Decomposition for Selected Local Authorities: In-Migration"


In %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(In),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")


#LAD_Selective = c("Knowsley", "Cornwall", "Neath Port Talbot", "Central Bedfordshire", "Nottingham")
Selective_SK_Out = filter(Sk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_Out_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Sk_Out_*", replacement = "", x = Cat, perl = TRUE))

Selective_GK_Out = filter(Gk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD",matches("_Out_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Gk_Out_*", replacement = "", x = Cat, perl = TRUE))

Selective_RK_Out = filter(Rk_All, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_Out_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Rk_Out_*", replacement = "", x = Cat, perl = TRUE))

Selective_Share_Out = filter(Shares_Total, LAD %in% LAD_Selective)%>%
  select(., c("LAD", matches("_Out_")))%>%
  t()%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]%>%
  data.frame()%>%
  mutate_if(is.character,as.numeric)%>%
  rownames_to_column(var = "Cat")%>%
  mutate(Cat = sub(pattern = "Share_Out_*", replacement = "", x = Cat, perl = TRUE))



LAD_Selective = c("Knowsley", "Cornwall", "Neath.Port.Talbot", "Central.Bedfordshire", "Nottingham")

LAD_Selective = LAD_Selective[1]

Select = function(LAD_Selective){
  Table = data.frame(LAD = c(LAD_Selective,"","","","","",""),Cat = Selective_SK_Out[,"Cat"])%>%
    merge(., Selective_SK_Out[, c("Cat", LAD_Selective)])%>%
    rename(Sk =LAD_Selective )%>%
    merge(., Selective_GK_Out[, c("Cat", LAD_Selective)])%>%
    rename(Gk =LAD_Selective )%>%
    merge(., Selective_RK_Out[, c("Cat", LAD_Selective)])%>%
    rename(Rk =LAD_Selective )%>%
    merge(., Selective_Share_Out[, c("Cat", LAD_Selective)])%>%
    rename(Share =LAD_Selective )
  return(Table)
}
Test = lapply(LAD_Selective,Select)


Out = rbind(Test[[1]], Test[[2]])%>%
  rbind(., Test[[3]])%>%
  rbind(., Test[[4]])%>%
  rbind(., Test[[5]])%>%
  mutate(#Group = sub(pattern = "Sk_In_*", replacement = "", x = Group, perl = TRUE),
    Sk = (round(as.numeric(Sk), 3)),
    Gk = round(as.numeric(Gk), 3),
    Rk = round(as.numeric(Rk), 3),
    #TK = Sk*Gk*Rk,
    Share = (round(as.numeric(Share), 3)),
    Relative_Inequality = round(Share/Sk,3),
    Relative_Effect = round(Share - Sk,3),
    Cat = str_replace(Cat, "^\\S_", "")
  )%>%
  mutate(Cat = gsub("_"," ", Cat))%>%
  rename(Category = 1,
         "Local Authority" = 2,
         "Share of Migrants" = 3,
         "Gini Index" = 4,
         #TK = 5,
         "Gini Correlation" = 5,
         "Gini Share" = 6,
         "Relative Inequality" = 7,
         "Relative Effect" = 8)%>%
  select(c(2,1,3,4,5,6,7,8))



Caption = "Decomposition for Selected Local Authorities: Out-Migration"

Out %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Out),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")


