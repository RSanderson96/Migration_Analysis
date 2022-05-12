#In this file: Calculating Gini Index for 2019 only - input merged table, output table of gini results (national)

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.


Packages = c("plyr","dplyr","reshape2", "migration.indices", "kableExtra")


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



#Import dataset and merge into a matrix
Matrix_2019 = read.csv("Paper_Merged_2019.csv")%>%select(-1)%>%
  acast(., OutLA ~ InLA)
Matrix_2019[is.na(Matrix_2019)]<-0
diag(Matrix_2019)<-0

#Calculate the gini index
Gini = migration.gini(Matrix_2019, corrected = FALSE)

Gini_In = data.frame(LAD = rownames(Matrix_2019), Gini_In = unlist(Gini[5]))
Gini_Out = data.frame(LAD = rownames(Matrix_2019), Gini_Out = unlist(Gini[6]))
row.names(Gini_In) <- NULL
row.names(Gini_Out) <- NULL

#Make the full gini index file
Full_Gini = merge(Gini_In, Gini_Out, by = "LAD")%>%
  mutate(Scaled_Gini_In = as.numeric(scale(Gini_In)),
         Scaled_Gini_Out = as.numeric(scale(Gini_Out)))%>%
  write.csv(.,"Full_Gini.csv")

#Making table
Mig_2019_Details = data.frame(Index = c("Out-Migration(Rows)", 
                                        "In-Migration (Columns)",
                                        #"Exchanges Gini Index", 
                                        "Total Gini Index"),
                              Values = c(unlist(Gini[3]),
                                         unlist(Gini[4]),
                                         unlist(Gini[1])))%>%
  mutate(Values = summarise(., Values = round(Values, digits = 4)))

row.names(Mig_2019_Details) <- NULL



Table = Mig_2019_Details
Caption = "Index Results: All Local Authorities 2019"


Table %>%
  kbl(caption= Caption,
      format= "html",
      col.names = names(Table),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica")
