#In this file: Visualisations for Final_Gini Dataset

#Requires - CSV of gini index scores for each small area being compared.
#Need shapefiles to make maps

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

Packages = c("plyr","tidyverse","dplyr", "ggplot2")

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


#Import full Gini dataset
Full_Gini = read.csv("Full_Gini.csv")%>%
  select(-1)%>%
  #add categories
  mutate(Redistributors = case_when(
           -1<= Scaled_Gini_In & Scaled_Gini_In<= 1  & -1<=Scaled_Gini_Out & Scaled_Gini_Out<= 1 ~ "Not Redistributors",
           Scaled_Gini_In <= 0  & Scaled_Gini_Out >= 1|Scaled_Gini_In <= -1  & Scaled_Gini_Out >= 0 ~ "Pure Inward",
           Scaled_Gini_In >= 0  & Scaled_Gini_Out <= -1|Scaled_Gini_In >= 1  & Scaled_Gini_Out <= 0 ~ "Pure Outward",
           Scaled_Gini_In >= 1  & Scaled_Gini_Out >= 0 |Scaled_Gini_In >= 0  & Scaled_Gini_Out >= 1 ~ "Intensive Redistributors",
           Scaled_Gini_In <= -1  & Scaled_Gini_Out <= 0 |Scaled_Gini_In <= 0  & Scaled_Gini_Out <= -1 ~ "Extensive Redistributors")
         )

#Anaylsis: Scatter


#GGPlot - out vs in, add straight lines for the 0 points, and add a regression line. 

# Fit regression line
require(stats)
reg_2019<-lm(Scaled_Gini_In ~ Scaled_Gini_Out, data = Full_Gini)
coeff_2019=coefficients(reg_2019)

#Pick the points to label
LAD_Vis = c("Knowsley", 
            "Cornwall", 
            "Neath Port Talbot", 
            "Central Bedfordshire", 
            "North East Derbyshire" ,
            "Nottingham",
            "Eden",
            "Barking and Dagenham",
            "Charnwood",
            "Milton Keynes",
            "Thurrock",
            "County Durham"
)

#If you want to identify specific pointss, use ggplotly - install the package. This code makes the scatter plot

ggplot(Full_Gini, aes(x=Scaled_Gini_Out, y=Scaled_Gini_In,col = Redistributors))+

  xlim(-4.2, 4.2)+
  ylim(-4.2, 4.2)+
  geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
            fill = "transparent", color = "red")+
  geom_point(aes(x = Scaled_Gini_Out, y = Scaled_Gini_In))+
  geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=0.5, show.legend=TRUE)+
  geom_abline(intercept = coeff_2019[1], slope = coeff_2019[2], color="blue",linetype="dashed", size=1.5)+
  geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=0.5)+
  geom_hline(yintercept=0, color = "black")+
  geom_vline(xintercept = 0, color = "black")+ 
  ggtitle("Migration Field Diagram")+ xlab("Out-Migration Gini") + ylab("In-Migration Gini")+
  annotate("text", x = 3.3, y = 4, label = "y = x") +
  annotate("text", x = 4, y = 2.3, color = "blue", label = "R")+
  geom_text(data =Full_Gini%>%filter(LAD %in% LAD_Vis),aes(x=Scaled_Gini_Out, y=Scaled_Gini_In, label = LAD),nudge_y = 0.1, colour = "black", size = 3)+
  annotate("text", x = -2.75, y = 2.75, color = "black", label = "Outwards Redistributors")+
  annotate("text", x = 2.75, y = -2.75, color = "black", label = "Inwards Redistributors")
  
  
  



###Map visualisation

#Making the biplot
Path = getwd()

Gini_Map_2019= sf::st_read(dsn = (paste0(Path, "/SHAPES")),layer="LAD_Map")%>%
  select(c(2,3))%>%
  rename(LAD = 1)%>%
  merge(., Full_Gini, by = "LAD")%>%
  st_transform(27700)%>%
  select(-c("Scaled_Gini_In", "Scaled_Gini_Out"))

Missing_Map = sf::st_read(dsn = (paste0(Path, "/SHAPES")),layer="Missing_Map")%>%
  select(-"code")%>%
  rename(LAD = name)%>%
  mutate(bi_class = NA,
         Gini_In = NA,
         Gini_Out = NA)%>%
  st_transform(27700) 




data <- bi_class(Gini_Map_2019, x = Gini_Out, y = Gini_In, style = "quantile", dim = 3)%>%
  rbind(., Missing_Map)

install.packages('ggsn')
library(ggsn)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "grey", size = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Migration Field Categories: 
    Inward or Outward redistributors?"#,
  ) +
  bi_theme()+
  theme(plot.title = element_text(size = 14, face = "bold"))
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Focused Out-Migration",
                    ylab = "Focused In-Migration",
                    size = 8)

ggdraw() +
  draw_plot(map, 0, 0, 1.1, 1.1) +
  draw_plot(legend, 0.65, 0.55, 0.17, 0.22)



Full_Gini = read.csv("Full_Gini.csv")%>%select(-1)

n = 5
Most_Dispersed_In = Full_Gini%>%
  select(c("LAD", "Gini_In"))%>%
  arrange(-Gini_In)%>%
  mutate(Rank = c(1:nrow(.)))%>%
  filter(., Rank%in%c((nrow(.)-(n-1)):(nrow(.))))%>%
  #mutate(Rank = rev(c(1:n)))%>%
  #arrange(-Rank)%>%
  rename(In=LAD)%>%
  mutate(In = paste0(In, " (",round(Gini_In,3), ")"))%>%
  select(-"Gini_In")



Most_Dispersed_Out = Full_Gini%>%
  select(c("LAD", "Gini_Out"))%>%
  arrange(-Gini_Out)%>%
  mutate(Rank = c(1:nrow(.)))%>%
  filter(., Rank%in%c((nrow(.)-(n-1)):(nrow(.))))%>%
  #select(-"Gini_Out")%>%
  #mutate(Rank = rev(c(1:n)))%>%
  #arrange(-Rank)%>%
  rename(Out = LAD)%>%
  mutate(Out = paste0(Out, " (",round(Gini_Out,3), ")"))%>%
  select(-"Gini_Out")

Dispersed = merge(Most_Dispersed_In,Most_Dispersed_Out, by = "Rank")
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








