#SRI-4 responders 

library(dplyr)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(reshape2)
library(BiocManager)
library(pheatmap)
library(tidyverse)
library(RColorBrewer)
library(grid)
library(viridis)


#Access ANA col
ana <- ana_sheet
ana<-data.frame(ana)
rownames(ana)<- ana$Unique.Subject.Identifier
ana<- ana %>%
  select(ANA.positive.negative)
ana <- data.frame(ana)  

#Access the data
df1<- lilac_a_respvars

#Bind ana_sheet to df1
df2<- cbind(ana, df1)

#Select the data
var1 <- df2 %>%
  select(`Unique Subject Identifier`, 
         `SRI response`, 
         `Actual Treatment for Period 01`, 
         `Geographic Region 1`, 
         `Interferon Gene Expression Group 1`, 
         ANA.positive.negative,`Baseline AJC (both tender/swollen)`, 
         `AJC (both tender/swollen)>=2`, `AJC (both tender/swollen)>=4`, 
         `AJC (both tender/swollen)>=6`, `AJC (both tender/swollen)>=8`)

#Filter the data set
var2 <- var1 %>%
  filter(`Actual Treatment for Period 01`== "Placebo" | var1$`Actual Treatment for Period 01`== "BIIB059 450 mg")


#Order the data 
or_var2 <- or_var2 <- var2[order(var2$`SRI response`,
                                 var2$`Actual Treatment for Period 01`,
                                 var2$`Geographic Region 1`,
                                 var2$`Interferon Gene Expression Group 1`, 
                                 var2$ANA.positive.negative ,
                                 var2$`Baseline AJC (both tender/swollen)` ),]
#Melt the ordered data
orvar2_melt <- melt(or_var2)

#Convert id into a factor
orvar2_melt$`Unique Subject Identifier` <- factor(orvar2_melt$`Unique Subject Identifier`, levels = unique(orvar2_melt$`Unique Subject Identifier`))

#Creating a matrix for the master HM
row.names(orvar2_melt) <- orvar2_melt$`Unique Subject Identifier`
orvar3_melt<-orvar2_melt%>%
  select(-`Unique Subject Identifier`)


#Matrix for responders and non-responders
m<-matrix(nrow= 1, orvar2_melt$value)
colnames(m)<- orvar2_melt$`Unique Subject Identifier`
m
rownames(m) <- factor(rownames(m), 
                           levels = unique(rownames(m)))
p<-t(m)

#Create a data frame for Responders and Non-Responders
resp_non <- orvar2_melt %>%
  select(`Unique Subject Identifier`,`SRI response`,value)
resp_non<-data.frame(resp_non)
rownames(resp_non)<- (resp_non$Unique.Subject.Identifier)
resp_non<-  resp_non%>%
  select(-Unique.Subject.Identifier)
resp_non<- resp_non %>%
  select(-value)


#Create a data frame for Treatment/Placebo
treat_plac  <- orvar2_melt %>%
  select(`Unique Subject Identifier`, `Actual Treatment for Period 01`)
rownames(treat_plac)<- treat_plac$`Unique Subject Identifier`
treat_plac <- treat_plac%>%
  select(`Actual Treatment for Period 01`)

#Create a data frame for Geographic region 
geo <- orvar2_melt%>%
  select(`Unique Subject Identifier`, `Geographic Region 1`)
rownames(geo)<- geo$`Unique Subject Identifier`
geo<-geo %>%
  select(-`Unique Subject Identifier`)

#Create a data frame for Interferon Gene expression
gene_exp <- orvar2_melt %>%
  select(`Unique Subject Identifier`, `Interferon Gene Expression Group 1`)
rownames(gene_exp)<-gene_exp$`Unique Subject Identifier`
gene_exp<-gene_exp %>%
  select(-`Unique Subject Identifier`)

#Create  a data frame for ANA
ana_pn <- orvar2_melt %>%
  select(`Unique Subject Identifier`, ANA.positive.negative)
rownames(ana_pn)<- ana_pn$`Unique Subject Identifier`
ana_pn <- ana_pn %>%
  select(-`Unique Subject Identifier`)

#Create data frame for baseline ajc
df_ajc <- orvar2_melt %>%
  select(`Unique Subject Identifier`,value)
rownames(df_ajc)<- df_ajc$`Unique Subject Identifier`
df_ajc <- df_ajc %>%
  select(-`Unique Subject Identifier`)
df_ajc$AJCBINNED<-cut(df_ajc$value,c(-1,4,10,1000),labels =c("<4",">=4 to <10",">=10"))


ann_df <- data.frame(df_ajc$AJCBINNED,gene_exp,geo,treat_plac,resp_non)
rownames(ann_df) <- factor(rownames(ann_df),
                           levels = unique(rownames(ann_df)))



#Creating list of data frames
names(ann_df) <- c("Baseline AJC",
                   "INF expression",
                   "Geographic region",
                   "Treatment/Placebo",
                   "SRI response")
#Color dictionary creation
color_dict <- list(`Baseline AJC`= c("<4" = "black",
                                     ">=4 to <10"="blue",
                                     ">=10"="yellow"),
                   `INF expression`= c("High"= "cyan", 
                                       "Low"= "yellow"),
                   `Geographic region`= c("Asia" = "darkolivegreen",
                                          "Europe and Middle East"= "darkblue", 
                                          "Latin America" = "coral", 
                                          "United States"="violetred"),
                   `Treatment/Placebo` = c("BIIB059 450 mg" = "magenta", 
                                           "Placebo" = "#1E90FF"),
                   `SRI response` = c("Y"= "green3", 
                                        "N"="red3"
                   ))



#Setting breaks
breaks = c(seq(0,26,2))
breaks2 <- breaks


#Generating annotated heatmap
pheatmap(m,cluster_rows = F, cluster_cols = F,
         main= "SRI Responder Vs Non-responders - Baseline AJC (both tender/swollen)",
         color = c("0 to <2"="#225188",
                   ">=2 to <4"="#86CAE1",
                   ">=4 to <6" ="#F7FCC9",
                  ">=6 to <8" ="#FA9D4F",
                  ">=8 to <10 "="brown",
                  ">=10 to <12" = "brown",
                  ">=12 to <14" = "brown",
                  ">=14 to <16" = "brown",
                  ">=16 to <18" = "brown",
                  ">=18 to <20" = "brown",
                  ">=20 to <22" = "brown",
                  ">=22 to <24" = "brown",
                  ">=24 to <26" = "brown"
                  ),
         legend = T,
         border_color = T,
         legend_breaks = c(0,2,4,6,8, 10, 12, 14, 16,18,20,22,24,26) ,
         show_rownames = T, fontsize = 7 ,angle_col = 90,show_colnames = T,
         labels_row = "Baseline AJC (both tender/swollen)",
         annotation_col = ann_df ,
         annotation_colors = color_dict,
         annotation_legend = T,
         gaps_col = c(28,68,104),
         breaks = breaks2
)









