library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(reshape2)
library(viridis)
library(ggExtra)



d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

glimpse(ajc)

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
       y=`AJC (both tender/swollen) at baseline`,
       fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3,outlier.alpha = 0 ) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count",
       x="Actual Treatment for Period 01",
       y="Baseline active joint count score")+
  scale_x_discrete(labels=my_xlab)+
  ylim(0,25)
  



#High INF- AJC

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Interferon Gene Expression Group 1`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`== "High")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count- INF High",
       x="Actual Treatment for Period 01",
       y="Baseline active joint count score")+
  scale_x_discrete(labels=my_xlab)+
  ylim(0,25)




#Low INF- AJC

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Interferon Gene Expression Group 1`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`== "Low")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count- INF Low",
       x="Actual Treatment for Period 01",
       y="Baseline active joint count score")+
  scale_x_discrete(labels=my_xlab)+
  ylim(0,25)




#Low C3 protein
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C3 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C3 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count (Low C3)",
       x="Actual Treatment for Period 01",
       y="Baseline active joint count score")+
  scale_x_discrete(labels=my_xlab)+
  ylim(0,25)



#Low C4 protein
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C4 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C4 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count (Low C4)",
       x="Actual Treatment for Period 01",
       y="Baseline active joint count score")+
  scale_x_discrete(labels=my_xlab)+
  ylim(0,25)











#Geographic region (Asia) - AJC
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | 
           ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Asia")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`,
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

glimpse(ajc)

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y= `AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count -Asia",
       x="Actual Treatment for Period 01",
       y="AJC (both tender/swollen) at baseline")+
  scale_x_discrete(labels=my_xlab)



#Geographic region (Europe and Middle East) - AJC
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | 
           ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Europe and Middle East")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`,
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

glimpse(ajc)

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y= `AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count -Europe and Middle East",
       x="Actual Treatment for Period 01",
       y="AJC (both tender/swollen) at baseline")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))




#Geographic region (Latin America) - AJC
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | 
           ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Latin America")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`,
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

glimpse(ajc)

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y= `AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count - Latin America",
       x="Actual Treatment for Period 01",
       y="AJC (both tender/swollen) at baseline")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))


#Geographic region (USA) - AJC
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `AJC (both tender/swollen) at baseline`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`AJC (both tender/swollen) at baseline`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | 
           ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "United States")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`,
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

glimpse(ajc)

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y= `AJC (both tender/swollen) at baseline`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline Active Joint Count - USA",
       x="Actual Treatment for Period 01",
       y="AJC (both tender/swollen) at baseline")+
  scale_x_discrete(labels=my_xlab)






#CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score)",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)










#Geographic region - (Asia) CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Asia")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score)- Asia",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,40))
  





#Geographic region - (Latin America) CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Latin America")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score)-Latin America",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,40))




#Geographic region - (United States) CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "United States")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score)-	United States",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,40))
  

  
  
  
  

#Geographic region - (Europe and Middle East) CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Geographic Region 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Europe and Middle East")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score)-	Europe and Middle East",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,40))  
  
  
  
  






#CLASI-A- INF High

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Interferon Gene Expression Group 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`=="High")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score) -INF High",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)



#CLASI-A- INF Low

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Interferon Gene Expression Group 1`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`=="Low")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score) -INF Low",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)






#Low C3-CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C3 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C3 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score) - Low C3",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)





#Low C4-CLASI-A

d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C4 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `Baseline CLASI-A (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline CLASI-A (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C4 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline CLASI-A (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline CLASI-A (Dervied Score) - Low C4",
       x="Actual Treatment for Period 01",
       y="Baseline CLASI-A (Derived Score)")+
  scale_x_discrete(labels=my_xlab)








#SLEDAI
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)







#Geographic region - SLEDAI (Asia)
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `Geographic Region 1`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Asia")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)- Asia",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))











#Geographic region - SLEDAI (	Latin America)
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `Geographic Region 1`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Latin America")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)- 	Latin America",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))







#Geographic region - SLEDAI (Europe and Middle East)
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `Geographic Region 1`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "Europe and Middle East")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)-Europe and Middle East",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))







#Geographic region - SLEDAI (United States)
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `Geographic Region 1`,
         `SRI response`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Geographic Region 1`== "United States")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)-United States",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)+
  expand_limits(y=c(NA,25))











#SLEDAI - INF high
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Interferon Gene Expression Group 1`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`== "High")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Derived Score)- INF High",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)



#SLEDAI - INF Low
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Interferon Gene Expression Group 1`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         `Interferon Gene Expression Group 1`== "Low")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Derived Score)- INF Low",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)



#SLEDAI -Low C3
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C3 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C3 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Derived Score)- Low C3",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)






#SLEDAI -Low C4
d1 <- lilac_a_respvars

#AJC box plot
ajc_d1 <- d1 %>%
  select(`Unique Subject Identifier`,
         `SRI response`,
         `Low Complement C4 Baseline Flag`,
         `Actual Treatment for Period 01`,
         `Baseline SLEDAI-2k (Derived Score)`)

#Drop respondents with missing values 
ajc_drop <- ajc_d1 %>%
  drop_na(`Baseline SLEDAI-2k (Derived Score)`)

#Filter the data set
ajc_2 <- ajc_drop %>%
  filter(ajc_drop$`Actual Treatment for Period 01` == "Placebo" | ajc_drop$`Actual Treatment for Period 01` == "BIIB059 450 mg",
         ajc_drop$`Low Complement C4 Baseline Flag`== "Y")

#Order the data set
or_ajc_2 <- ajc_2[order(ajc_2$`SRI response`, 
                        ajc_2$`Actual Treatment for Period 01`),]
ajc <- or_ajc_2[order(or_ajc_2$`Actual Treatment for Period 01`,decreasing = TRUE),]


#Filter SRI responder Vs Non Responder
sri_resp <- ajc_2 %>%
  filter(`SRI response` == "Y")

sri_non_resp <- ajc_2 %>%
  filter(`SRI response` == "N")


#Generating Box plot
my_xlab <- paste(unique(ajc$`Actual Treatment for Period 01`), 
                 "\n(N=",
                 table(ajc$`Actual Treatment for Period 01`),")",
                 sep="")

ggplot(ajc,
       aes(x=`Actual Treatment for Period 01`, 
           y=`Baseline SLEDAI-2k (Derived Score)`,
           fill= `SRI response`))+
  geom_jitter(aes(color=factor(`SRI response`),fill=factor(`SRI response`)), 
              shape=21,alpha=0.5,size=2, stroke=1, 
              position= position_jitterdodge(jitter.width = .2, jitter.height = 0))+
  geom_boxplot(alpha= 0.3) + 
  labs(title = "SRI-4 Responders Vs Non-Responders- Baseline SLEDAI-2k (Dervied Score)- Low C4",
       x="Actual Treatment for Period 01",
       y="Baseline SLEDAI-2k (Derived Score)")+
  scale_x_discrete(labels=my_xlab)





