library("stats")
library("plyr")
library("haven")
library("reshape2")
library("rstatix")
library("tidyverse")
library("ggpubr")
library("dplyr")
library("emmeans")
library("readxl")
library("lme4")

dataWB <- read_excel("D://High-Low Exercise/NSCA US reformat.xlsx",
                     sheet="Sheet1",col_names=T,progress=T)
HL_df <- dataWB
HL_df <- HL_df %>% convert_as_factor(UID,Sex,Condition)

HL_df<-make_clean_names(HL_df)


RF<-anova_test(data=HL_df,wid=UID,
               dv=RF...change,
               within=Condition,
               between = Sex)
get_anova_table(RF,correction="GG")

VL<-anova_test(data=HL_df,wid=UID,
               dv=VL...change,
               within=Condition,
               between = Sex)
get_anova_table(VL,correction="GG")

VM<-anova_test(data=HL_df,wid=UID,
               dv=VM...change,
               within=Condition,
               between = Sex)
get_anova_table(VM,correction="GG")

MeanMT<-anova_test(data=HL_df,wid=UID,
               dv=Mean.MT...change,
               within=Condition,
               between = Sex)
get_anova_table(MeanMT,correction="GG")

EI<-anova_test(data=HL_df,wid=UID,
                   dv=Weighted.EI...change,
                   within=Condition,
                   between = Sex)
get_anova_table(EI,correction="GG")

MVIC<-anova_test(data=HL_df,wid=UID,
               dv=MVIC...change,
               within=Condition,
               between = Sex)
get_anova_table(MVIC,correction="GG")

Intraset<-anova_test(data=HL_df,wid=UID,
                 dv=Mean.intraset.change,
                 within=Condition,
                 between = Sex)
get_anova_table(Intraset,correction="GG")

Midlast<-anova_test(data=HL_df,wid=UID,
                     dv=Mean.Mid.last.change,
                     within=Condition,
                     between = Sex)
get_anova_table(Midlast,correction="GG")

FirstLast<-anova_test(data=HL_df,wid=UID,
                    dv=First.rep...Last.rep,
                    within=Condition,
                    between = Sex)
get_anova_table(FirstLast,correction="GG")

AMP<-anova_test(data=HL_df,wid=UID,
                dv=AMP...change,
                within=Condition,
                between = Sex)
get_anova_table(AMP,correction="GG")

MDF <-anova_test(data=HL_df,wid=UID,
                 dv=MDF...change,
                 within=Condition,
                 between = Sex)
get_anova_table(MDF,correction="GG")

outliers<-identify_outliers(data=HL_df,variable="Mean.Mid.last.change")$Mean.Mid.last.change
mean<-mean(HL_df$Mean.Mid.last.change,na.rm=TRUE)
sd<-sd(HL_df$Mean.Mid.last.change,na.rm=TRUE)
z <- (outliers - mean) / sd


for (i in 6:dim(HL_df)[2]{
  mad<-mad(HL_df[,i],na.rm=TRUE)
  lower<-median(HL_df[,i],na.rm=TRUE) - 3 * mad
  upper<-median(HL_df[,i],na.rm=TRUE) + 3 * mad
  outlier_ind <- which(HL_df[,i] < lower | HL_df[,i] > upper)
  HL_df[,i][outlier_ind]
}
i<-57
mad<-mad(HL_df[,i],na.rm=TRUE)
lower<-median(HL_df[[i]],na.rm=TRUE) - 3 * mad
upper<-median(HL_df[[i]],na.rm=TRUE) + 3 * mad
outlier_ind <- which(HL_df[,i] < lower | HL_df[,i] > upper)
HL_df[,i][outlier_ind,]


data.frame(HL_df[,i])


#Hampel <- function(data,){
  
 # }