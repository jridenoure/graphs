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
factor1 <- c("1","2","3","4","5")
factor2 <- c("80","30","80","30","80")
factor3 <- c("M","F","F","M","F")
varA.VM_Pre <- rnorm(5)
varA.VM_Post <- rnorm(5)
varA.VL_Pre <- rnorm(5)
varA.VL_Post <- rnorm(5)

testdf <- data.frame(cbind(factor1,factor2,factor3,
                           varA.VM_Pre,
                           varA.VM_Post,
                           varA.VL_Pre,
                           varA.VL_Post))


#table4a %>% pivot_longer(c(`1999`,`2000`),names_to="time",values_to="cases")


testdf_1<-testdf %>% pivot_longer(starts_with("var"),
               names_to = c(".value", "timepoint"),
               names_sep = "_")
testdf_1 %>% pivot_longer(starts_with("var"),
                                  names_to = "Muscle",
                          names_prefix="varA."
                                  )

dataWB <- read_excel("D://High-Low Exercise/NSCA US reformat.xlsx",
                     sheet="ultrasoundData",col_names=T,progress=T)
HL_MVIC <- read_excel("D://High-Low Exercise/NSCA US reformat.xlsx",
                     sheet="MVIC",col_names=T,progress=T)
HL_EMG <- read_excel("D://High-Low Exercise/NSCA US reformat.xlsx",
                     sheet="EMGData",col_names=T,progress=T)
HL_EMG_df <- make_clean_names(HL_EMG)


HL_MVIC_df <- HL_MVIC
HL_MVIC_df<-make_clean_names(HL_MVIC_df) 
HL_MVIC_df<- HL_MVIC_df %>% convert_as_factor(UID,Sex,Condition)

HL_US_df <- dataWB
HL_US_df <- HL_US_df %>% convert_as_factor(UID,Sex,Condition)

HL_US_df<-make_clean_names(HL_US_df)
########Mus thickness#########
MT_df<-HL_US_df %>% pivot_longer(starts_with("MT"),
                        names_to = c(".value", "timepoint"),
                        names_sep = "_")
MT_df<-MT_df %>% select(Sex:Condition,timepoint:last_col())

MT_df<- MT_df %>% pivot_longer(starts_with("MT"),
                             names_to = "Muscle",
                       names_prefix = "MT.") 
MT_df<-MT_df %>% convert_as_factor(timepoint,Muscle)
                             
MT<-anova_test(data=MT_df,wid=UID,
               dv=value,
               within=c(timepoint,Condition),
               between = c(Muscle,Sex))
get_anova_table(MT,correction="GG")
########### Echo Intensity by Muscle ###############
#ugly ahh block of code#
EIM_df<-(((HL_US_df 
        %>% pivot_longer(starts_with("EI"),
                               names_to = c(".value","timepoint"),
                               names_sep = "_")) 
        %>% select(Sex:Condition,timepoint:last_col()))
        %>% pivot_longer(starts_with("EI"),
                              names_to = "Muscle",
                              names_prefix = "EI.")) 
        %>% convert_as_factor(timepoint,Muscle)
EIM<-anova_test(data=EIM_df,wid=UID,
               dv=value,
               within=c(timepoint,Condition),
               between = c(Muscle,Sex))
get_anova_table(EIM,correction="GG")
################ Weighted echo intensity, total quads ################
EIW_df <- ((HL_US_df %>% pivot_longer(starts_with("Weighted.EI"),
                                names_to = c(".value","timepoint"),
                                names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col())) %>% convert_as_factor(timepoint)
EIW<-anova_test(data=EIW_df,wid=UID,
                dv=Weighted.EI,
                within=c(timepoint,Condition),
                between = Sex)
get_anova_table(EIW,correction="GG")

########## Mean Quad MT ###############

MQMT_df <- ((HL_US_df %>% pivot_longer(starts_with("mean.MT"),
                                   names_to = c(".value","timepoint"),
                                   names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col())) %>% convert_as_factor(timepoint)
MQMT<-anova_test(data=MQMT_df,wid=UID,
                dv=mean.MT,
                within=c(timepoint,Condition),
                between = Sex)
get_anova_table(MQMT,correction="GG")

########## Sum Qd MT ###############
Sum_df <- ((HL_US_df %>% pivot_longer(starts_with("Sum"),
                                    names_to = c(".value","timepoint"),
                                    names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col())) %>% convert_as_factor(timepoint)
Sum<-anova_test(data=Sum_df,wid=UID,
                 dv=Sum,
                 within=c(timepoint,Condition),
                 between = Sex)
get_anova_table(Sum,correction="GG")

######### Fat thickness ############

FT_df<-(((HL_US_df %>% pivot_longer(starts_with("FT"),
                            names_to = c(".value","timepoint"),
                            names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col()) %>% pivot_longer(starts_with("FT"),
                                                                                                              names_to = "Muscle",
                                                                                                              names_prefix = "FT.")) %>% convert_as_factor(timepoint,Muscle))

FT<-anova_test(data=FT_df,wid=UID,
                dv=value,
                within=c(timepoint,Condition),
                between = c(Muscle,Sex))
get_anova_table(FT,correction="GG")

############ MVIC RESHAPING #############
HL_MVIC_df<-(HL_MVIC_df %>% pivot_longer(starts_with("MVIC"),
                                               names_to = c(".value","timepoint"),
                                               names_sep = "_")) %>% convert_as_factor(Sex,Condition,UID,timepoint)

########## EMG RESHAPING ###############
#HL_EMG_df<-
##### AMP MAX ##########

AMP_df<-(((HL_EMG_df %>% pivot_longer(starts_with("AMP."),
                                 names_to = c(".value", "timepoint"),
                                 names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col())) %>% pivot_longer(starts_with("AMP."),
                                                                                                                     names_to = "Muscle",
                                                                                                                     names_prefix="AMP.")) %>% convert_as_factor(timepoint,Muscle,Condition,Sex,UID)
 
########### MDF MAX################
MDF_df<-(((HL_EMG_df %>% pivot_longer(starts_with("MDF."),
                                      names_to = c(".value", "timepoint"),
                                      names_sep = "_")) %>% select(Sex:Condition,timepoint:last_col())) %>% pivot_longer(starts_with("MDF."),
                                                                                                                         names_to = "Muscle",
                                                                                                                         names_prefix="MDF.")) %>% convert_as_factor(timepoint,Muscle,Condition,Sex,UID)

############# QUAD COMPOSITE AMP #############
HL_EMG_df %>% select(Sex,UID,Condition,Mean.Quad.AMP_Pre,Mean.Quad.AMP_Post) %>% pivot_longer(starts_with("Mean.Quad.AMP"),names_to = c(".value","timepoint"),names_sep = "_") %>% convert_as_factor(Sex,Condition,timepoint,UID)
