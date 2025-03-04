library(ggplot2)
library(reshape2)
library(rstatix)
library(readxl)
library(tidyverse)
library(ggsignif)
library(extrafont)
library(tcltk)
library(tools)

#only run this once:#
font_import()

dfpath<-tk_choose.files(caption = "Choose data file")
file_ext(dfpath)
df<-NULL
############## intake ###################
while(is.null(df)==TRUE){
  if(file_ext(dfpath)=="xlsx"){
    
    if(length(excel_sheets(path=dfpath))>1){
      activeSheet <- tk_select.list(choices = excel_sheets(path=dfpath))
      df <- read_excel(dfpath,col_names=T,sheet = activeSheet)}
    
    else if (length(excel_sheets(path=dfpath))==1){df <- read_excel(dfpath,col_names=T)}
  }
  if(file_ext(dfpath)=="csv"){
    df <- read_delim(dfpath)}
}
rm(activeSheet)

df <- read_excel(dfpath,col_names=T,sheet = activeSheet)

tk_messageBox(type=("ok"),message="Choose factors")

factors <- tk_select.list(choices = colnames(df),multiple=TRUE)


df<- df %>% convert_as_factor(vars=factors)


tk_messageBox(type=("ok"),message="Data will be converted from wide to long. Select repeated measures and enter new variable name.")

activeVars <- c(factors,rptMeasures)


rptMeasures <- tk_select.list(choices = symdiff(factors,colnames(df)),multiple=TRUE)
activeVars <- c(factors,rptMeasures)
ttkentry()
tk_messageBox(type=("ok"),message="Data will be converted from wide to long. Select repeated measures and enter new variable name.")

colnames(df) == activeVars

inputs <- function(){
  
  xvar <- tclVar("")
  
  
  tt <- tktoplevel()
  tkwm.title(tt,"Variable Name")
  x.entry <- tkentry(tt, textvariable=xvar)
  
  reset <- function()
  {
    tclvalue(xvar)<-""
    
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    x <- toString(tclvalue(xvar))
    
    e <- parent.env(environment())
    e$x <- x
    
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  
  tkgrid(tklabel(tt,text="Enter Variable Name"),columnspan=2)
  tkgrid(tklabel(tt,text="Variable name"), x.entry, pady = 10, padx =10)
  
  tkgrid(submit.but, reset.but)
  
  tkwait.window(tt)
  return(c(x))
}


varName <- inputs()



#SetsReps <- SetsReps[,0:6]
#SetsReps <- SetsReps %>% convert_as_factor(UniqueID,Sex,Condition)
SetsReps <- SetsReps %>% rename(
  "1" = S1reps,
  "2" = S2reps,
  "3" = S3reps
)
SetsRepsLong<- SetsReps %>% pivot_longer(
  cols=`1`:`3`,
  names_to="Set",
  values_to="Reps"
)
SetsRepsLong<- SetsRepsLong %>% convert_as_factor(Set)
SetsRepsLong$Sex <- SetsRepsLong$Sex %>% case_match("0"~"M","1"~"F")

axis_title_size <- 20
axis_text_size <- 20
title_text_size <- 20
legend_text_size <- 20
graph_title_size<- 25


ggplot(data=SetsRepsLong,aes(x=Condition,fill=Set, y=Reps)) + 
  
  geom_boxplot(color="black",alpha=.65,outlier.shape = NA,size=1.25) + 
  geom_point(size=3.75,position=position_jitterdodge(), aes(color=Sex,group=Set)) + 
  scale_color_manual(values = c("M"="#00205B","F"="#C5113A")) + 
  
  xlab("Condition") + 
  ylab("Reps") + 

  
  theme_classic() + 
  theme(text=element_text(family="Arial Narrow"))+
  theme(plot.background = element_rect(fill = '#DAF1F8', colour = '#DAF1F8')) +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent')) + 
  theme(legend.background = element_rect(fill = 'transparent', colour = 'transparent')) + 
  theme(axis.title=element_text(face="bold",size=axis_title_size))+
  theme(axis.text=element_text(face="bold",size=axis_text_size))+ 
  theme(legend.title = element_text(hjust=.5,face="bold",size=legend_text_size))+
  theme(legend.text = element_text(hjust=.5,face="bold",size=legend_text_size))+
  theme(legend.position=c(.9,.85))+
  theme(legend.key.size= unit(.9,"cm"))+
  
  ggtitle("Reps completed in each set") +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=graph_title_size)) + 
  
  scale_fill_manual(values = c("1"="#becccc","2"="#778080","3"="#2f3030"))+
  annotate('text', x = 1, y = 40, label =  paste0("*"),size=20)+
  annotate('text', x = 1.22, y = 40, label =  paste0("*"),size=20)+
  annotate('text', x = 1.29, y = 41.5, label =  paste0("#"),size=12)

#  theme(title=element_text(size=20,face="bold"),
#plot.title=element_text(hjust=.5),
#axis.title.x=element_text(size=16,face="bold"),
#axis.title.y=element_text(size=20,face="bold"),
#axis.ticks.y =element_line(linewidth=1),
#axis.text.x = element_text(size=16,face="bold",color="black"),
#axis.text.y = element_text(size=16,face="bold",color="black"),
#panel.background = element_rect(fill="#DAF1F8"),
#plot.background = element_rect(fill="#DAF1F8"))
#  scale_y_continuous(breaks = c(1000,2000,3000,4000,5000,6000),
#limits=c(1000,5800)

#geom_jitter(width=.2,height=.2,size=4) + 

############AMP GRAPH###############

dataWBClean_warmup<-read_excel("D://High-Low Exercise/JR High Low MVIC refs.xlsx",col_names=T,sheet="Norm.Warmup")
Norm_Warmup_df <- dataWBClean_warmup
Norm_Warmup_df <- Norm_Warmup_df %>% convert_as_factor(UniqueID,Sex,Set,Rep,Condition)

AMP_filter <- Norm_Warmup_df[,0:5]
AMP_filter<-duplicated(AMP_filter)
Norm_Warmup_df <- Norm_Warmup_df[!AMP_filter,]


Norm_Warmup_df$Sex <- Norm_Warmup_df$Sex %>% case_match("1"~"F","2"~"M")
Norm_Warmup_df$Condition <- Norm_Warmup_df$Condition %>% case_match("1"~"80%","0"~"30%")
Norm_Warmup_df$Rep <- Norm_Warmup_df$Rep %>% case_match("1"~"First","4"~"Middle","7"~"Last")
Norm_Warmup_df<- Norm_Warmup_df %>% convert_as_factor(Sex,Condition,Rep)
view(Norm_Warmup_df)


Q <- quantile(Norm_Warmup_df$Norm.warmup, probs=c(.25, .75), na.rm = TRUE)

iqr<-IQR(Norm_Warmup_df$Norm.warmup,na.rm=TRUE)

Norm_Warmup_outlier_RM <- subset(Norm_Warmup_df, Norm_Warmup_df$Norm.warmup > (Q[1] - 3*iqr) 
                         & Norm_Warmup_df$Norm.warmup < (Q[2]+3*iqr))
Norm_Warmup_outlier_RM$Norm.warmup <- Norm_Warmup_outlier_RM$Norm.warmup * 100

sigVector =  c("***"=0.001,
                "**"=0.01, "*"=0.05)





ggplot(data=Norm_Warmup_outlier_RM,aes(x=Condition,fill=Set, y=Norm.warmup)) + 
  
  geom_boxplot(color="black",alpha=.65,outlier.shape = NA,size=1.25) + 
  geom_point(size=3.75,position=position_jitterdodge(),
             aes(color=Sex,group=Set)) + 
  scale_color_manual(values = c("M"="#00205B","F"="#C5113A")) + 
  
  xlab("Condition") + 
  ylab("EMG AMP, normalized % to warmup") + 
  ylim(0,850)+

  theme_classic() + 
  theme(text=element_text(family="Arial Narrow"))+
  theme(plot.background = element_rect(fill = '#DAF1F8', colour = '#DAF1F8')) +
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent')) + 
  theme(legend.background = element_rect(fill = 'transparent', colour = 'transparent')) + 
  theme(axis.title=element_text(face="bold",size=axis_title_size))+
  theme(axis.text=element_text(face="bold",size=axis_text_size))+ 
  theme(legend.title = element_text(hjust=.5,face="bold",size=legend_text_size))+
  theme(legend.text = element_text(hjust=.5,face="bold",size=legend_text_size))+
  theme(legend.position=c(.95,.85))+
  theme(legend.key.size= unit(.9,"cm"))+

  ggtitle("EMG Amplitude") +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=graph_title_size)) + 
  geom_signif(comparisons = list(c("30%","80%")),map_signif_level = sigVector[3],
              textsize=10,size=1.5) +
  scale_fill_manual(values = c("1"="#becccc","2"="#778080","3"="#2f3030"))



