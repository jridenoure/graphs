library("tidyverse")
library("tidyr")
library("readxl")
library("janitor")
library("tidyselect")
library("dplyr")
library("rstatix")
library("stringr")
library("emmeans")
library("lme4")
library("ez")
EMGdata <- read_excel("D://High-Low Exercise/NSCA US reformat 25.3.3.xlsx",
                      sheet="EMGData",col_names = TRUE)
ultrasoundData <- read_excel("D://High-Low Exercise/NSCA US reformat 25.3.3.xlsx",
                      sheet="ultrasoundData",col_names = TRUE)
MVICdata <- read_excel("D://High-Low Exercise/NSCA US reformat 25.3.3.xlsx",
                      sheet="MVIC",col_names = TRUE)

########## ULTRASOUND #############

Ultrasound<- ultrasoundData

MT<-((((Ultrasound %>% pivot_longer(starts_with("MT"),
                                   names_to=c(".value","time"),names_sep="_")) %>% select(Sex,UID,Condition,time:last_col()))) %>% convert_as_factor(Sex,UID,Condition,time)) %>% pivot_longer(cols=starts_with("MT"),names_to="Muscle",values_to="MT")

FT<-((((Ultrasound %>% pivot_longer(starts_with("FT"),
                                   names_to=c(".value","time"),names_sep="_")) %>% select(Sex,UID,Condition,time:last_col()))) %>% convert_as_factor(Sex,UID,Condition,time)) %>% pivot_longer(cols=starts_with("FT"),names_to="Muscle",values_to="FT")

EI<-((((Ultrasound %>% pivot_longer(starts_with("EI"),
                                   names_to=c(".value","time"),names_sep="_")) %>% select(Sex,UID,Condition,time:last_col()))) %>% convert_as_factor(Sex,UID,Condition,time)) %>% pivot_longer(cols=starts_with("EI"),names_to="Muscle",values_to="EI")

MeanMT<-((Ultrasound %>% pivot_longer(starts_with("mean.MT"),
                                    names_to=c(".value","time"),names_sep="_")) %>% select(Sex,UID,Condition,time:last_col())) %>% convert_as_factor(Sex,UID,Condition,time)

WtEI<-((Ultrasound %>% pivot_longer(starts_with("Weighted.EI"),
                                    names_to=c(".value","time"),names_sep="_")) %>% select(Sex,UID,Condition,time:last_col())) %>% convert_as_factor(Sex,UID,Condition,time)


MT$Muscle<-sapply((strsplit(MT$Muscle,"\\.")),"[[",2)
FT$Muscle<-sapply((strsplit(FT$Muscle,"\\.")),"[[",2)
EI$Muscle<-sapply((strsplit(EI$Muscle,"\\.")),"[[",2)

MT <- MT %>% convert_as_factor(Muscle)
FT <- FT %>% convert_as_factor(Muscle)
EI <- EI %>% convert_as_factor(Muscle)

##########ANOVAS############

###### MT #########
MT_anova <- (MT %>% anova_test(dv=MT,wid=UID,between=c(Condition,Sex),within=c(time,Muscle))) %>% get_anova_table(correction="GG")
MT_anova


###### FT #########
FT_anova <- (FT %>% anova_test(dv=FT,wid=UID,between=c(Condition,Sex),within=c(time,Muscle))) %>% get_anova_table(correction="GG")
FT_anova

###### EI #########
EI_anova <- (EI %>% anova_test(dv=EI,wid=UID,between=c(Condition,Sex),within=c(time,Muscle))) %>% get_anova_table(correction="GG")
EI_anova

###### MEAN MT #########
MeanMT_anova <- (MeanMT %>% anova_test(dv=mean.MT,wid=UID,between=c(Condition,Sex),within=time)) %>% get_anova_table(correction="GG")
MeanMT_anova

####### WEIGHTED EI ###########
WtEI_anova <- (WtEI %>% anova_test(dv=Weighted.EI,wid=UID,between=c(Condition,Sex),within=time)) %>% get_anova_table(correction="GG")
WtEI_anova

########## Mus Thickness POST HOCS ###########

MT_model = lmer(MT ~ Muscle*Sex*Condition*time+(1|UID),data=MT)
########### condition x time ############
emm_MT_condition_time<-emmeans(MT_model,list(pairwise~Condition*time),adjust="bonferroni")
emm_MT_condition_time
########### mus x time ############
emm_MT_mus_time<-emmeans(MT_model,list(pairwise~Muscle*time),adjust="bonferroni")
emm_MT_mus_time
########### sex x time ############
emm_MT_sex_time<-emmeans(MT_model,list(pairwise~Sex*time),adjust="bonferroni")
emm_MT_sex_time

########## ECHO INT POST HOCS ###########
EI_model = lmer(EI ~ Muscle*Sex*Condition*time+(1|UID),data=EI)

############ Sex x time ##############
emm_EI_sex_time<-emmeans(EI_model,list(pairwise~Sex*time),adjust="bonferroni")
emm_EI_sex_time
############ Sex x Mus ##############
emm_EI_sex_muscle<-emmeans(EI_model,list(pairwise~Sex*Muscle),adjust="bonferroni")
emm_EI_sex_muscle

############ Mean MT ##############
MeanMT_model = lmer(mean.MT ~ Sex*Condition*time+(1|UID),data=MeanMT)

########### Condition x Time ##########
emm_MeanMT_c_t<-emmeans(MeanMT_model,list(pairwise~Condition*time),adjust="bonferroni")
emm_MeanMT_c_t
####### Sex x Time #######
emm_MeanMT_s_t<-emmeans(MeanMT_model,list(pairwise~Sex*time),adjust="bonferroni")
emm_MeanMT_s_t

########### Weighted EI ############
WtEI_model = lmer(Weighted.EI ~ Sex*Condition*time+(1|UID),data=WtEI)
emm_WtEI_s_t<-emmeans(WtEI_model,list(pairwise~Sex*time),adjust="bonferroni")
emm_WtEI_s_t

############ Sex x Mus ##############
  emm_EI_sex_muscle<-emmeans(EI_model,list(pairwise~Sex*Muscle),adjust="bonferroni")
  emm_EI_sex_muscle
############ Sex x Mus ##############
emm_EI_sex_muscle<-emmeans(EI_model,list(pairwise~Sex*Muscle),adjust="bonferroni")
emm_EI_sex_muscle
############# trying an ANCOVA ############
#MT_wide <- (MT %>% pivot_wider(id_cols=c(UID,Sex,Condition,Muscle),names_from=time,values_from=MT)) %>% convert_as_factor(Muscle)
#dat<-na.omit(MT_wide)
#MT_ancova <- (MT_wide %>% anova_test(dv=Post,covariate=Pre,wid=UID,between=c(Condition,Sex),within=Muscle)) %>% get_anova_table(correction="GG")
#MT_ancova

#ezANOVA(data=MT_wide,dv=Post,wid=UID,between=c(Condition,Sex,Muscle),within_covariate=Pre,detailed=TRUE,type=2)

### debugging #####
#debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}
#debug_contr_error(MT_wide)

#debug_contr_error2 <- function (form, dat, subset_vec = NULL) {
  ## step 0
  if (!is.null(subset_vec)) {
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  }
  ## step 0 and 1
  dat_internal <- stats::lm(form, data = dat, method = "model.frame")
  attr(dat_internal, "terms") <- NULL
  ## rely on `debug_contr_error` for steps 2 to 4
  c(list(mf = dat_internal), debug_contr_error(dat_internal, NULL))
}
#debug_contr_error2(Post~Sex*Condition*Muscle+UID,MT_wide)