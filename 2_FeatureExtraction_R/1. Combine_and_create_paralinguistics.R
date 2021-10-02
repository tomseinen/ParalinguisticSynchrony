library(dplyr)
library(tidyr)
library(stringr)

annotation<-read.csv("AudioClusters_v3.csv",header = T)

patientids<-unique(annotation$patient)

## Set number of parts
n<-5
## set the minimum quality
minimumquality<-1

All_patient_sessions<-NULL

for (patientid in patientids){
  print(patientid)
  
  #Get the session result file names created in Python
  pathstr<-paste0("Location/of/python/result/files/",patientid)
  fl<-list.files(path=pathstr,
                 pattern=".csv", 
                 all.files=FALSE,
                 full.names=TRUE)
  pt_annotation<- annotation %>% # get the annotations for the patient
    filter(patient==patientid)
  
  #order and select the file names
  ss<-unlist(str_extract(fl, "(s|S)[0-9]{1,2}"))
  fl<-fl[order(nchar(ss),ss)] # Order the session file names on session number
  fl<-fl[str_detect(fl, "(s|S)[0-9]{1,2}")] #take only the s# files
  print(fl) # print all the files
  
  # Go over the csv files
  for (csvfilenr in c(1:length(fl))){
    resultcsv <- read.csv(fl[csvfilenr], header = T) #Get the session result file
    
    sessionnr <- pt_annotation$session[csvfilenr] #get the session number
    print(sessionnr)
    
    # select sessions of good enough quality
    if(pt_annotation$cijfer[pt_annotation$session==sessionnr]<=minimumquality){
      
      # load the annotation
      ses_pt_annotation<- pt_annotation %>% # get the annotation
        filter(session==sessionnr) %>% # of the session
        gather('c0','c1','c2','c3', key = 'cluster', value = 'speaker')%>% # 
        mutate(cluster=as.numeric(gsub("c", "", cluster)))%>%
        select(cluster,speaker)%>%
        na_if("")
      
      # annotate the pitch file
      result<-resultcsv%>%
        left_join(ses_pt_annotation, by='cluster')
      
      # Split the session in bins
      fbin<-cut(result$t,breaks = seq(0,result$t[nrow(result)],by = result$t[nrow(result)]/n)) # Determine the number of bin breaks
      result$bin<-as.integer(fbin) # assign the bin number to the result table
      
      ##=== Calculate total speachrate ===##
      prep<-result%>%
        filter(!is.na(speaker))%>%
        na_if(0)%>%
        group_by(speaker)%>%
        mutate(change=case_when(
          !is.na(p) & is.na(lag(p,1)) ~ 1,
          TRUE ~ 0))
      srate<-prep%>%group_by(bin,speaker)%>%
        summarise(srate=(sum(change)/n())*100)%>%
        group_by(bin)%>%
        filter(n() >= 2)      
      
      ##=== Filter the pitch ===#
      
      filt<-result%>%
        na_if(0)
      
      ##=== calculate the features ===##
      
      # The mean
      agg_mean<-filt %>%
        filter(!is.na(speaker))%>%
        group_by(bin,speaker)%>%
        summarise(pmean=mean(p, na.rm = T))%>%
        group_by(bin)%>%
        filter(n() >= 2)%>%
        filter(!any(is.na(pmean)))
      
      #print(table(agg_mean$speaker))
      
      # The median
      agg_median<-filt %>%
        filter(!is.na(speaker))%>%
        group_by(bin,speaker)%>%
        summarise(pmedian=median(p, na.rm = T))%>%
        group_by(bin)%>%
        filter(n() >= 2)%>%
        filter(!any(is.na(pmedian)))
      
      #print(table(agg_median$speaker))
      
      # the variance
      agg_var<-filt %>%
        filter(!is.na(speaker))%>%
        group_by(bin,speaker)%>%
        summarise(pvar=sd(p, na.rm = T))%>%
        group_by(bin)%>%
        filter(n() >= 2)%>%
        filter(!any(is.na(pvar)))
      
      #print(table(agg_var$speaker))
      
      # combine the aggregations
      agg<-agg_mean %>%
        left_join(agg_median, by=c("bin","speaker"))%>%
        left_join(agg_var, by=c("bin","speaker"))%>%
        left_join(srate, by=c("bin","speaker"))%>%
        mutate(patient=patientid)%>%
        mutate(session=sessionnr)
      
      if(is.null(All_patient_sessions)){
        All_patient_sessions<-agg
      } else {
        All_patient_sessions<-bind_rows(All_patient_sessions,agg)
      }
    } # if good quality
  } # for sessions
} # for patients

##=== some postprosessing ===##
All<-All_patient_sessions%>%
  select(patient,session,bin,speaker,pmedian,pmean,pvar,srate)%>%
  arrange(patient, session,bin,speaker)

# saving the results
saveRDS(All,file = "results_n5.rds")
All<-readRDS(file = "results_n5.rds") #test loading


###### laad en save bdi ######
library(foreign)

BDIdata = read.spss('BDI_CONDITIE.sav', to.data.frame=TRUE)
BDIdata<-BDIdata[,c(2:5,117:121)]

datavragenlijst<- read.spss('databestand_vragenlijstenV2.sav', to.data.frame=TRUE)
datavragenlijst$Clientnr[81]<-109206
BDIdata$Behandelaar<-datavragenlijst[which(datavragenlijst$Clientnr %in% BDIdata$Clientnr),"Behandelaar"]

InfoAndBDI<-BDIdata%>%
  rename(patient=Clientnr,age=Leeftijd,gender=Geslacht,type=Conditie,name_therapist=Behandelaar,bdi1=bdibase,bdi4=bdis4,bdi8=bdis8,bdi12=bdis12,bdi16=bdis16)%>%
  select(patient, age,gender,name_therapist,type,bdi1,bdi4,bdi8,bdi12,bdi16)%>%
  arrange(patient,age,gender,name_therapist,type)

saveRDS(InfoAndBDI,file = "InfoBDI.rds")
