library(dplyr)
library(tidyr)


## Load BDI and features:
data <- readRDS(file = "results_n5.rds")
InfoBDI <- readRDS(file = "InfoBDI.rds")

## Combine data and info bdi:
InfoBDI<-InfoBDI%>%
  gather('bdi1','bdi4','bdi8','bdi12','bdi16', key = "session",value = "BDI")%>%
  mutate(session=as.numeric(substring(session,4))) # sessie nummer
data <- data%>%
  left_join(InfoBDI, by = c("patient","session"))


## Standardize values per person
## Needed for comparing between patients
data<-data%>%
  group_by(patient,session,speaker)%>%
  mutate(pvar=pvar-mean(pvar, na.rm = TRUE),
         pmean=pmean-mean(pmean, na.rm = TRUE),
         pmedian=pmedian-mean(pmedian, na.rm = TRUE),
         srate=srate-mean(srate, na.rm = TRUE))