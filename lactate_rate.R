list.of.packages <- c("randomForest","sqldf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(randomForest)

Sys.setlocale("LC_TIME", 'en_US.UTF-8')
#73k.csv produced by this SQL:
#select dp.SUBJECT_ID, dp.SEX, dp.DOB, dp.DOD, dp.HOSPITAL_EXPIRE_FLG, ce.VALUE1NUM as lactate_value,ce.CHARTTIME as lactate_date from  "MIMIC2V26"."d_patients" dp left join "MIMIC2V26"."chartevents" ce  on dp.SUBJECT_ID=ce.SUBJECT_ID and ce.ITEMID=818 where ce.VALUE1NUM > 0 order by dp.SUBJECT_ID, ce.CHARTTIME

# Several lactate measurements per patient
data <- read.csv("73k.csv",sep=";")
data$SUBJECT_ID <- as.numeric(data$SUBJECT_ID)
data$VALUE1NUM <- as.numeric(data$LACTATE_VALUE)
data$realtime <- as.POSIXct(strptime(data$LACTATE_DATE, "%b %d, %Y %I:%M:%S"))

lactate_rate <- function(subject_id) {
  x <- data[data$SUBJECT_ID==subject_id,c("VALUE1NUM","realtime")]
  x <- x[order(x$realtime),]
  rate <- diff(x$VALUE1NUM)/as.numeric(diff(x$realtime))
  data.frame(subject_id=subject_id,min=min(rate),max=max(rate))
}

lactate_first_last <- function(subject_id) {
  x <- data[data$SUBJECT_ID==subject_id,c("VALUE1NUM","realtime")]
  x <- x[order(x$realtime),]
  data.frame(first=head(x$VALUE1NUM, 1),last=tail(x$VALUE1NUM,1))
}

patients <- sqldf("select distinct SUBJECT_ID, HOSPITAL_EXPIRE_FLG from data")
# produced by this SQL
# select subject_id, max(locate(DESCRIPTION, 'SEPTIC')) from  "MIMIC2V26"."icd9" dp group by subject_id order by subject_id

ps <- read.csv("patients_sepsis.csv", sep=";")
names(ps) <- c("X","SUBJECT_ID","SEPSIS")
ps$sepsis_index <- as.numeric(as.character(ps$SEPSIS))
ps$SUBJECT_ID <- as.numeric(ps$SUBJECT_ID)
ps <- ps[!is.na(ps$sepsis_index),]
ps$sepsis <- ps$sepsis_index > 0
ps <- ps[,c("SUBJECT_ID","sepsis")]

patients <- merge(patients,ps,by="SUBJECT_ID")

# List of patients with more than 1 lactate measurement
subjects <- sqldf("select SUBJECT_ID from data where VALUE1NUM > 0 group by subject_id having count(*) > 1")
# Should really use apply. 
for (sid in subjects$SUBJECT_ID) {
  df <- lactate_first_last(sid)
  if (nrow(df) == 1) {
    patients[patients$SUBJECT_ID==sid,"first_lactate"] <- df$first
    patients[patients$SUBJECT_ID==sid,"last_lactate"] <- df$last
  }
  df <- lactate_rate(sid)
  if (nrow(df) == 1) {
    patients[patients$SUBJECT_ID==sid,"min_rate"] <- df$min
    patients[patients$SUBJECT_ID==sid,"max_rate"] <- df$max
  }
}

patients <- patients[!is.na(patients$min_rate),]
patients <- patients[!is.na(patients$max_rate),]
patients <- patients[is.finite(patients$min_rate),]
patients <- patients[is.finite(patients$max_rate),]
randomForest(HOSPITAL_EXPIRE_FLG ~ min_rate+max_rate+first_lactate+last_lactate+sepsis, data=patients)


