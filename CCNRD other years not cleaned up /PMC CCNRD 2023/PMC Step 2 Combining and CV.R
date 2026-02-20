###Combinging all complete .csv's into one document with treatment columns

####################Start Here#####################



Run1=read.csv("CCNRD PMC 2023 rd1 calcs full.csv")
Run2=read.csv("CCNRD PMC 2023 rd2 calcs full.csv")
Run3=read.csv("CCNRD PMC 2023 rd3 calcs full.csv")
Run4=read.csv("CCNRD PMC 2023 rd4 calcs full.csv")
Run5=read.csv("CCNRD PMC 2023 rd5 calcs full.csv")
Run6=read.csv("CCNRD PMC 2023 rd6 calcs full.csv")
Run8=read.csv("CCNRD PMC 2023 rd8 calcs full.csv")
Run9=read.csv("CCNRD PMC 2023 rd9 calcs full.csv")
trtcode = read.csv("CCNRD_samplelist.csv") 
str(trtcode)
str(Run9)    


mergeAll=rbind(Run1,Run2,Run3,Run4,Run5,Run6,Run8,Run9)
head(mergeAll)
mergeClean=select(mergeAll,plot,depth_cm,sample.id,ugC.g.day)


write.csv(mergeAll, "PMC_CCNRD_all_calcs_25Jan24.csv",
          row.names = FALSE)


#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_CCNRD_all_calcs_25Jan24.csv", stringsAsFactors = FALSE) 
RTNC.data<-read.csv("RTNC_run10_calcs.csv", stringsAsFactors = FALSE)

num <- all.data$number 
num2<- RTNC.data

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
full.means <- all.data %>% 
  group_by(sample.id) %>%
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

no.NA = all.data[!is.na(all.data$ugC.g.day),]


#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressesed as a percent. 
add.cv <- no.NA %>% 
  group_by(sample.id) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv) 

#bring in full sample names from the name master. 
sample.id <- read.csv("CCNRD_samplelist.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "PMC_CCNRD_all_calcs_CV_25Jan24.csv")

##now look at the document for sampels that need to be looked into further. 
###example
###if CV > 20, maybe one of the samples is an outlire, once deleted no longer on the list 

write.csv(missing, "CCNRD_missing.csv", row.names = FALSE)



###### RTNC data CV
#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
RTNC.data<-read.csv("RTNC_run11_calcs.csv", stringsAsFactors = FALSE)

num2<- RTNC.data

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
full.means <- RTNC.data %>% 
  group_by(sample.id) %>% 
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

no.NA = RTNC.data[!is.na(RTNC.data$ugC.g.day),]



#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressesed as a percent. 
add.cv <- RTNC.data %>% 
  group_by(sample.id) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv) 

#bring in full sample names from the name master. 
sample.id <- read.csv("List of samples RTNC.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "RTNC_PMC_all_calcs_CV_05Feb24.csv")





#histogram with all samples 
ggplot(pmc.var, aes(sample.cv))+ 
  geom_histogram(binwidth = 1, na.rm = TRUE) 

#Isolate the samples with >20% CV 
gtr.20.cv <- subset(pmc.var, sample.cv >20) 

#histogram samples >20% CV 
ggplot(gtr.20.cv, aes(sample.cv))+ geom_histogram(binwidth = 5, na.rm = TRUE) 

















