###Combinging all complete .csv's into one document with treatment columns

####################Start Here#####################



#Run1=read.csv("CCND PMC 2025 rd1 calcs full.csv")
Run2=read.csv("CCND PMC 2025 rd2 calcs full.csv")
Run3=read.csv("CCND PMC 2025 rd3 calcs full.csv")
Run4=read.csv("CCND PMC 2025 rd4 calcs full.csv")
Run5=read.csv("CCND PMC 2025 rd5 calcs full.csv")
Run6=read.csv("CCND PMC 2025 rd6 calcs full.csv")
#Run7=read.csv("CCND PMC 2025 rd7 calcs full.csv")
#Run8=read.csv("CCND PMC 2025 rd8 calcs full.csv")

trtcode = read.csv("List of samples CCND.csv") 
str(trtcode)
str(Run3)    


mergeAll=rbind(Run2,Run3,Run4,Run5,Run6)
#,,Run4,Run5,Run6,Run7,Run8
head(mergeAll)
mergeClean=select(mergeAll,plot,depth.cm,sample.id,ugC.g.day)


write.csv(mergeClean, "PMC_CCND_all_calcs_17Dec25.csv",
          row.names = FALSE)


#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_CCND_all_calcs_17Dec25 cleaned.csv", stringsAsFactors = FALSE) 

num <- all.data$number 

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
sample.id <- read.csv("List of samples CCND.csv", 
                      stringsAsFactors = FALSE)

#Join the mean and variability dataset with the sample name key 
pmc.var <- full_join(sample.id, mean.cv, by = "sample.id")

#keep only samples that have CV > 0. 
pmc.var <- subset(pmc.var, sample.cv > 0) 

write.csv(pmc.var, "PMC_CCND_all_calcs_CV_17Dec25.csv")

##now look at the document for sampels that need to be looked into further. 
###example
###if CV > 20, maybe one of the samples is an outlire, once deleted no longer on the list 

write.csv(missing, "CCND_missing.csv", row.names = FALSE)


