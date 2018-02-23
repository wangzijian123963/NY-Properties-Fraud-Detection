library(dplyr)
library(stringr)
# load data set
dat<-read.csv("NY_property.csv")
str(dat)
dat$BBLE<-as.character(dat$BBLE)
borough<-as.factor(substr(dat$BBLE,1,1))
boro_names<-c("MANHATTAN","BRONX","BROOKLYN","QUEENS","STATEN ISLAND")
boro_name<-rep("",nrow(dat))
for (i in 1:nrow(dat)){
  if (borough[i]==1) {
    boro_name[i]=boro_names[1]
  }
  else if (borough[i]==2) {
    boro_name[i]=boro_names[2]
  }
  else if (borough[i]==3) {
    boro_name[i]=boro_names[3]
  }
  else if (borough[i]==4) {
    boro_name[i]=boro_names[4]
  }
  else {
    boro_name[i]=boro_names[5]
  }
}

#adding new columns
dat$BOROUGH<-borough
dat$BORO_NAME<-as.factor(boro_name)
#delete useless columns
dat<-dat[,c(1,31,32,3,4,7,8,9,10,11,12,13,14,19,21,22)]
#data with na zip code
no_zip<-dat$RECORD[is.na(dat$ZIP)]
# 1-MANHATTAN: 10000
# 2-BRONX: 10400
# 3-BROOKLYN: 11200
# 4-QUEENS: 11300
# 5-STATEN ISLAND: 10300
record<-dat$RECORD
for (i in 1:nrow(dat)){
  if (record[i] %in% no_zip){
    if (borough[i]==1) {
      dat$ZIP[i]=10000
    }
    else if (borough[i]==2) {
      dat$ZIP[i]=10400
    }
    else if (borough[i]==3) {
      dat$ZIP[i]=11200
    }
    else if (borough[i]==4) {
      dat$ZIP[i]=11300
    }
    else {
      dat$ZIP[i]=10300
    }
  }
}
# new variable zip3
dat$ZIP3<-substr(dat$ZIP,1,3)

#calculating avg of variables
zip3<-levels(as.factor(dat$ZIP3))
avgLTFRONT<-avgLTDEPTH<-avgSTORIES<-avgFULLVAL<-avgAVLAND<-avgAVTOT<-avgBLDFRONT<-avgBLDDEPTH<-rep(0,13)
for (i in 1:length(zip3)) {
  dat_i=dat[dat$ZIP3==zip3[i],]
  avgLTFRONT[i]=mean(dat_i$LTFRONT[dat_i$LTFRONT!=0])
  avgLTDEPTH[i]=mean(dat_i$LTDEPTH[dat_i$LTDEPTH!=0])
  avgSTORIES[i]=round(mean(dat_i$STORIES[!is.na(dat_i$STORIES)]),digits=0)
  avgFULLVAL[i]=mean(dat_i$FULLVAL[dat_i$FULLVAL!=0])
  avgAVLAND[i]=mean(dat_i$AVLAND[dat_i$AVLAND!=0])
  avgAVTOT[i]=mean(dat_i$AVTOT[dat_i$AVTOT!=0])
  avgBLDFRONT[i]=mean(dat_i$BLDFRONT[dat_i$BLDFRONT!=0])
  avgBLDDEPTH[i]=mean(dat_i$BLDDEPTH[dat_i$BLDDEPTH!=0])
}
avg_tlb<-data.frame(cbind(zip3,avgLTFRONT,avgLTDEPTH,avgSTORIES,avgFULLVAL,avgAVLAND,avgAVTOT,avgBLDFRONT,avgBLDDEPTH))
# since stories, bldfront and blddepth for zip3 338 are all missing, and the address for all 3 zip3 338 is west 16th street, which should has a zip code of 10011, 
# i use the avgSTORIES, avgBLDFRONT, avgBLDDEPTH of zip3 100 to replace the avg of these fields in zip3 338.
avg_tlb$avgSTORIES[13]<-avg_tlb$avgSTORIES[1]
avg_tlb$avgBLDFRONT[13]<-avg_tlb$avgBLDFRONT[1]
avg_tlb$avgBLDDEPTH[13]<-avg_tlb$avgBLDDEPTH[1]

# change variable type
avg_tlb$avgLTFRONT<-as.numeric(as.character(avg_tlb$avgLTFRONT))
avg_tlb$avgLTDEPTH<-as.numeric(as.character(avg_tlb$avgLTDEPTH))
avg_tlb$avgSTORIES<-as.numeric(as.character(avg_tlb$avgSTORIES))
avg_tlb$avgFULLVAL<-as.numeric(as.character(avg_tlb$avgFULLVAL))
avg_tlb$avgAVLAND<-as.numeric(as.character(avg_tlb$avgAVLAND))
avg_tlb$avgAVTOT<-as.numeric(as.character(avg_tlb$avgAVTOT))
avg_tlb$avgBLDFRONT<-as.numeric(as.character(avg_tlb$avgBLDFRONT))
avg_tlb$avgBLDDEPTH<-as.numeric(as.character(avg_tlb$avgBLDDEPTH))

#filling missing value
record<-numeric()
LTFRONT<-LTDEPTH<-STORIES<-FULLVAL<-AVLAND<-AVTOT<-BLDFRONT<-BLDDEPTH<-numeric()
for (i in 1:length(zip3)){
  dat_i=dat[dat$ZIP3==zip3[i],]
  dat_i$LTFRONT[dat_i$LTFRONT==0]<-avg_tlb$avgLTFRONT[i]
  dat_i$LTDEPTH[dat_i$LTDEPTH==0]<-avg_tlb$avgLTDEPTH[i]
  dat_i$STORIES[is.na(dat_i$STORIES)]<-avg_tlb$avgSTORIES[i]
  dat_i$FULLVAL[dat_i$FULLVAL==0]<-avg_tlb$avgFULLVAL[i]
  dat_i$AVLAND[dat_i$AVLAND==0]<-avg_tlb$avgAVLAND[i]
  dat_i$AVTOT[dat_i$AVTOT==0]<-avg_tlb$avgAVTOT[i]
  dat_i$BLDFRONT[dat_i$BLDFRONT==0]<-avg_tlb$avgBLDFRONT[i]
  dat_i$BLDDEPTH[dat_i$BLDDEPTH==0]<-avg_tlb$avgBLDDEPTH[i]
  record<-c(record,dat_i$RECORD)
  LTFRONT<-c(LTFRONT,dat_i$LTFRONT)
  LTDEPTH<-c(LTDEPTH,dat_i$LTDEPTH)
  STORIES<-c(STORIES,dat_i$STORIES)
  FULLVAL<-c(FULLVAL,dat_i$FULLVAL)
  AVLAND<-c(AVLAND,dat_i$AVLAND)
  AVTOT<-c(AVTOT,dat_i$AVTOT)
  BLDFRONT<-c(BLDFRONT,dat_i$BLDFRONT)
  BLDDEPTH<-c(BLDDEPTH,dat_i$BLDDEPTH)
}
new_fields<-data.frame(cbind(record,LTFRONT,LTDEPTH,STORIES,FULLVAL,AVLAND,AVTOT,BLDFRONT,BLDDEPTH))
new_fields<-new_fields%>%
  arrange(record)

# construct new dataset
dat$LTFRONT<-new_fields$LTFRONT
dat$LTDEPTH<-new_fields$LTDEPTH
dat$STORIES<-new_fields$STORIES
dat$FULLVAL<-new_fields$FULLVAL
dat$AVLAND<-new_fields$AVLAND
dat$AVTOT<-new_fields$AVTOT
dat$BLDFRONT<-new_fields$BLDFRONT
dat$BLDDEPTH<-new_fields$BLDDEPTH

# export data
write.csv(dat,"cleaned_data_v3.csv")
