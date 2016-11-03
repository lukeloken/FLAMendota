# =============================================
# Code to Extract Flame Data from Loken Desktop
# Output rdata file
# More Comments...
# =============================================

# Load packages
library(gtools)

#### Merge all spatial summaries ####

data_dir<-"E:/Dropbox/FLAME_YaharaLakes/spatialsummaries"

spatial_summaries<-list.files(data_dir)

merged_summary<-as.data.frame(matrix(nrow=0, ncol=0))

for (summary_file in spatial_summaries){
  
  date<-as.Date(sub("cleaned.csv", "", sub("LakeMendota", "", summary_file)))
  if (date>"2016-01-01"){
    
    summary<-read.csv(paste(data_dir, summary_file, sep="/"), header=T, stringsAsFactors = F)
    str(summary)
    head(summary)
    rownames(summary)<-summary$Variable
    transposed_summary<-as.data.frame(t(subset(summary, select = -c(Variable))))
    transposed_summary$Date<-as.Date(rep(date, nrow(transposed_summary)))
    transposed_summary$Statistic<-rownames(transposed_summary)
    
    if (nrow(merged_summary)==0){
      merged_summary<-transposed_summary
    }
    else {
      merged_summary<-smartbind(merged_summary, transposed_summary, fill=NA)
    }
  }
}
merged_summary$Date<-as.Date(merged_summary$Date)
head(merged_summary)
str(merged_summary)

merged_mean<-merged_summary[merged_summary$Statistic=='Mean',]

plot(merged_mean$Date, merged_mean$TempC, type="b")
plot(merged_mean$Date, merged_mean$NITRATEM, type="b")
plot(merged_mean$Date, merged_mean$XCO2Dppm_t, type="b")
plot(merged_mean$Date, merged_mean$XCH4Dppm_t, type="b")
plot(merged_mean$Date, merged_mean$BGAPCRFU_t, type="b")
plot(merged_mean$Date, merged_mean$ChlARFU_t, type="b")
plot(merged_mean$Date, merged_mean$fDOMRFU_t, type="b")
