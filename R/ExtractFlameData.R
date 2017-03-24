# =============================================
# Code to Extract Flame Data from Loken Desktop
# Output rdata file of all spatial summaries
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
rownames(merged_summary)<-c()


# Save to Git folder
saveRDS(merged_summary, file='Data/FlameSpatialSummaries.rds')
write.table(merged_summary, file='Data/FlameSpatialSummaries.csv')

