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
rownames(merged_summary)<-c()

# Merge columns that have the same variable
# Prior to 2016-06-29, mu sumbols in col name were converted to 'Angstrom Mu symbol'
# After 2016-06-29, Run_Flame code converted these names to 'u'

SPC_columns<-names(merged_summary)[grep('SPC', names(merged_summary))]
ChlA_columns<-names(merged_summary)[grep('ChlA', names(merged_summary))]
BGA_columns<-names(merged_summary)[grep('BGA', names(merged_summary))]
RFU_columns<-names(merged_summary)[grep('RFU', names(merged_summary))]
ChlA_columns<-ChlA_columns[!ChlA_columns %in% RFU_columns] #Remove RFU columns
BGA_columns<-BGA_columns[!BGA_columns %in% RFU_columns] #Remove RFU columns

SPC_h_columns<-SPC_columns[grep('_h', SPC_columns)]
SPC_t_columns<-SPC_columns[grep('_t', SPC_columns)]
SPC_RAW_columns<-SPC_columns[!SPC_columns %in% c(SPC_h_columns, SPC_t_columns)]

ChlA_h_columns<-ChlA_columns[grep('_h', ChlA_columns)]
ChlA_t_columns<-ChlA_columns[grep('_t', ChlA_columns)]
ChlA_RAW_columns<-ChlA_columns[!ChlA_columns %in% c(ChlA_h_columns, ChlA_t_columns)]

BGA_h_columns<-BGA_columns[grep('_h', BGA_columns)]
BGA_t_columns<-BGA_columns[grep('_t', BGA_columns)]
BGA_RAW_columns<-BGA_columns[!BGA_columns %in% c(BGA_h_columns, BGA_t_columns)]

merged_summary$SPCScm_h<-rowMeans(merged_summary[SPC_h_columns], na.rm=T)
merged_summary$SPCScm_t<-rowMeans(merged_summary[SPC_t_columns], na.rm=T)
merged_summary$SPCuScm<-rowMeans(merged_summary[SPC_RAW_columns], na.rm=T)

merged_summary$ChlAgL_h<-rowMeans(merged_summary[ChlA_h_columns], na.rm=T)
merged_summary$ChlAgL_t<-rowMeans(merged_summary[ChlA_t_columns], na.rm=T)
merged_summary$ChlAugL<-rowMeans(merged_summary[ChlA_RAW_columns], na.rm=T)

merged_summary$BGAPCgL_h<-rowMeans(merged_summary[BGA_h_columns], na.rm=T)
merged_summary$BGAPCgL_t<-rowMeans(merged_summary[BGA_t_columns], na.rm=T)
merged_summary$BGAPCgL<-rowMeans(merged_summary[BGA_RAW_columns], na.rm=T)

head(merged_summary)
str(merged_summary)

merged_mean<-merged_summary[merged_summary$Statistic=='Mean',]

plot(merged_mean$Date, merged_mean$TempC, type="b")
plot(merged_mean$Date, merged_mean$NITRATEM, type="b")
plot(merged_mean$Date, merged_mean$XCO2Dppm_t, type="b")
plot(merged_mean$Date, merged_mean$CO2St_t, type="b")
plot(merged_mean$Date, merged_mean$XCH4Dppm_t, type="b")
plot(merged_mean$Date, merged_mean$CH4St_t, type="b")
plot(merged_mean$Date, merged_mean$BGAPCRFU_t, type="b")
plot(merged_mean$Date, merged_mean$ChlARFU_t, type="b")
plot(merged_mean$Date, merged_mean$fDOMRFU_t, type="b")
plot(merged_mean$Date, merged_mean$SPCScm_t, type="b")
plot(merged_mean$Date, merged_mean$ODOmgL, type="b")
