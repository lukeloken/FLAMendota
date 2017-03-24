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

# SPC_columns<-names(merged_summary)[grep('SPC', names(merged_summary))]
# ChlA_columns<-names(merged_summary)[grep('ChlA', names(merged_summary))]
# BGA_columns<-names(merged_summary)[grep('BGA', names(merged_summary))]
# RFU_columns<-names(merged_summary)[grep('RFU', names(merged_summary))]
# ChlA_columns<-ChlA_columns[!ChlA_columns %in% RFU_columns] #Remove RFU columns
# BGA_columns<-BGA_columns[!BGA_columns %in% RFU_columns] #Remove RFU columns
# 
# SPC_h_columns<-SPC_columns[grep('_h', SPC_columns)]
# SPC_t_columns<-SPC_columns[grep('_t', SPC_columns)]
# SPC_RAW_columns<-SPC_columns[!SPC_columns %in% c(SPC_h_columns, SPC_t_columns)]
# 
# ChlA_h_columns<-ChlA_columns[grep('_h', ChlA_columns)]
# ChlA_t_columns<-ChlA_columns[grep('_t', ChlA_columns)]
# ChlA_RAW_columns<-ChlA_columns[!ChlA_columns %in% c(ChlA_h_columns, ChlA_t_columns)]
# 
# BGA_h_columns<-BGA_columns[grep('_h', BGA_columns)]
# BGA_t_columns<-BGA_columns[grep('_t', BGA_columns)]
# BGA_RAW_columns<-BGA_columns[!BGA_columns %in% c(BGA_h_columns, BGA_t_columns)]
# 
# merged_summary$SPCScm_h<-rowMeans(merged_summary[SPC_h_columns], na.rm=T)
# merged_summary$SPCScm_t<-rowMeans(merged_summary[SPC_t_columns], na.rm=T)
# merged_summary$SPCuScm<-rowMeans(merged_summary[SPC_RAW_columns], na.rm=T)
# 
# merged_summary$ChlAgL_h<-rowMeans(merged_summary[ChlA_h_columns], na.rm=T)
# merged_summary$ChlAgL_t<-rowMeans(merged_summary[ChlA_t_columns], na.rm=T)
# merged_summary$ChlAugL<-rowMeans(merged_summary[ChlA_RAW_columns], na.rm=T)
# 
# merged_summary$BGAPCgL_h<-rowMeans(merged_summary[BGA_h_columns], na.rm=T)
# merged_summary$BGAPCgL_t<-rowMeans(merged_summary[BGA_t_columns], na.rm=T)
# merged_summary$BGAPCgL<-rowMeans(merged_summary[BGA_RAW_columns], na.rm=T)
# 
# head(merged_summary)
# str(merged_summary)

CH4_Only<-merged_summary[!is.na(merged_summary$XCH4Dppm_t),]
CO2_Only<-merged_summary[!is.na(merged_summary$XCO2Dppm_t),]
NO3_Only<-merged_summary[!is.na(merged_summary$NITRATEM),]

YSIList<-split(merged_summary, merged_summary$Statistic)
CH4List<-split(CH4_Only, CH4_Only$Statistic)
CO2List<-split(CO2_Only, CO2_Only$Statistic)
NO3List<-split(NO3_Only, NO3_Only$Statistic)


# merged_mean<-merged_summary[merged_summary$Statistic=='Mean',]
# merged_min<-merged_summary[merged_summary$Statistic=='Min',]
# merged_q1<-merged_summary[merged_summary$Statistic=='Q1',]
# merged_median<-merged_summary[merged_summary$Statistic=='Median',]
# merged_q3<-merged_summary[merged_summary$Statistic=='Q3',]
# merged_max<-merged_summary[merged_summary$Statistic=='Max',]
# merged_sd<-merged_summary[merged_summary$Statistic=='sd',]


plot(YSIList$Mean$Date, YSIList$Mean$TempC, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$NITRATEM, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$XCO2Dppm_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$CO2St_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$XCH4Dppm_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$CH4St_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$BGAPCRFU_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$ChlARFU_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$fDOMRFU_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$SPCScm_t, type="b")
plot(YSIList$Mean$Date, YSIList$Mean$ODOmgL, type="b")

points(YSIList$Mean$Date, YSIList$Mean$XCO2Dpp, type="l", col="red")
points(YSIList$Mean$Date, YSIList$Mean$XCH4Dppm_t, type="l", col="red")



#Methane moving boxplots

ch4_ylim<-range(c(CH4List$Mean$CH4St_t, CH4List$Q1$CH4St_t, CH4List$Q3$CH4St_t), na.rm=T)

plot(CH4List$Mean$Date, CH4List$Mean$CH4St_t, type="n", pch=15, col="darkred", ylim=ch4_ylim)

polyx<-c(CH4List$Q1$Date, rev(CH4List$Q3$Date))
polyy<-c(CH4List$Q1$CH4St_t, rev(CH4List$Q3$CH4St_t))
polygon(polyx, polyy, border=NA, col="lightgrey")
points(CH4List$Median$Date, CH4List$Median$CH4St_t, type="l", pch=15, col="black", lwd=2)
points(CH4List$Mean$Date, CH4List$Mean$CH4St_t, type="l", pch=15, col="red", lwd=2)




