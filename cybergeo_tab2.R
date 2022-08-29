### Cybergeo tables and figures

library(knitr)
library(dplyr, quietly=TRUE)
library(data.table)
library(FactoMineR)


## Preparation 

# Load Hypercube and select period
hc<-readRDS("hypercube/hc_sta_reg.RDS")
hc %>% filter(is.na(when)==F, when < as.Date("2021-01-01"))
sum(hc$news)

# Define weights correction for equal contribution of media
wgt<-hc[,list(coeff = sum(news)), list(who)]  %>% 
  mutate(coeff=mean(coeff)/coeff)
hc<-left_join(hc,wgt)
hc$news_wgt<-hc$news*hc$coeff

# Load table of label and choose language
reg_def<-read.table("dict/worldgeo_def_V2.csv", sep=";",quote = '"', encoding = "UTF-8",header=T)
tab_def<-reg_def %>% filter(lang=="fr") %>% select(code,type,label)
tab_def<-tab_def[duplicated(tab_def$code)==F,]

# Add labels to hypercube
hc<-merge(hc, tab_def, by.x="where2", by.y="code",all.x=T, all.y=F)
sum(hc$news_wgt)



## Tableau 2 ------------------------------------

dt<-hc[where2 !="_no_",.(N1=sum(news_wgt), N2=sum(news)), .(label,type)]

dt<-dt[,.( reg=label,type = type,
                      N1=N1, R1=rank(-N1),P1=N1/sum(N1),
                      N2=N2, R2=rank(-N2), P2=N2/sum(N2))]

dt<-dt[order(R1),]
writexl::write_xlsx(dt,"cybergeo/tab2.xlsx")



