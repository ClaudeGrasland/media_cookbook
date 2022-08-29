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


## Tableau 1 ------------------

# 1.1 number of news
dt1<-hc[,.(tot=sum(news)),.(who)]

# 1.2 number of news with at least one region
dt2<-hc[where1!="_no_",.(sta=sum(news)),.(who)]

# 1.3 number of news with at least one state
dt3<-hc[where2!="_no_",.(reg=sum(news)),.(who)]

# 1.4 number of news with at least one state & one region
dt4<-hc[where1!="_no_" & where2 !="_no_",.(stareg=sum(news)),.(who)]

dt<-dt1 %>% left_join(dt2) %>% left_join(dt3) %>% left_join(dt4) %>%
        mutate(regpct = 100*reg/tot,
               stapct = 100*sta/tot,
               staregpct = 100*stareg/tot)
writexl::write_xlsx(dt,"cybergeo/tab1.xlsx")
cor.test(dt$regpct,dt$stapct)


## Annexe au tableau 1 : croisement région-pays

dt<-hc[,.(N=sum(news)), .(regOK=where2!="_no_",
                          staOK = where1!="_no_")]
mat<-dcast(dt, formula = regOK~staOK)

mat<-as.matrix(mat[,-1])
class(mat)
mod<-chisq.test(mat)
summary(mod)
mod$expected
mat






