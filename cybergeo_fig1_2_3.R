### Cybergeo tables and figures

library(knitr)
library(dplyr, quietly=TRUE)
library(data.table)
library(FactoMineR)
library(pheatmap)

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

# Correct bugs
hc$label[hc$label=="Machrek"]<-"Maghreb"
hc$label[hc$label=="Europe médiane"]<-"Europe centrale"

### préparation des données

dt<-hc[where2 !="_no_",.(N=sum(news_wgt)), .(label,who)]
dt<-dt[,.( reg=label,  N=round(N), R=rank(-N),P=N/sum(N)), .(who)]

# Selection of regions in top10 of at least one media
sel<-dt$reg[dt$R<11] %>% unique()
sel
dt2<-dt[dt$reg %in% sel,]

# Transformation en matrice
x<-dcast(dt2, formula = reg~who, value.var = "N", fill = 0)
mat<-as.data.frame(x[,-1])
row.names(mat)<-x$reg
mat
dim(mat)

## AFC
library(explor)
afc<-CA(mat,graph = F)
explor(afc)

##  CAHs basée sur l'AFC

#cah1 <- HCPC(afc,nb.clust = 5,graph = FALSE)
#plot.HCPC(cah1,choice="tree")

#cah2 <- HCPC(afc,nb.clust = 4,graph = FALSE,cluster.CA = "columns")
#plot.HCPC(cah2,choice="tree")

## Dual CAH "A la Bertin"

library(pheatmap)
x<-chisq.test(mat)
res<-as.matrix(x$residuals)
exp<-as.matrix(x$expected)
res[exp<2]<-0
res[res>3]<-3
res[res< -3]<- -3

pheatmap(res, cutree_rows = 6,cutree_cols = 6)





