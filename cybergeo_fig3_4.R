### Cybergeo tables and figures

library(quanteda, quiet=T)
library(readr)
library(knitr)
library(ggplot2)
library(data.table)
library(lubridate, quiet=T)
library(tidytext, quiet=T)
library(stringr)
library(visNetwork, quietly = T)
library(dplyr)
library(reshape2)
library(igraph)

#######  Preparation #############
hc_sta_reg<-readRDS("hypercube/hc_sta_reg.RDS") %>% 
  filter(where1 !="_no_", where2 != "_no_") %>% 
  filter(is.na(when)==F, when < as.Date("2021-01-01"))

# Load table of label and choose language

reg_def<-read.table("dict/worldgeo_def_V2.csv", sep=";",quote = '"', encoding = "UTF-8",header=T)
tab_def<-reg_def %>% filter(lang=="fr") %>% select(code,type,label)
tab_def<-tab_def[duplicated(tab_def$label)==F,]

# Adaft some long labels
tab_def$label[tab_def$code=="ST_CHN"]<-"Chine"
tab_def$label[tab_def$code=="ST_ARE"]<-"E.A.U."
tab_def$label[tab_def$code=="ST_COD"]<-"RD Congo"
tab_def$label[tab_def$code=="ST_NLD"]<-"Pays-Bas"



# Merge regional names
lab_reg<-tab_def %>% filter(!substr(code,1,3) %in% c("ST_","CA_")) %>% select(where2=code,label2=label)
hc_sta_reg<-left_join(hc_sta_reg,lab_reg)

# Merge state names
lab_reg<-tab_def %>% filter(substr(code,1,3) %in% c("ST_")) %>%
  mutate(where1=substr(code,4,6))%>%
  select(where1,label1=label)
hc_sta_reg<-left_join(hc_sta_reg,lab_reg)

# Correct bugs
hc_sta_reg$label2[hc_sta_reg$label2=="Machrek"]<-"Maghreb"
hc_sta_reg$label2[hc_sta_reg$label2=="Europe médiane"]<-"Europe centrale"

# Eliminate national links
hc_sta_reg<-hc_sta_reg[!(substr(who,4,6)== where1),]
hc_sta_reg<-hc_sta_reg[!(substr(who,4,6)== "NIR" & where1=="GBR"),]

#########   Select links  #########

# function
hc_filter <- function(don = hc,
                      who = "who",
                      when = "when",
                      where1 = "where1",
                      where2 = "where2",
                      wgt = "tags",
                      self = FALSE,
                      when_start = NA,
                      when_end = NA,
                      who_exc = NA,
                      who_inc = NA,
                      where1_exc = NA,
                      where1_inc = NA,
                      where2_exc = NA,
                      where2_inc = NA)

{                          
  
  df<-data.table(who = don[[who]],
                 when = don[[when]],
                 where1 = don[[where1]],
                 where2 = don[[where2]],
                 wgt = don[[wgt]])
  
  
  # Select time period
  if (is.na(when_start[1])==FALSE) { 
    df <- df[when >= as.Date(when_start), ]}
  if (is.na(when_end[1])==FALSE) { 
    df <- df[when <= as.Date(when_end), ]}
  # Select who
  if (is.na(who_exc[1])==FALSE) { 
    df <- df[!(who %in% who_exc), ]}
  if (is.na(who_inc[1])==FALSE) { 
    df <- df[(who %in% who_inc), ]}
  # Select where1
  if (is.na(where1_exc[1])==FALSE) { 
    df <- df[!(where1 %in% where1_exc), ]}
  if (is.na(where1_inc[1])==FALSE) { 
    df <- df[(where1 %in% where1_inc), ]}
  # Select where2
  if (is.na(where2_exc[1])==FALSE) { 
    df <- df[!(where2 %in% where2_exc), ]}
  if (is.na(where2_inc[1])==FALSE) { 
    df <- df[(where2 %in% where2_inc), ]}
  # eliminate internal links
  if (self==FALSE) { 
    df <- df[(where1 != where2), ]}
  return(df)
  
}

# Application
hc<-hc_filter(don = hc_sta_reg,
              where1 = "label1",
              where2 = "label2",
              #              who_inc = c("fr_FRA_figaro", "fr_FRA_lmonde"),
              #             who_inc = c("de_DEU_frankf", "de_DEU_suddeu"),
              #             who_inc = c("tr_TUR_cumhur", "tr_TUR_yenisa"),
              wgt = "tags",
              where2_exc = c("_no_"),
              self = FALSE
)

######## Create complete interaction matrix ########

build_int <- function(don = don,       # a dataframe with columns i, j , Fij
                      i = "where1",
                      j = "where2",
                      Fij = "wgt",
                      s1 = 5,
                      s2 = 5,
                      n1 = 3,
                      n2 = 3,
                      k = 0)
 
# Function 
{  
  df<-data.table(i=don[[i]],j=don[[j]],Fij=don[[Fij]])
  int <-df[,.(Fij=sum(Fij)),.(i,j)]
  int<-dcast(int,formula = i~j,fill = 0)
  mat<-as.matrix(int[,-1])
  row.names(mat)<-int$i
  mat<-mat[apply(mat,1,sum)>=s1,apply(mat,2,sum)>=s2 ]
  m0<-mat
  m0[m0<k]<-0
  m0[m0>=k]<-1
  mat<-mat[apply(m0,1,sum)>=n1,apply(m0,2,sum)>=n2 ]
  int<-reshape2::melt(mat)
  names(int) <-c("i","j","Fij")
  return(int)
}

int <- build_int(don = hc,
                 i="where1",
                 j="where2",
                 s1=3,
                 s2=3,
                 n1=1,
                 n2=1,
                 k=0)
head(int)

####### Estimate random model


# function

rand_int <- function(int = int, # A table with columns i, j Fij
                     maxsize = 100000,
                     diag    = FALSE,
                     resid   = FALSE) {
  # Eliminate diagonal ?
  if (diag==FALSE) { 
    int <- int[as.character(int$i) != as.character(int$j), ]}
  
  # Compute model if size not too large
  if (dim(int)[1] < maxsize) {
    # Proceed to poisson regression model
    mod <- glm( formula = Fij ~ i + j,family = "poisson", data = int)
    
    # Add residuals if requested
    if(resid == TRUE)   { 
      # Add estimates
      int$Eij <- mod$fitted.values
      
      # Add absolute residuals
      int$Rabs_ij <- int$Fij-int$Eij
      
      # Add relative residuals
      int$Rrel_ij <- int$Fij/int$Eij
      
      # Add chi-square residuals
      int$Rchi_ij <-  (int$Rabs_ij)**2 / int$Eij
      int$Rchi_ij[int$Rabs_ij<0]<- -int$Rchi_ij[int$Rabs_ij<0]
    }
    
  } else { paste ("Table > 100000 -  \n 
                     modify maxsize =  parameter \n
                     if you are sure that your computer can do it !")}
  # Export results
  int$i<-as.character(int$i)
  int$j<-as.character(int$j)
  return(int)
  
}


mod<-rand_int(int,
              resid = TRUE,
              diag = FALSE)

length(table(mod$i))


##### Visalize ###

# function
geo_network<- function(don = mod,
                       from = "i",
                       to = "j", 
                       size = "Fij",
                       minsize = 1,
                       maxsize = NA,
                       test = "Fij",
                       mintest = 1,
                       loops  = FALSE, 
                       title = "Network")
  
{
  # check
  #don<-mod[mod$i!="FRA",]
  #from<-"i"
  #to<-"j"
  #size<-"Fij"
  #test<-"Rchi_ij"
  #minsize<-5
  #maxsize<-NA
  #mintest<-3.84
  #title<-"Network"
  #loops<-FALSE
  
  
  int<-data.frame(i = as.character(don[,from]),
                  j = as.character(don[,to]),
                  size = don[,size],
                  test = don[,test]
  )
  if (is.na(minsize)==FALSE) {int =int[int$size >= minsize,]} 
  if (is.na(maxsize)==FALSE) {int =int[int$size <= maxsize,]} 
  if (is.na(mintest)==FALSE) {int =int[int$test >= mintest,]}
  
  #nodes<-data.frame(code = unique(c(int$i,int$j)))
  #nodes$code<-as.character(nodes$code)
  #nodes$id<-1:length(nodes$code)
  #nodes$label<-nodes$code
  
  nodesi<-data.frame(code = unique(c(int$i)))
  nodesi$code<-as.character(nodesi$code)
  nodesi$id<-1:length(nodesi$code)
  nodesi$label<-nodesi$code
  nodesi$color<-"blue"
  #nodesi$shape="square"
  #nodesi$group = "States"
  tot<-int %>% group_by(i) %>% summarize(size =sum (size)) %>% select (code=i,size)
  tot$code<-as.factor(tot$code)
  nodesi <- left_join(nodesi,tot) %>% mutate(value = sqrt(1+5 *size/max(size)))
  
  nodesj<-data.frame(code = unique(c(int$j)))
  nodesj$code<-as.character(nodesj$code)
  nodesj$id<-1:length(nodesj$code)+length(nodesi$code)
  nodesj$label<-nodesj$code
  nodesj$color<-"red"
  #nodesj$shape="circle"
  #nodesj$group="region"
  tot<-int %>% group_by(j) %>% summarize(size =sum (size)) %>% select (code=j,size)
  tot$code<-as.factor(tot$code)
  nodesj <- left_join(nodesj,tot) %>% mutate(value = sqrt(1+5 *size/max(size)))
  
  nodes<-rbind(nodesi,nodesj)
  
  
  # Adjust edge codes
  edges <- int %>% mutate(width = 2+5*size / max(size)) %>%
    left_join(nodes %>% select(i=code, from = id)) %>%  
    left_join(nodes %>% select(j=code, to = id ))
  edges$color<-"gray90"
  
  # compute nodesize
  #tot<-int %>% group_by(i) %>% summarize(size =sum (size)) %>% select (code=i,size)
  #tot$code<-as.factor(tot$code)
  #nodes <- left_join(nodes,tot) %>% mutate(value = sqrt(10 *size/max(size)))
  
  
  #sel_nodes <-nodes %>% filter(code %in% unique(c(sel_edges$i,sel_edges$j)))
  
  # eliminate loops
  
  
  if(loops == FALSE) {edges <- edges[edges$from < edges$to,]}
  
  net<- visNetwork(nodes, 
                   edges, 
                   main = title,
                   height = "700px", 
                   width = "100%")   %>%   
    visNodes(scaling =list(min =10, max=40, 
                           label=list(min=20,max=40, 
                                      maxVisible = 20)))%>%
    visOptions(highlightNearest = TRUE,
               #               selectedBy = "group", 
               #               manipulation = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    #      visLegend() %>%
    visIgraphLayout(layout ="layout.fruchterman.reingold",smooth = TRUE)
  
  net
  return(net)
} 

# raw network

netraw<- geo_network(mod,
            size = "Fij",
            minsize = 5,
            test = "Fij",
            mintest = 5)
netraw

# residual network


netres<-geo_network(mod,
            size = "Rabs_ij",
            minsize = 5,
            test = "Rchi_ij",
            mintest = 3.84)
netres
