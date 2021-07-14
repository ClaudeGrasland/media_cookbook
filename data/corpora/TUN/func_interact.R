library(quanteda, quiet=T)
library(readr)
library(knitr)
library(ggplot2)
library(data.table)
library(lubridate, quiet=T)
library(tidytext, quiet=T)
library(stringr)
library(lubridate, quiet=T)
library(dplyr)


hypercube <-function(don = don,
                     id = "id",
                     order = "order",
                     when = "date",
                     when_cut = "year",
                     who = "source",
                     where1 = "tags",
                     where2 = "tags")
  
{   
  
  # create data.table accroding to parameter chosen
  df<-data.table(id = don[,id],
                 order = don[,order],
                 who = don[,who],
                 when = as.character(cut(don[,when],breaks=when_cut)),
                 where1 = don[,where1],
                 where2 = don[,where2])
  
  
  
  # add code _no_ for empty fields
  df$where1[df$where1==""]<-"_no_"
  df$where2[df$where2==""]<-"_no_"
  
  
  # unnest where1
  df<-unnest_tokens(df,where1,where1,to_lower=F)
  
  # unnest where2
  df<-unnest_tokens(df,where2,where2,to_lower=F)  
  
  # define number of occurence by id
  nb<-df[,.N,list(id,order)] %>%  mutate(wgt = 1/N) %>% select(-N)
  df<-df %>% left_join(nb) 
  
  rm(nb)
  
  # Aggregate
  hc<- df[,.( tags = .N, news=sum(wgt)) ,.(order,who, when,where1,where2)]
  
  # Convert date to time
  hc$when<-as.Date(hc$when)
  
  # return hypercube
  return(hc)
  
}



hc_filter <- function(don = hc,
                      who = "who",
                      when = "when",
                      where1 = "where1",
                      where2 = "where2",
                      wgt = "tags",
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
  if (is.na(when_start)==FALSE) { 
    df <- df[when >= as.Date(when_start), ]}
  if (is.na(when_end)==FALSE) { 
    df <- df[when <= as.Date(when_end), ]}
  # Select who
  if (is.na(who_exc)==FALSE) { 
    df <- df[!(who %in% who_exc), ]}
  if (is.na(who_inc)==FALSE) { 
    df <- df[(who %in% who_inc), ]}
  # Select where1
  if (is.na(where1_exc)==FALSE) { 
    df <- df[!(where1 %in% where1_exc), ]}
  if (is.na(where1_inc)==FALSE) { 
    df <- df[(where1 %in% where1_inc), ]}
  # Select where2
  if (is.na(where2_exc)==FALSE) { 
    df <- df[!(where2 %in% where2_exc), ]}
  if (is.na(where2_inc)==FALSE) { 
    df <- df[(where2 %in% where2_inc), ]}
  
  return(df)
  
}


######

build_int <- function(don = don,       # a dataframe with columns i, j , Fij
                      i = "where1",
                      j = "where2",
                      Fij = "wgt",
                      s1 = 5,
                      s2 = 5,
                      n1 = 3,
                      n2 = 3,
                      k = 0)
  
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
  int<-melt(mat)
  names(int) <-c("i","j","Fij")
  return(int)
}

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
  return(int)
  
}

