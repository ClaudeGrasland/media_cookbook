### Extract texts
# Select texts illustrating macro-region mentions or links

library(quanteda)
library(stringr)
library(tidytext)
library(dplyr)

qd<-readRDS("quanteda/corpus_worldgeo_V3.RDS")
td<-tidy(qd)
def<-read.table("dict/worldgeo_def_V2.csv", header=T, sep=";")

td2<-td %>% filter(td$source %in% c("tr_TUR_yenisa", "tr_TUR_cumhur"),
                   str_detect(td$tags, "CO_ERA"))

td2$text

