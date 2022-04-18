library(data.table)
library(tidyverse)


data(trees)

# 複数のファイルを一括で読み込む処理
x<-c("01","02","03")
files<-str_glue("./input/master_22{x}.csv")
new_dataframe<-files %>% 
  map_dfr(fread)