library(data.table)
library(tidyverse)
rm(list=ls())

# 複数のファイルを一括で読み込む処理
x<-c("01","02","03")
files<-str_glue("./input/sample_22{x}.csv")

# readr::read_csv()
new_dataframe<-files %>% 
  map_dfr(read_csv)

# data.table::fread()
# 処理速度は速い。自由度高い
new_dataframe<-files %>% 
  map_dfr(fread)


N <- 15
dat <- tibble(tag1 = sample(LETTERS[1:3], N, replace = TRUE),
              tag2 = sample(letters[1:5], N, replace = TRUE),
              y = rnorm(N),
              x = runif(N))

dat_nest<-dat %>% 
  group_nest(tag1) 

dat_nest %>% 
  mutate(x_mean = map(data, ~mean(.$x)))

library(tidyverse)
library(lubridate)
data<-data.frame(sunspot.month)

x<-c("yes","yes","no","no")
y<-c(202111,202111,202112,202112)
z<-c(202011,202011,202012,202012)

sample_data<-data.frame(x,y,z) %>% 
  mutate(y=ym(y),
         z=ym(z),
         w = ifelse(x=="yes",y,z) )



#PRマッチング
# 解析用データ
# 対象者全員
propen_data<-fix %>% 
  mutate(use = dplyr::recode(use,
                             "はい" 　= 1,
                             "いいえ" = 0)) %>% 
  mutate(sex = recode(sex,
                      "男" =  1,
                      "女" =  2)) 

write_csv(propen_data,
          paste(getwd(),"propen_data.csv",sep = "/"),
          append = FALSE)


propensityScoreModel = glm(use ~ age + sex+day_sum_pre + burden_kenpo_sum_pre, 
                           family = binomial(link = "logit"), 
                           data   = propen_data)

propen_data$propensity_scores = propensityScoreModel$fitted.values

propen_data<-propen_data %>% 
  mutate(ipw = case_when(use == 1~ 1/propensity_scores,
                         use == 0 ~ 1/(1- propensity_scores)))

propen_data_cat<-propen_data %>% 
  mutate(use=ifelse(use==1,"利用あり","利用なし"))

#マッチング前の分布
ggplot(data=propen_data_cat)+
  geom_histogram(mapping =  aes(x=propensity_scores, fill=use),position = "identity", alpha = 0.5,binwidth=0.01)+
  theme_bw(base_family = "HiraKakuProN-W3")+
  scale_x_continuous(breaks = seq(0,1,0.1))

ggplot(data=propen_data_cat)+
  geom_histogram(mapping =  aes(x=ipw, fill=use),position = "identity", alpha = 0.5,binwidth=0.01)+
  theme_bw(base_family = "HiraKakuProN-W3")
# scale_x_continuous(breaks = seq(0,1,0))

# マッチング
result<-Match(Tr = propen_data$use, X=propen_data$propensity_scores, M=1,caliper = 0.2, replace = TRUE)

propendata_result<-rbind(propen_data[result$index.treated,],propen_data[result$index.control,])

data_fix_propen<-propendata_result %>% 
  mutate(use=ifelse(use==1,"利用あり","利用なし"))

# マッチング後の分布
ggplot(data=data_fix_propen)+
  geom_histogram(mapping =  aes(x=propensity_scores, fill=use),position = "identity", alpha = 0.5)+
  theme_bw(base_family = "HiraKakuProN-W3")

# アウトプット
table_propen<-data_fix_propen %>% 
  ungroup() %>% 
  dplyr::select(use,burden_kenpo_month_post,day_month_post) %>% 
  tbl_summary(by=use,
              statistic = list("burden_kenpo_month_post" ~ "{mean}({sd})",
                               "day_month_post" ~ "{mean}({sd})")) %>% 
  add_p()
table_propen

write_csv(propen_data_after,
          paste(getwd(),"propen_data_after_finishUsers.csv",sep = "/"))


propen_after_mean<-propen_data_after %>% 
  summarise(count = n(),
            day_mean_use             = mean(day_month_post_use, na.rm = TRUE),
            day_mean_nouse           = mean(day_month_post_nouse, na.rm = TRUE),
            burden_kenpo_mean_use    = mean(burden_kenpo_month_post_use, na.rm = TRUE),
            burden_kenpo_mean_nouse  = mean(burden_kenpo_month_post_nouse, na.rm = TRUE))

write_csv(propen_after_mean,
          paste(getwd(),"propen_data_mean_finishUsers.csv",sep = "/"),
          append = FALSE)

t.test(propen_data_after$day_month_post_use,propen_data_after$day_month_post_nouse, paired = FALSE)
t.test(propen_data_after$burden_kenpo_month_post_use,propen_data_after$burden_kenpo_month_post_nouse, paired = FALSE)


# 利用前に1回以上
propendata_preday1<-propen_data %>% 
  filter(day_sum_pre>=1) 

#マッチング
result_preday1<-Match(Tr = propendata_preday1$use, X=propendata_preday1$propensity_scores, M=1,caliper = 0.2, replace = TRUE)

propendata_result_preday1<-rbind(propendata_preday1[result_preday1$index.treated,],propendata_preday1[result_preday1$index.control,])

data_fix_propen_preday1<-propendata_result_preday1 %>% 
  mutate(use=ifelse(use==1,"利用あり","利用なし"))

# マッチング後の分布
ggplot(data=data_fix_propen_preday1)+
  geom_histogram(mapping =  aes(x=propensity_scores, fill=use),position = "identity", alpha = 0.5)+
  theme_bw(base_family = "HiraKakuProN-W3")

# アウトプット
table_propen_preday1<-data_fix_propen_preday1 %>% 
  ungroup() %>% 
  dplyr::select(use,burden_kenpo_month_post,day_month_post) %>% 
  tbl_summary(by=use,
              statistic = list("burden_kenpo_month_post" ~ "{mean}({sd})",
                               "day_month_post" ~ "{mean}({sd})")) %>% 
  as_flex_table()

print(table_propen_preday1,"pptx")

table_propen_preday1