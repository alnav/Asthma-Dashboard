### Creating Dataset for Alessio Project
library(lubridate)
library(tidyverse)
rm(list=ls())
set.seed(12345)
n<-10000

data<-as.data.frame(cbind(ppid=floor(runif(n,1,2500)))) %>%
  mutate(date = as.Date(floor(runif(n,0,365*8)),origin="2015-01-1")) %>%
  distinct()
n<-length(unique(data$ppid))

registration<-data %>%
  group_by(ppid) %>%
  filter(date==min(date)) %>%
  ungroup %>% 
  rename(start = date) %>%
  mutate(start_age = floor(runif(n,18,80)),
         sex = ifelse(runif(n,0,1)<=0.45,"Male","Female"),
         eosinophils = ifelse(runif(n,0,1)<0.3,rbeta(n,1.2,5),NA),
         fe_no = ifelse(runif(n,0,1)<0.2,(rbeta(n,0.85,5)*100),NA),
         attack_propensity = runif(n,0,1))
n<-nrow(data)

data_all<-data %>% 
  arrange(ppid,date) %>%
  left_join(registration) %>%
  mutate(gap = as.numeric(date-start),
         age = start_age+ floor(gap/365.25),
         BMI = (runif(n,0,1)>0.3)*rnorm(n,mean=25,sd=3),
         eosinophils = ifelse(gap==0,eosinophils,NA),
         fe_no = ifelse(gap==0,fe_no,NA),
         fev1 = rnorm(n,90,8),
         fvc = ifelse(sex=="Male",5,3.5)*rnorm(n,1,0.2),
         attack_prev_year = ifelse(runif(n,0,1)<0.2 | 
                                     (attack_propensity>=0.8 & runif(n,0,1)>0.5),
                                   1,0),
         smoker = ifelse(runif(n,0,1)<0.15,1,0)) %>%
  select(-start,-gap,-start_age,-attack_propensity)

data_long<-data_all %>%
  pivot_longer(cols = c(eosinophils,fe_no,BMI:smoker),
               names_to = "code_group",values_to = "value") %>%
  filter(!is.na(value))

write.csv(data_long,file="Alessio_data_10.10.24.csv")
