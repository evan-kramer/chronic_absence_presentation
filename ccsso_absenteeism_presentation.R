# CCSSO Chronic Absenteeism Presentation
# Evan Kramer
# 3/7/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/data/", 
            ifelse(between(month(today()), 1, 9), year(today()) - 1, year(today())),
            "_chronic_absenteeism"))

student = read_csv("student_chronic_absenteeism.csv", col_types = "dcdcdcccddddddddddddd")
school = read_csv("school_chronic_absenteeism.csv")
district = read_csv("district_chronic_absenteeism.csv")
state = read_csv("state_chronic_absenteeism.csv")

# What percent of absences were excused vs. unexcused?
