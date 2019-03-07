# CCSSO Chronic Absenteeism Presentation
# Evan Kramer
# 3/7/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
library(rgdal)
setwd("N:/ORP_accountability/")

# Switches 
data = T
clean = F
analysis = F

# Data
if(data) {
  student = read_csv(str_c("data/", year(today()) - 1, "_chronic_absenteeism/student_chronic_absenteeism.csv", 
                  col_types = "dcdcdcccddddddddddddd"))
  school = read_csv(str_c("projects/", year(today()) - 1, "_report_card/school_chronic_absenteeism.csv"))
  district = read_csv(str_c("projects/", year(today()) - 1, "_report_card/district_chronic_absenteeism.csv"))
  state = read_csv(str_c("projects/", year(today()) - 1, "_report_card/state_chronic_absenteeism.csv"))
  scores = read_csv(str_c("data/", year(today()) - 1, "_final_accountability_files/", year(today()) - 1, "_school_accountability_file.csv"))
  raw = read_dta("instructional_days_student_file.dta") %>% 
    clean_names() %>% 
    transmute(instructional_program_num = as.numeric(instructional_program_num),
              district_no = as.numeric(district_no), school_no = as.numeric(school_no), 
              grade, student_key, first_name, middle_name, last_name, begin_date, end_date, 
              isp_days = as.numeric(isp_days), cnt_total = as.numeric(cnt_total)) 
} else {
  rm(data)
}

# Cleaning
if(clean) {
  
} else{
  rm(clean)
}

# Analysis
if(analysis) {
  # Geographic distribution of absenteeism rates
  tn_counties = readOGR(
    dsn = "C:/Users/CA19130/Downloads/TN_counties",
    layer = "TN_counties",
    stringsAsFactors = F
  )
  geo = readOGR(
    dsn = "C:/Users/CA19130/Downloads/EDGE_GEOCODE_PUBLICSCH_1617/EDGE_GEOCODE_PUBLICSCH_1617",
    layer = "EDGE_GEOCODE_PUBLICSCH_1617", 
    stringsAsFactors = F
  )
  tn_geo = geo[geo@data$STATE == "TN", ]
  
  ggplot(
    data = mutate(metrics, # CHANGE THIS SECTION
      system_name = case_when(
        system_name == "Franklin" ~ "Franklin SSD",
        system_name == "H Rock Bruceton" ~ "Hollow Rock - Bruceton",
        !is.na(system_name) ~ system_name
      )
    ) %>%
    left_join(
      readxl::read_excel("H:/EDEN Data/EDEN 18-19/LEA and School Master Files/2018-19 EDFacts School Master File_2019-01-29.xlsx",
                         sheet = 2) %>% 
        transmute(system = as.numeric(lea_id_state), school = as.numeric(sch_id_state), 
                  sch_id_nces = str_c(lea_id_nces, sch_id_nces)), by = c("system", "school")
    ) %>% 
    inner_join(fortify(tn_geo@data), by = c("sch_id_nces" = "NCESSCH")),
  aes(x = LON, y = LAT)) + 
    geom_point(aes(color = factor(focus)), alpha = 0.3) + #, aes(size = adm, color = "red")) + 
    geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                 aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    theme_void() +
    scale_fill_discrete(guide = FALSE) +
    scale_color_discrete(name = "ATSI School") +
    scale_size_continuous(name = "ADM") + 
    coord_map() + 
    ggtitle("ATSI Schools") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # ggsave(str_c("N:/ORP_accountability/projects/Evan/School Improvement/Map of Focus Schools and Size, ", yr - 1, "-", str_sub(yr, -2, -1), ".png"), 
  #        units = "in", width = 9.17, height = 4.95)
  
  # What percent of absences were excused vs. unexcused?
  # How many students were enrolled across multiple districts?
  group_by(absenteeism, student_key, grade) %>% 
    summarize(n_schools = n_distinct(system, school)) %>%
    group_by(grade, n_schools) %>% 
    summarize(n = n_distinct(student_key)) %>% 
    ungroup() %>% 
    ggplot(aes(x = grade, fill = as.factor(n_schools), y = n)) + 
    geom_bar(stat = "identity", position = "stack") + 
    theme_bw()
    
  # Distribution of number of days enrolled
  group_by(absenteeism, isp_days) %>% 
    summarize()
} else {
  rm(analysis)
}

