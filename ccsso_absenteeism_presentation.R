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
analysis = F

# Data
if(data) {
  student = read_csv(str_c("data/", year(today()) - 1, "_chronic_absenteeism/student_chronic_absenteeism.csv"), 
                  col_types = "dcdcdcccddddddddddddd")
  school = read_csv(str_c("projects/", year(today()) - 1, "_report_card/Real Data/school_chronic_absenteeism.csv"))
  district = read_csv(str_c("projects/", year(today()) - 1, "_report_card/Real Data/district_chronic_absenteeism.csv"))
  district2 = read_csv(str_c("data/", year(today()) - 1, "_chronic_absenteeism/district_chronic_absenteeism_primary_enrollment_only.csv"))
  state = read_csv(str_c("projects/", year(today()) - 1, "_report_card/Real Data/state_chronic_absenteeism.csv"))
  scores = read_csv(str_c("data/", year(today()) - 1, "_final_accountability_files/", year(today()) - 1, "_school_accountability_file.csv"))
  attendance = read_dta(str_c("data/", year(today()) - 1, "_chronic_absenteeism/instructional_days_student_file.dta")) %>% 
    janitor::clean_names() %>% 
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") & 
             type_of_service == "P") %>%
    transmute(instructional_program_num = as.numeric(instructional_program_num),
              district_no = as.numeric(district_no), school_no = as.numeric(school_no), 
              grade, student_key, first_name, middle_name, last_name, begin_date, end_date, 
              isp_days = as.numeric(isp_days), cnt_total = as.numeric(cnt_total),
              cnt_excused = as.numeric(cnt_excused), cnt_excused_trans = as.numeric(cnt_excused_trans), 
              cnt_unexcused = as.numeric(cnt_unexcused), cnt_unexcused_trans = as.numeric(cnt_unexcused_trans)) %>% 
    mutate_at(vars(starts_with("cnt_")), funs(ifelse(is.na(.), 0, .))) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade,
              student_key = as.integer(student_key), first_name, middle_name, last_name,
              begin_date, end_date, isp_days,
              count_total = cnt_total,
              count_excused = cnt_excused + cnt_excused_trans,
              count_unexcused = cnt_unexcused + cnt_unexcused_trans) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    # (Drops 0 records)
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences (Drops 9 records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    # take maximum instructional program number (Doesn't drop any records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
    # Collapse multiple enrollments at the same school
    group_by(system, school, grade, student_key) %>%
    summarize(first_name = first(first_name), middle_name = first(middle_name), last_name = first(last_name),
              days_excused = sum(count_excused, na.rm = T),
              days_unexcused = sum(count_unexcused, na.rm = T),
              n_absences_total = sum(count_total, na.rm = T),
              isp_days = sum(isp_days, na.rm = T)) %>%
    ungroup() %>%
    # Merge on instructional calendar file
    inner_join(
      readxl::read_excel(str_c("data/", year(today()) - 1, "_chronic_absenteeism/Instructional_Days_SchoolFile.xls")) %>%
                 transmute(year = 2018, system_name = DISTRICT_NAME, system = DISTRICT_NO,
                           school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS), 
      by = c("system", "school")
    ) %>%
    # Create counts
    mutate(n_students = 1,
           All = 1L) %>%
    # Merge on demographics
    left_join(
      read_delim("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/Demographics_SY2017_18.txt", delim = "\t") %>%
        janitor::clean_names() %>%
        mutate(
          Hispanic = race == 4,
          Black = race == 3,
          Native = race == 1,
          HPI = race == 5,
          Asian = race == 2,
          White = race == 6
        ) %>%
        group_by(student_key) %>%
        summarise_at(c("ed", "swd", "ell", "t1t4", "Hispanic", "Black", "Native", "HPI", "Asian", "White"), max, na.rm = TRUE) %>%
        transmute(student_key, BHN = pmax(Black, Hispanic, Native), ED = ed, SWD = swd, EL = pmax(ell, t1t4),
                  Hispanic, Black, Native, HPI, Asian, White), by = "student_key"
    )
  survey = readxl::read_excel("C:/Users/CA19130/Downloads/Health Services Survey_Totals.xlsx", n_max = 145) %>% 
    select(district_number, district_name = FirstName, starts_with("n_")) %>% 
    mutate(return_to_class_rate = round(100 * n_return_to_class / n_total_clinic_visits, 1))
} else {
  rm(data)
}

# Analysis
if(analysis) {
  # Absenteeism rainbow graphs
  readxl::read_excel(str_c("C:/Users/CA19130/Downloads/school_profile_", year(today()) - 2, "-", str_sub(year(today()) - 1, -2, -1), ".xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(system = district_id, school = school_id, pct_ed = economically_disadvantaged_pct) %>% 
    inner_join(filter(scores, subgroup == "All Students" & indicator == "Chronic Absenteeism" & n_count >= 30) %>% 
                 select(system, school, starts_with("metric"), starts_with("score")), 
               by = c("system", "school")) %>% 
    # arrange(desc(metric)) %>%
    arrange(desc(score)) %>%
    mutate(
      x = row_number(),
      # y = metric,
      y = score,
      ed_bin = case_when(
        pct_ed < 15 ~ "Below 15%",
        between(pct_ed, 15, 29.99) ~ "15%-29.9%",
        between(pct_ed, 30, 44.99) ~ "30%-44.9%",
        between(pct_ed, 45, 59.99) ~ "45%-59.9%",
        pct_ed >= 60 ~ "60% and higher"
        ), 
      ed_bin = factor(ed_bin, levels = c("Below 15%", "15%-29.9%", "30%-44.9%", "45%-59.9%", "60% and higher"))
    ) %>%
    filter(!is.na(ed_bin)) %>%
    ggplot(aes(x = x, y = y, fill = ed_bin)) +
    geom_bar(stat = "identity", width = 1) + 
    scale_x_continuous(breaks = NULL, name = "Each bar represents a school.") + 
    scale_y_continuous(name = str_c(year(today()) - 1, " Chronic Absenteeism Rate")) +
    scale_fill_manual(name = "Percent of Students in Poverty",
                      values = c("Below 15%" = rgb(94/255, 129/255, 181/255),
                                 "15%-29.9%" = rgb(105/255, 177/255, 159/255),
                                 "30%-44.9%" = rgb(236/255, 220/255, 111/255),
                                 "45%-59.9%" = rgb(230/255, 154/255, 73/255),
                                 "60% and higher" = rgb(212/255, 96/255, 87/255))) +
    ggtitle("Absenteeism Rates as a Function of Poverty") +
    theme_bw()
    
  # What percent of absences were excused vs. unexcused?
  group_by(attendance, grade) %>% 
    summarize(Excused = sum(days_excused, na.rm = T),
              Unexcused = sum(days_unexcused, na.rm = T)) %>% 
    mutate(grade = factor(grade, levels = c("K", str_c("0", 1:9), as.character(10:12)))) %>% 
    gather(key = "absence_type", value = "percent", 2:3) %>% 
    ggplot(aes(x = grade, y = percent, fill = absence_type)) +
    geom_bar(stat = "identity", position = "stack") + 
    theme_bw() + 
    scale_x_discrete(name = "Grade") + 
    scale_y_continuous(name = "Number of Total Absences") +
    scale_fill_discrete(name = "Absence Type") + 
    ggtitle(str_c("Number of Absences by Grade and Type, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absences_by_grade_and_type.png",
         units = "in", width = 9.17, height = 4.95)
  
  # Separate cut scores by grade
  ggplot(filter(scores, indicator == "Chronic Absenteeism" & subgroup == "All Students" & designation_ineligible == 0) %>% 
           arrange(metric) %>%
           mutate(rank = row_number()),
         aes(x = rank, y = metric, fill = pool)) + 
    geom_bar(stat = "identity") + 
    geom_hline(yintercept = state$pct_10_pct_or_more[state$subgroup == "All Students" & state$grade == "All Grades"],
               col = "red") + 
    scale_x_continuous(name = "Each bar represents a school.", labels = NULL) +
    scale_y_continuous(name = "Percent Chronically Absent") +
    scale_fill_discrete(name = "Pool") + 
    theme_bw() + 
    ggtitle(str_c("Absenteeism Rates by School, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/state_absentee_rates.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Do excused/unexcused rates vary by demographics?
  temp = tibble()
  for (s in c("All", "BHN", "ED", "SWD", "EL", "Black", "Hispanic", "Native", "HPI", "Asian", "White")) {
    temp = filter_(attendance, paste(s, " == 1L")) %>% 
      summarize(subgroup = s, 
                Excused = round(100 * sum(days_excused, na.rm = T) / sum(n_absences_total, na.rm = T), 1),
                Unexcused = round(100 * sum(days_unexcused, na.rm = T) / sum(n_absences_total, na.rm = T), 1)) %>% 
      bind_rows(temp, .) 
  }
  mutate(temp, 
         subgroup = case_when(
           subgroup == "All" ~ "All Students",
           subgroup == "BHN" ~ "Black/Hispanic/Native American",
           subgroup == "ED" ~ "Economically Disadvantaged",
           subgroup == "SWD" ~ "Students with Disabilities",
           subgroup == "EL" ~ "English Learners",
           subgroup == "Black" ~ "Black or African American",
           subgroup == "Hispanic" ~ "Hispanic or Latino",
           subgroup == "Native" ~ "American Indian or Alaska Native",
           subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
           subgroup %in% c("Asian", "White") ~ subgroup
           ), 
         subgroup = factor(subgroup, levels = c("White", "Students with Disabilities", "Native Hawaiian or Other Pacific Islander",
                                                "Hispanic or Latino", "English Learners", "Economically Disadvantaged",
                                                "Black/Hispanic/Native American", "Black or African American", "Asian", 
                                                "American Indian or Alaska Native", "All Students"))
  ) %>%
  gather(key = "absence_type", value = "n", 2:3) %>%
  ggplot(aes(x = subgroup, y = n, fill = absence_type, alpha = subgroup %in% c("Black or African American", "White"))) +
    geom_bar(stat = "identity", position = "stack") + 
    theme_bw() + 
    scale_x_discrete(name = "") + 
    scale_y_continuous(name = "Proportion of Total Absences") +
    scale_fill_discrete(name = "Absence Type") + 
    scale_alpha_discrete(NULL, guide = F) + 
    coord_flip() +
    ggtitle(str_c("Percentage of Absences by Subgroup and Type, ", year(today()) - 1))
  # ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absences_by_subgroup_and_type.png",
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absences_by_subgroup_and_type_highlighted.png",
         units = "in", width = 9.17, height = 4.95)
  
  # Minimum enrollment thresholds
  temp = group_by(student, student_id, system, school) %>% 
    summarize(n_absences = sum(n_absences, na.rm = T), 
              isp_days = sum(isp_days, na.rm = T),
              instructional_days = max(instructional_calendar_days, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(absentee_rate = round(100 * n_absences / isp_days, 1),
           pct_enrolled = round(100 * isp_days / instructional_days, 1))
  
  mutate(temp, enrolled_bin = case_when(
    between(pct_enrolled, 0, 9.9) ~ "0%-9.9%",
    between(pct_enrolled, 10, 19.9) ~ "10%-19.9%",
    between(pct_enrolled, 20, 29.9) ~ "20%-29.9%",
    between(pct_enrolled, 30, 39.9) ~ "30%-39.9%",
    between(pct_enrolled, 40, 49.9) ~ "40%-49.9%",
    between(pct_enrolled, 50, 59.9) ~ "50%-59.9%",
    between(pct_enrolled, 60, 69.9) ~ "60%-69.9%",
    between(pct_enrolled, 70, 79.9) ~ "70%-79.9%",
    between(pct_enrolled, 80, 89.9) ~ "80%-89.9%",
    between(pct_enrolled, 90, 100) ~ "90% and above"
  )) %>% 
  group_by(enrolled_bin) %>% 
    summarize(pct_chronically_absent = round(100 * sum(absentee_rate >= 10, na.rm = T) / sum(!is.na(absentee_rate)), 1)) %>% 
    ungroup() %>%
    filter(!is.na(enrolled_bin)) %>%
  ggplot(aes(x = factor(enrolled_bin, levels = c("90% and above", "80%-89.9%", "70%-79.9%", "60%-69.9%", "50%-59.9%",
                                                 "40%-49.9%", "30%-39.9%", "20%-29.9%", "10%-19.9%", "0%-9.9%")),
             y = pct_chronically_absent)) + 
    geom_bar(stat = "identity", position = "stack") + 
    scale_x_discrete(name = "Proportion of Year Enrolled") + 
    scale_y_continuous(name = "Percent of Students Chronically Absent") + 
    coord_flip() + 
    ggtitle(str_c("Absenteeism Rates by Enrollment Length, ", year(today()) - 1)) + 
    theme_bw() 
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rates_by_enrollment_length.png",
         units = "in", width = 9.17, height = 4.95)    
  
  # Students enrolled in multiple districts
  full_join(
    group_by(attendance, student_key) %>% 
      summarize(n_distinct_schools = n_distinct(system, school)) %>% 
      ungroup(),
    group_by(filter(attendance, isp_days / instructional_days >= .5), student_key) %>% 
      summarize(n_distinct_schools_50_pct = n_distinct(system, school)) %>% 
      ungroup(),
    by = "student_key"
  ) %>% 
    group_by(n_distinct_schools, n_distinct_schools_50_pct) %>% 
    summarize(n = n_distinct(student_key)) %>% 
    ungroup() %>% 
    mutate(n_distinct_schools_50_pct = ifelse(is.na(n_distinct_schools_50_pct), 0, n_distinct_schools_50_pct)) %>% 
    mutate(pct = round(100 * n / sum(n, na.rm = T), 1)) 
    # group_by(n_distinct_schools) %>% 
    # mutate(pct = round(100 * n / sum(n, na.rm = T), 1)) %>% 
    # ungroup() %>% 
    # filter(n_distinct_schools_50_pct < 2) %>% 
    # ggplot(aes(x = factor(n_distinct_schools), y = pct, fill = factor(n_distinct_schools_50_pct))) + 
    # geom_bar(stat = "identity", position = "stack") + 
    # geom_text(aes(group = factor(n_distinct_schools_50_pct), label = n)) +
    # coord_flip() +
    # theme_bw()
  
  # School geographic distribution of absenteeism rates
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
  geo_dist = readOGR(
    dsn = "C:/Users/CA19130/Downloads/EDGE_GEOCODE_PUBLICLEA_1617/EDGE_GEOCODE_PUBLICLEA_1617",
    layer = "EDGE_GEOCODE_PUBLICLEA_1617",
    stringsAsFactors = F
  )
  tn_geo = geo[geo@data$STATE == "TN", ]
  ggplot(
    # data = filter(school, subgroup == "All Students", grade == "All Grades") %>%
    data = filter(school, subgroup == "All Students", grade == "All Grades" & pct_10_pct_or_more <= 50) %>%
      transmute(system, school, pct_chronically_absent = pct_10_pct_or_more) %>% 
      left_join(
        readxl::read_excel("H:/EDEN Data/EDEN 18-19/LEA and School Master Files/2018-19 EDFacts School Master File_2019-01-29.xlsx",
                           sheet = 2) %>% 
          transmute(system = as.numeric(lea_id_state), school = as.numeric(sch_id_state), 
                    sch_id_nces = str_c(lea_id_nces, sch_id_nces)), by = c("system", "school")
      ) %>% 
      inner_join(fortify(tn_geo@data), by = c("sch_id_nces" = "NCESSCH")),
    aes(x = LON, y = LAT)
  ) + 
    geom_point(aes(color = pct_chronically_absent), alpha = 1) + #, aes(size = adm, color = "red")) + 
    geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                 aes(x = long, y = lat, group = group), fill = NA, color = "black") +
    theme_void() +
    scale_fill_discrete(guide = FALSE) +
    scale_color_gradient(name = "Chronic Absence Rate", low = "#7fbf7b", high = "#af8dc3") +
    coord_map() + 
    ggtitle(str_c("Chronic Absenteeism Rates, ", unique(school$year))) + 
    theme(plot.title = element_text(hjust = 0.5))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absenteeism_geography_school.png",
         units = "in", width = 9.17, height = 4.95)
  ggplot(
    data = left_join(
      filter(district2, subgroup == "All Students" & grade_band == "All Grades") %>% 
        select(system, starts_with("n_")) %>% 
        left_join(
          read_csv("C:/Users/CA19130/Documents/Data/Crosswalks/system system_name county crosswalk.csv") %>% 
            transmute(system, county = str_replace(county, " County", "")),
          by = "system"
        ) %>% 
        group_by(county) %>% 
        summarize_at(vars(starts_with("n_")), funs(sum(., na.rm = T))) %>% 
        ungroup() %>% 
        transmute(county = str_to_lower(county),
                  pct_chronically_absent = round(100 * n_chronically_absent / n_students, 1)),
      filter(map_data("county"), region == "tennessee"),
      by = c("county" = "subregion")
    ),
    aes(x = long, y = lat, group = group)
  ) + 
    geom_polygon(aes(fill = pct_chronically_absent), color = "black") + 
    theme_void() + 
    coord_map() + 
    scale_fill_gradient(name = "Chronic Absence Rate", low = "#7fbf7b", high = "#af8dc3") +
    ggtitle(str_c("Chronic Absenteeism Rates, ", unique(district2$year))) + 
    theme(plot.title = element_text(hjust = 0.5))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absenteeism_geography_district.png",
         units = "in", width = 9.17, height = 4.95)
  
  # Absenteeism vs. poverty
  ggplot(filter(district2, grade_band == "All Grades" & subgroup == "All Students") %>% 
           left_join(group_by(student, system, ED) %>% 
                       summarize(n = n_distinct(student_id)) %>% 
                       ungroup() %>% 
                       group_by(system) %>%
                       mutate(pct_ed = round(100 * n / sum(n, na.rm = T), 1)) %>% 
                       filter(ED == 1) %>% 
                       ungroup(), by = "system"),
         aes(x = pct_ed, y = pct_chronically_absent, size = n)) + 
    geom_point(alpha = 0.3) + 
    # geom_smooth(method = "lm", se = F) +
    theme_bw() + 
    scale_x_continuous(name = "Percent of Students in Poverty") + 
    scale_y_continuous(name = "Percent Chronically Absent") +
    scale_size_continuous(name = "Number of Students") + 
    ggtitle(str_c("District Chronic Absenteeism Rates as a Function of Poverty, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_poverty.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Absenteeism vs. proficiency
  ggplot(
    inner_join(
      filter(scores, subgroup == "All Students" & designation_ineligible == 0 & indicator == "Chronic Absenteeism") %>% 
        transmute(system, school, pool, n_count_abs = n_count, metric_abs = metric),
      filter(scores, subgroup == "All Students" & designation_ineligible == 0 & indicator == "Achievement") %>% 
        transmute(system, school, pool, n_count_ach = n_count, metric_ach = metric),
      by = c("system", "school", "pool")
    ),
    aes(x = metric_abs, y = metric_ach)
  ) +
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Proficient") + 
    scale_x_continuous(name = "Percent Chronically Absent") +
    ggtitle(str_c("School-Level Proficiency as a Function of Chronic Absenteeism Rates, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_proficiency.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Asthma
  ggplot(
    inner_join(
      transmute(survey, system = district_number, n_students_with_asthma),
      filter(district2, subgroup == "All Students" & grade_band == "All Grades"),
      by = "system"
    ),
    aes(
      x = round(100 * n_students_with_asthma / n_students, 1), 
      y = pct_chronically_absent
    )
  ) + 
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Chronically Absent") + 
    scale_x_continuous(name = "Percent of Students with Asthma", limits = c(0, 100)) +
    ggtitle(str_c("Absenteeism Rates as a Function of Students with Asthma, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_asthma.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Return to class rates
  ggplot(
    inner_join(
      transmute(survey, system = district_number, return_to_class_rate),
      filter(district2, subgroup == "All Students" & grade_band == "All Grades"),
      by = "system"
    ),
    aes(
      x = return_to_class_rate,
      y = pct_chronically_absent
    )
  ) + 
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Chronically Absent") + 
    scale_x_continuous(name = "Return to Class Rate", limits = c(0, 100)) +
    ggtitle(str_c("Absenteeism Rates as a Function of Return-to-Class Rates, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_return_to_class.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Percent of schools with full-time nurse
  ggplot(
    group_by(scores, system) %>% 
      summarize(n_schools = n_distinct(school)) %>% 
      ungroup() %>% 
      inner_join(
        transmute(survey, system = district_number, n_schools_with_ft_nurse),
        by = "system") %>% 
      inner_join(
        filter(district2, subgroup == "All Students" & grade_band == "All Grades") %>% 
          transmute(system, n_students, pct_chronically_absent),
        by = "system"
      ),
    aes(
      x = pmin(100, round(100 * n_schools_with_ft_nurse / n_schools, 1)),
      y = pct_chronically_absent
    )
  ) + 
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Chronically Absent") + 
    scale_x_continuous(name = "Percent of Schools with Full-Time Nurses", limits = c(0, 100)) +
    ggtitle(str_c("Absenteeism Rates as a Function of Nurse Availability, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_nurse_prevalence.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Severe allergies
  ggplot(
    inner_join(
      transmute(survey, system = district_number, n_students_with_severe_allergies),
    filter(district2, subgroup == "All Students" & grade_band == "All Grades") %>% 
      transmute(system, n_students, pct_chronically_absent),
    by = "system"
  ),
    aes(
      x = pmin(100, round(100 * n_students_with_severe_allergies / n_students, 1)),
      y = pct_chronically_absent
    )
  ) + 
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Chronically Absent") + 
    scale_x_continuous(name = "Percent of Students with Severe Allergies", limits = c(0, 100)) +
    ggtitle(str_c("Absenteeism Rates as a Function of Severe Allergy Incidence, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_severe_allergies.png", 
         width = 9.17, height = 4.95, units = "in")
  
  # Total chronic health conditions
  ggplot(
    inner_join(
      transmute(survey, system = district_number, n_chronic_health_condition),
      filter(district2, subgroup == "All Students" & grade_band == "All Grades") %>% 
        transmute(system, n_students, pct_chronically_absent),
      by = "system"
    ),
    aes(
      x = pmin(100, round(100 * n_chronic_health_condition / n_students, 1)),
      y = pct_chronically_absent
    )
  ) + 
    geom_point(alpha = 0.2) + 
    theme_bw() + 
    scale_y_continuous(name = "Percent Chronically Absent") + 
    scale_x_continuous(name = "Percent of Students with Chronic Health Conditions", limits = c(0, 100)) +
    ggtitle(str_c("Absenteeism Rates as a Function of Chronic Health Conditions, ", year(today()) - 1))
  ggsave("projects/Evan/Projects/20190327 Chronic Absenteeism Presentation/Visualizations/absentee_rate_chronic_conditions.png", 
         width = 9.17, height = 4.95, units = "in")
} else {
  rm(analysis)
}
