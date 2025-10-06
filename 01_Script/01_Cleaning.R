#Input : Df from the result of the survey
#Output : Cleaned df ready for analysis
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
#Load the data
gross_data<- read_excel(here::here("02_Input", "Base_VF.xlsx")) #Change the name of the file

#Change 1 remove anything that does not have id
gross_data <- gross_data%>%
  filter(!is.na(id & id != ""))
#Change 2 changing some answer to NA
survey <- gross_data %>%         #Notice we rename the df because we want to be able to go back
  mutate(across(
    .cols = c(Q7, Q9, Q11, Q13), # Specify columns
    .fns = ~ replace(.x, .x == "some_answer", NA)                
  ))

#Change 3 Filter out some outlier (all sort of filter)
survey <- gross_data %>%
  filter(!is.na(V1) & V1!= "") %>%
  filter(amount > 300 | amount < 50)

#Change 4 Managing multiple choice question
mc_question <- gross_data %>%
  select(id, mc1, mc2, mc3, mc4) #Just isolating all the mc question
mc_long <- mc_question %>%
  filter(!is.na(mc1)) %>%         #Because separate_rows do not deal with N/A
  separate_rows(mc1, sep = "-") %>%
  filter(Q5B != "" & !is.na(Q5B)) %>% #N/A's are useless for this df
  transmute(id, brand = mc1, type = "mutliple1") %>% #brand here will replace and type is for us to remember what are these values when we will bind_row
  mutate( brad = as.numeric(brand)) #They were character because of separator
#That is for one but let's make a function map for multiple variable into a list
transform_mc_question <- function(data, id, mc_var, type_name) {
  data %>%
    select({{id}}, {{mc_var}}) %>%
    filter(!is.na({{mc_var}})) %>%
    separate_rows({{mc_var}}, sep = "-") %>%
    filter({{mc_var}} != "" & !is.na({{mc_var}})) %>%
    transmute(
      {{id}},
      brand = as.numeric({{mc_var}}),
      type = type_name
    )
}

# Corrected usage with map
mc_question_vars <- c(M1 = "M1", M2= "M2", M3= "M3", M4 ="M4") #M1 = "M1" is for list and variable name
type_names <- c("multiple1", "multiple2", "multiple3", "multiple4") #name of type

# Using map2 since we have two parallel vectors
mc_list <- map2(mc_question_vars, type_names, 
                ~ transform_mc_question(survey, id, !!.x, .y)) #replace id by actual variable id

#Change 5 Changing multiple binary question (suppose 0 = no, 1 = yes)
binary_to_long <- survey %>%
  select(id, starts_with("QX_")) %>% #change the pattern
  pivot_longer(
    cols = starts_with("QX_"),
    names_to = "brand_code", #brand_code is QX_5 => "5"
    values_to = "awareness"  #Binary
  ) %>%
  filter(awareness == 1) %>%  # Keep only "Yes" responses
  mutate(
    brand = as.numeric(str_remove(brand_code, "QX_"))) %>%  # Extract brand number
  select(id, brand, type)
#That is for one but let's make a map function
transform_binary <- function(data, id, mc_binary) {
  # Input validation
  binary_cols <- data %>% select(starts_with({{mc_binary}}))
  if(!all(unlist(binary_cols) %in% c(0, 1, NA))) {
    warning("Some binary columns contain values other than 0/1")
  }
  
  data %>%
    select({{id}}, starts_with({{mc_binary}})) %>%
    pivot_longer(
      cols = starts_with({{mc_binary}}),
      names_to = "answer",
      values_to = "yes_no",
      values_drop_na = TRUE  # Optional: exclude NA values
    ) %>%
    filter(yes_no == 1) %>%
    mutate(
      answer = as.numeric(str_remove(answer, fixed({{mc_binary}})))
    ) %>%
    select({{id}}, answer)
}
binary_vector <- c(B1 = "B1",B2= "B2",B3= "B3",B4 ="B4")
binary_list <- map(binary_vector, ~transform_binary(survey, id, .x))

#Change 7 Date & Time: I really don't know yet

#Change 8 Creating derived Variables
# Age groups, income brackets, composite scores
survey <- survey %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(0, 25, 35, 45, 55, 65, 100),
                    labels = c("0-25", "26-35", "36-45", "46-55", "56-65", "66+"), #cut uses by default interval = "left" => (0,25]
                    include.lowest = TRUE)
  )
 #If we need right interval
survey <- survey %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(0, 25, 35, 45, 55, 65, 100),
                    labels = c("0-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                    right = FALSE)  # [0,25) includes 0, excludes 25
  )
#Coversion of a group into another group

survey <- survey %>%
  mutate(
    age_group_large = case_when(
      age_group == "15-18" ~ "15-18",           # Keep unchanged
      age_group %in% c("19-25", "26-30") ~ "19-30",     # Combine these
      age_group %in% c("31-35", "36-40") ~ "31-40",     # Combine these  
      age_group %in% c("41-45", "46-50") ~ "41-50",     # Combine these
      age_group %in% c("51-55", "56-60", "61-65") ~ "51-65",  # Combine these
      age_group == "65+" ~ "65+",               # Keep unchanged
      TRUE ~ age_group  # Fallback for any unexpected values
    )
  )