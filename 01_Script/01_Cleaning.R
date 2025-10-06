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
    cols = starts_with("Q6_"),
    names_to = "brand_code", #brand_code is QX_5 => "5"
    values_to = "awareness"  #Binary
  ) %>%
  filter(awareness == 1) %>%  # Keep only "Yes" responses
  mutate(
    brand = as.numeric(str_remove(brand_code, "Q6_")),  # Extract brand number
    type = "Assisted"
  ) %>%
  select(id, brand, type)
#That is for one but let's make a map function
transform_binary <- function(data,id, mc_binary ) {
  
}