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
  transmute(id, brand = mc1, type = "mutliple1") #brand here will replace and type is for us to remember what are these values when we will bind_row


