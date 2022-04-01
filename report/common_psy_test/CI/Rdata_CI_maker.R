setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('rcicr', 'tidyverse')

load(file = "graduate.Rdata")

done_list <- 
  tibble(CI_path = list.files(path = 'cis',
                              full.names = T)) %>% 
  mutate(CI_path = as.character(CI_path)) %>%
  mutate(index = str_extract(pattern = "(?<=participant\\_).*(?=_autoscaled)", string =  CI_path)) %>% 
  mutate(index = stringi::stri_trans_nfc(index)) %>% 
  pull(index)

par_data_processed <- par_data_processed %>% 
  filter(!Name %in% done_list)

par_data_processed %>% 
  select(Name) %>% 
  unique()

list <- split(par_data_processed, f = par_data_processed$email)


#generate CI
for (i in 1:length(list)) {
  
  par <- data.frame(list[i])
  
  names(par) <- names(par_data_processed)
  
  par$response <- ifelse(par$response == "f",1,
                         ifelse(par$response == "j", -1, "None"))
  
  par$participant <- par$Name
  
  par <- par %>% 
    filter(response != "None")
  
  par$ID <- par$ID
  
  par$response <- as.numeric(par$response)
  
  sex = ifelse(unique(par$sex) %in% c("1","male"), 
               "male", "female")
  
  par_rdatafile <- par %>% select(age, sex) %>% unique() %>% 
    mutate(rdatafile = case_when(
                                  (age >= 20 & age < 30 & sex == 1 ~ "20_m.Rdata"),
                                  (age >= 20 & age < 30 & sex == 2 ~ "20_f.Rdata"),
                                  (age >= 30 & age < 40 & sex == 1 ~ "30_m.Rdata"),
                                  (age >= 30 & age < 40 & sex == 2 ~ "30_f.Rdata"),
                                  (age >= 40 & age < 50 & sex == 1 ~ "40_m.Rdata"),
                                  (age >= 40 & age < 50 & sex == 2 ~ "40_f.Rdata"),
                                  (age >= 50 & age < 60 & sex == 1 ~ "50_m.Rdata"),
                                  (age >= 50 & age < 60 & sex == 2 ~ "50_f.Rdata")
                                  )) %>% 
    select(rdatafile)
  
  rdatafile = par_rdatafile$rdatafile
    
  baseimage <- sex
  
  ci <- batchGenerateCI2IFC(
    par,
    by = 'participant',
    stimuli = 'ID',
    responses = 'response',
    baseimage = baseimage,
    rdata = rdatafile,
    antiCI = F,
    saveasjpeg = F)
  
  scaled_cis <- autoscale(ci, saveasjpegs = TRUE)
  
}
