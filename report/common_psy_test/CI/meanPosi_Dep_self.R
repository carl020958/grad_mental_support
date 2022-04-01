setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('rcicr', 'tidyverse')

# code to generate posi_self & dep_self (preprocessing)

#posi
posi_ori <- readxl::read_excel("posiCI_data_210102.xlsx", sheet = "all")

posi_ori <- posi_ori %>% transmute(Name, sex, ID, response, age, student_ID = participant)

posi <- posi_ori

num_female <- as.numeric(posi %>% filter(sex == "female") %>% select(Name) %>% unique() %>% count())
num_male <- as.numeric(posi %>% filter(sex == "male") %>% select(Name) %>% unique() %>% count())

# #dep
# dep_ori <- readxl::read_excel("depCI_data.xlsx", sheet = "all")
# 
# dep_ori <- dep_ori %>% transmute(email = Name, sex, ID, response, age, student_ID = participant)
# 
# dep <- dep_ori
# 
# num_female_dep <- as.numeric(dep %>% filter(sex == "female") %>% select(email) %>% unique() %>% count())
# num_male_dep <- as.numeric(dep %>% filter(sex == "male") %>% select(email) %>% unique() %>% count())

# self-CI
load(file = "graduate.Rdata")

#age 없는 경우 넣어야 함(어떤 자극 세트를 보고 했는지에 따라)
# par_data_processed <- par_data_processed %>% mutate(age = 25), 

#studnet_id를 Rdata만들 때 넣지 못했다면 넣어야 함
par_data_processed <- par_data_processed %>% mutate(student_ID = NA)

done_list <- 
  tibble(CI_path = list.files(path = 'posi_self',
                              full.names = T)) %>% 
  mutate(CI_path = as.character(CI_path)) %>%
  mutate(index = str_extract(pattern = "(?<=posi_self\\_).*(?=_autoscaled)", string =  CI_path)) %>% 
  mutate(index = stringi::stri_trans_nfc(index)) %>% 
  pull(index)

par_data_processed <- par_data_processed %>% 
  filter(!Name %in% done_list)

par_data_processed %>% 
  select(Name) %>% 
  unique()


par_info <- par_data_processed$Name %>% unique()

# generate posi_self & dep_self
for(i in 1:length(par_info)){
  
  #posi_self
  par_self_CI <- par_data_processed %>% 
    filter(Name == par_info[i]) %>% 
    mutate(response = replace_na(response, "None"),
           response = ifelse(response == "f", 1,
                             ifelse(response == "j", -1, "None")),
           response = as.numeric(response)) %>% 
    filter(response != "None") %>% 
    # select(email, sex, ID, response, age, student_ID)
    transmute(Name, sex, ID, response, age, student_ID)
  
  par_sex <- par_self_CI$sex %>% unique()
  
  sex = ifelse(par_sex == 1, "male", "female")
  
  filter_sex = sex
  
  par_self_CI <- par_self_CI %>% slice(rep(1:n(), each = ifelse(par_sex == 1, num_male, num_female)))
  
  posi <- posi %>% filter(sex == filter_sex) 
  
  posi <- rbind(posi, par_self_CI)
  
  # rdatafile = ifelse(sex == "male",
  #                    "20_m.Rdata",
  #                    "20_f.Rdata")
  
  par_rdatafile <- par_self_CI %>% select(age, sex) %>% unique() %>% 
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
  
  groupCIs <- list(
    posi = generateCI2IFC(posi$ID, posi$response, baseimage, rdatafile,
                          saveasjpeg=F, antiCI = F, scaling = 'matched')
  )
  
  names(groupCIs) <- paste0("posi_self_", par_self_CI$Name %>% unique())
  
  groupCIs <- autoscale(groupCIs, saveasjpegs = T, targetpath = 'posi_self')
  
  posi <- posi_ori

  # #dep_self
  # par_self_CI_2 <- par_data_processed %>% 
  #   filter(email == par_info[i]) %>% 
  #   mutate(response = replace_na(response, "None"),
  #          response = ifelse(response == "f", 1,
  #                            ifelse(response == "j", -1, "None")),
  #          response = as.numeric(response)) %>% 
  #   filter(response != "None") %>% 
  #   select(email, sex, ID, response, age, student_ID)
  # 
  # par_sex_2 <- par_self_CI_2$sex %>% unique()
  # 
  # sex_2 = ifelse(par_sex_2 == 1, "male", "female")
  # 
  # filter_sex_2 = sex_2
  # 
  # par_self_CI_2 <- par_self_CI_2 %>% slice(rep(1:n(), each = ifelse(par_sex == 1, num_male_dep, num_female_dep)))
  # 
  # dep <- dep %>% filter(sex_2 == filter_sex_2) 
  # 
  # dep <- rbind(dep, par_self_CI_2)
  # 
  # # rdatafile = ifelse(sex == "male",
  # #                    "20_m.Rdata",
  # #                    "20_f.Rdata")
  # 
  # par_rdatafile_2 <- par_self_CI_2 %>% select(age, sex) %>% unique() %>% 
  #   mutate(rdatafile = case_when(
  #     (age >= 20 & age < 30 & sex == 1 ~ "20_m.Rdata"),
  #     (age >= 20 & age < 30 & sex == 2 ~ "20_f.Rdata"),
  #     (age >= 30 & age < 40 & sex == 1 ~ "30_m.Rdata"),
  #     (age >= 30 & age < 40 & sex == 2 ~ "30_f.Rdata"),
  #     (age >= 40 & age < 50 & sex == 1 ~ "40_m.Rdata"),
  #     (age >= 40 & age < 50 & sex == 2 ~ "40_f.Rdata")
  #   )) %>% 
  #   select(rdatafile)
  # 
  # rdatafile_2 = par_rdatafile_2$rdatafile
  # 
  # baseimage_2 <- sex_2
  # 
  # groupCIs_dep <- list(
  #   dep = generateCI2IFC(dep$ID, dep$response, baseimage_2, rdatafile_2,
  #                        saveasjpeg=F, antiCI = F, scaling = 'matched')
  # )
  # 
  # names(groupCIs_dep) <- paste0("dep_self_", par_self_CI_2$Name %>% unique())
  # 
  # groupCIs_dep <- autoscale(groupCIs_dep, saveasjpegs = T)
  # 
  # dep <- dep_ori
  
}

