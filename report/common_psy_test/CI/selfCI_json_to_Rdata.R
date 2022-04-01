### JSON->Rdata

pacman::p_load('rcicr', 'dplyr', 'jsonlite')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ----------
# read data
# ----------

data_name <- paste0("./raw/", list.files(path = './raw', pattern = "*.json"))

# 17 - f_30/ 18 - m_30/ 30 - f_20/ 31 - f_20

data_list <- list()
for (i in 1:length(data_name)){
  data_list[[i]] = data.frame()
}


#전체 데이터
for(j in 1:length(data_list)){
  
  data <- fromJSON(data_name[j])
  
  for (i in 1:length(data$replies$name)){
    
    data_list[[j]] <- rbind(data_list[[j]],
                            data.frame(data$replies$answers[i]) %>%
                              mutate(name = data$replies$name[i],
                                     email = data$replies$email[i],
                                     sex = data$replies$sex[i],
                                     age = data$replies$age[i],
                                     date = data$replies$date[i],
                                     phone = data$replies$phone[i],
                                     no = data$replies$no[i])
    )
  }
}

names(data_list) <- data_name

# --------------
# restrict date
# --------------

for (i in 1:length(data_list)){
  data_list[[names(data_list)[i]]] <- 
    data_list[[names(data_list)[i]]] %>% 
    mutate(date_lub = lubridate::ymd_hms(date)) %>% 
    filter(date_lub > '2021-07-01') %>% 
    select(-date_lub)  
}

#이메일 중복 여부 검사
.email_dup <- function(x){
  x %>% count(email) %>% filter(n != 300)
}
#동명이인 및 반복 시행 여부 검사
.same_name <- function(x){
  x %>% count(name) %>% filter(n != 300)
}

.same_ID <- function(x){
  x %>% count(no) %>% filter(n != 300)
}

data_list[[names(data_list)[1]]] %>% .email_dup
data_list[[names(data_list)[1]]] %>% .same_ID
data_list[[names(data_list)[1]]] %>% .same_name

data_list[[names(data_list)[2]]] %>% .email_dup
data_list[[names(data_list)[2]]] %>% .same_ID
data_list[[names(data_list)[2]]] %>% .same_name

data_list[[names(data_list)[3]]] %>% .email_dup
data_list[[names(data_list)[3]]] %>% .same_ID
data_list[[names(data_list)[3]]] %>% .same_name

data_list[[names(data_list)[4]]] %>% .email_dup
data_list[[names(data_list)[4]]] %>% .same_ID
data_list[[names(data_list)[4]]] %>% .same_name

data_list[[names(data_list)[5]]] %>% .email_dup
data_list[[names(data_list)[5]]] %>% .same_ID
data_list[[names(data_list)[5]]] %>% .same_name

data_list[[names(data_list)[6]]] %>% .email_dup
data_list[[names(data_list)[6]]] %>% .same_ID
data_list[[names(data_list)[6]]] %>% .same_name


#검사 2번 시행한 2번째 파일 확인
# data_list[[names(data_list)[1]]] %>%
#   filter(name %in% c('***', '***')) %>%
#   View()

# data_list[[names(data_list)[2]]] %>%
#   filter(name %in% c('***', '***')) %>%
#   View()

# data_list[[names(data_list)[3]]] %>%
#   filter(name %in% c('***')) %>%
#   View()

# data_list[[names(data_list)[4]]] %>%
#   filter(name %in% c('***', '***')) %>%
#   View()

# 김민경, 이주희, 윤지현, 김지현은 다른 사람
# data_list[[names(data_list)[5]]] %>%
#   filter(name %in% c('***', '***', '***', '***', '***', '***', '***', '***')) %>%
#   View()

# data_list[[names(data_list)[6]]] %>%
#   filter(name %in% c('***')) %>%
#   View()

# ------------------------
# remove duplicated date
# ------------------------

#검사 2번 시행한 2번째 파일 제거
data_list[[names(data_list)[1]]] <- 
  data_list[[names(data_list)[1]]] %>% 
  filter(!date %in% c('2021-07-05 18:15:42',
                      '2021-07-30 20:14:17'))

data_list[[names(data_list)[2]]] <- 
  data_list[[names(data_list)[2]]] %>% 
  filter(!date %in% c('2021-07-05 20:24:24', 
                      '2021-07-11 15:55:37'))

data_list[[names(data_list)[3]]] <- 
  data_list[[names(data_list)[3]]] %>% 
  filter(!date %in% c('2021-07-13 17:06:12'))

data_list[[names(data_list)[4]]] <- 
  data_list[[names(data_list)[4]]] %>% 
  filter(!date %in% c('2021-07-14 15:09:05', 
                      '2021-07-07 12:35:12'))

data_list[[names(data_list)[5]]] <- 
  data_list[[names(data_list)[5]]] %>% 
  filter(!date %in% c('2021-07-05 10:05:44',
                      '2021-07-05 18:36:42',
                      '2021-07-06 16:37:04',
                      '2021-07-10 12:56:22',
                      '2021-07-11 16:52:40' 
                      )) 

data_list[[names(data_list)[6]]] <-
  data_list[[names(data_list)[6]]] %>%
  filter(!date %in% c('2021-07-26 19:00:50')) #강동주


# ------------------------
# change name of columns 
# ------------------------

#이름 최종 정리
for(i in 1:length(data_list)){
  data_list[[i]] <- data_list[[i]] %>%
    rename(response = X1,
           key_resp_4_keys = X2,
           Inv = X3,
           Ori = X4) %>%
    mutate(response = ifelse(response == " ", NA, response)) %>%
    filter(!is.na(response)) %>%
    as_tibble()
}

################
# check gender #
################
attach(data_list)

table(data_list[[1]]$sex)
table(data_list[[2]]$sex)
table(data_list[[3]]$sex)
table(data_list[[4]]$sex)
table(data_list[[5]]$sex)
table(data_list[[6]]$sex)


###################################
# combine data & Make Name column #
###################################
all_df <- do.call("rbind", data_list)

all_df <- all_df %>% mutate(Name = paste0(name, stringr::str_sub(no, -4,-1)),
                            name_phone = paste0(name, stringr::str_sub(phone, -4, -1)))

# --------------------
# filter interet data 
# --------------------
#전체 파일에서 현재 연구(검사) 대상자 추출
par_list <- 
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1xVAX1lmceFKo4phHKhwbNWgxGc6innwvyCE6rye51EQ/edit#gid=240874620",
    sheet = '등록') %>%
    transmute(Name, email = 이메일...8, phone = 핸드폰, 
              std_id = 학번, name_phone = paste0(성명, stringr::str_sub(핸드폰, -4, -1)))

par_name_student_ID <- par_list$Name
par_studentID <- par_list$std_id
par_email <- par_list$email
par_name_phone <- par_list$name_phone

all_df <- 
  all_df %>% filter(
                    Name %in% par_name_student_ID |
                    no %in% par_studentID |
                    email %in% par_email |
                    name_phone %in% par_name_phone) %>% 
  select(-name_phone)


# -----------
# check data 
# -----------
#최종 포함 인원
all_df %>% select(email) %>% unique()

#2번 이상 검사했는지 2차 확인
all_df %>% select(Name) %>% count(Name) %>% filter(n > 300)

# ---------------------------------
# par_list의 name_student_ID로 통일 
# ---------------------------------

#통일 전에 확인
all_df %>%
  left_join(par_list %>% transmute(Name, GS = 'google_spreadsheet') %>% unique(), by = "Name") %>%
  select(Name, GS) %>% 
  unique() %>% 
  filter(is.na(GS))

all_df %>%
  left_join(par_list %>% transmute(Name, GS = 'google_spreadsheet') %>% unique(), by = "Name") %>%
  select(Name, GS) %>% 
  unique() %>% 
  View()

#통일(현재 19명)
all_df <-
  all_df %>%
  mutate(Name = case_when(
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          email == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          Name == '*****' ~ '*****',
                          T ~ Name
                          ))

#재차 확인
all_df %>%
  left_join(par_list %>% transmute(Name, GS = 'google_spreadsheet') %>% unique(), by = "Name") %>%
  select(Name, GS) %>% 
  unique() %>% 
  View()


# ----------------
# data preprocess 
# ----------------

par_data_processed <- all_df %>%
  mutate(response = tidyr::replace_na(response, "None"),
         ID = as.numeric(stringr::str_extract(Ori, '[0-9]{5}'))) %>%
  select(email, sex, ID, response, name, age, date, no, phone, Name) %>%
  filter(!email == "js94park@naver.com") %>%
  mutate(response = case_when(response == "J" ~ "j",
                              TRUE ~ "f"))

par_data_processed %>% 
  select(Name) %>% 
  unique() %>% 
  googlesheets4::sheet_write(
    "https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=815479940", 
    sheet = "CI")

save(par_data_processed, file = "graduate.Rdata")

load(file = "graduate.Rdata")

#dataname = par_data_processed
# load(file = "graduate.Rdata")

par_female <- par_data_processed %>% 
  filter(sex == 2)

par_male <- par_data_processed %>% 
  filter(sex == 1)

f_data <- readxl::read_excel("./CI_analysis_criter./female_data_CI_210203.xlsx")
m_data <- readxl::read_excel("./CI_analysis_criter./male_data_CI_210203.xlsx")

f_rate_data <- f_data %>% 
  tidyr::pivot_longer(ang_f_value:warm_j_value, names_to = "emotion", values_to = "value") %>% 
  tidyr::separate(emotion, sep = "_", into = c("valence", "response", "remainW")) %>% 
  select(ID, valence, response, value)

m_rate_data <- m_data %>% 
  tidyr::pivot_longer(ang_f_value:warm_j_value, names_to = "emotion", values_to = "value") %>% 
  tidyr::separate(emotion, sep = "_", into = c("valence", "response", "remainW")) %>% 
  select(ID, valence, response, value)

f_result_long <- par_female %>% 
  select(Name, email, sex, ID, response) %>% 
  left_join(f_rate_data, by = c("ID", "response")) %>%
  group_by(Name, valence) %>% 
  summarise(value_sum = mean(value, na.rm =T))


m_result_long <- par_male %>% 
  select(Name, email, sex, ID, response) %>% 
  left_join(m_rate_data, by = c("ID", "response")) %>%
  group_by(Name, valence) %>% 
  summarise(value_sum = mean(value, na.rm =T))

par_processed <- rbind(f_result_long, m_result_long)

par_selfCI_rated <- par_processed %>%
  mutate(valence = stringr::str_to_upper(valence),
         valence = paste0("CI_",valence)) 

# par_selfCI_rated %>%
#   writexl::write_xlsx("21_1_graduate_selfCI_rated.xlsx")

#save as Rdata  
save(par_selfCI_rated, file = "graduate_selfCI_rated.Rdata")
# 
#dataname = par_selfCI_rated
# load("graduate_selfCI_rated.Rdata")

