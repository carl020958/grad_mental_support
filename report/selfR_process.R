setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('tidyverse')

# ============== #
# final_data_num #
# ============== #

# ------------
# import data
# ------------

index <- readxl::read_xlsx("index.xlsx")

weight <- googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1RjLoxineZI6VHRADgqScuem_u7vg6PZxJGTc6puNnG0/edit#gid=1431906331', 
  sheet = 'weight')

par_list <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1xVAX1lmceFKo4phHKhwbNWgxGc6innwvyCE6rye51EQ/edit#gid=240874620', 
  sheet = '등록') %>% 
  rename(이메일 = '이메일...8') %>% 
  select(Name, 학번, 나이, 핸드폰, 이메일, 자기이해, 적성검사, 스트레스, 적응기제)

d_1 <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0', 
  sheet = '정신건강')

d_2 <- 
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0', 
  sheet = '자기이해')

d_6 <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0', 
    sheet = 'MMPI')

d_3 <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0',
    sheet = '적성')

d_4 <-
  googlesheets4::read_sheet(
  'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0',
  sheet = '정서')

d_7 <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1Hyn_7u4icyToQ3bM_rE3pCLnfS_bErLOPyhR4_D9p8g/edit#gid=0',
    sheet = '시트1')

# load(
#   '/Users/jisu/Dropbox_Carl/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/graduate_selfCI_rated.Rdata') # ZSU17

load(
  '/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/graduate_selfCI_rated.Rdata'
  ) #ZSU 15

# ===========================================
# 데이터 1차 전처리(raw의 복수, 무응답 제거) 
# ===========================================

# ---------
# 정신건강 
# ---------
raw_processed_1 <-
  d_1 %>% 
  # mutate(Name = paste0(이름, str_sub(student_id, -4, -1))) %>%
  # relocate(Name, .before = 이름) %>% 
  filter(!is.na("20.중요한 일에 집중할 수가 없었다.")) %>% 
  distinct(Name, .keep_all = T)

raw_1 <- bind_cols(raw_processed_1[, 22] %>% rename(학번 = `4. 학번 (예: 2020123456)`), 
                   raw_processed_1[, 27:74])

# par_list와 정신건강 간 학번 상이 여부 확인
check_1 <- 
  raw_1 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_1

# raw_processed_1 %>% 
#   filter(`4. 학번 (예: 2020123456)` %in% check_1) %>% 
#   View()

# --------------------
# 정신건강 집단 구하기
# --------------------
mw_index <- raw_processed_1[22] %>% rename(학번 = `4. 학번 (예: 2020123456)`)

mw_items <- 
  index %>% 
  filter(scale %in% c('mw', 'md', 'md_sv')) %>% 
  pull(item)

mw_df <- 
  raw_processed_1 %>% 
  mutate(across(everything(), ~ substr(.x, 1, 1))) %>% 
  mutate(across(everything(), ~ as.numeric(.x))) %>% 
  select(mw_items)

names(mw_df) <- sprintf('mw%0d', 1:28)

mw_item_group1 <- sprintf('mw%0d', 1:3)
mw_item_group2 <- sprintf('mw%0d', 4:14)
mw_item_group3 <- sprintf('mw%0d', 15:25)
mw_item_group4 <- sprintf('mw%0d', 26:27)

mw_group <- 
  mw_df %>% 
  rowwise() %>% 
  mutate(flourish_con1 = sum(c_across(all_of(mw_item_group1)) >= 5),
         flourish_con2 = sum(c_across(all_of(mw_item_group2)) >= 5),
         flourish = case_when(flourish_con1 >= 1 & flourish_con2 >= 6 ~ TRUE,
                              T ~ FALSE),
         mb_con1 = sum(c_across(all_of(mw_item_group1)) <= 2),
         mb_con2 = sum(c_across(all_of(mw_item_group2)) <= 2),
         mb = case_when(mb_con1 >= 1 & mb_con2 >= 6 ~ TRUE,
                        T ~ FALSE),
         middle = case_when(flourish == FALSE & mb == FALSE ~ TRUE,
                            T ~ FALSE),
         md_con1 = sum(c_across(all_of(mw_item_group3)) >= 4),
         md_con2 = case_when(mw28 >= 4 ~ 1,
                             T ~ 0),
         md_con3 = case_when(md_con1 >= 1 & md_con2 >= 1 ~ TRUE,
                             T ~ FALSE),
         md_con4 = sum(c_across(all_of(mw_item_group4)) >= 4),
         md = case_when(md_con3 == TRUE | md_con4 >= 1 ~ TRUE,
                        T ~ FALSE)
  ) %>% 
  mutate(mh_group = case_when(flourish == T & md == F ~ '플로리시',
                              flourish == T & md == T ~ '플로리시 및 심리적 어려움',
                              middle == T & md == F ~ '중간수준의 정신건강',
                              middle == T & md == T ~ '중간수준의 정신건강 및 심리적 어려움',
                              mb == T & md == F ~ '정신적 쇠약',
                              mb == T & md == T ~ '정신적 쇠약 및 심리적 어려움')
  ) %>% 
  select(mh_group)

par_mw_group <- bind_cols(mw_index, mw_group)


# ---------
# 자기이해 
# ---------
raw_processed_2 <-
  d_2 %>% 
  filter(!is.na("177. 나는 다른 사람들로부터 예민한 사람이라는 평가를 받는다.")) %>% 
  distinct(Name, .keep_all = T)

raw_2 <- bind_cols(raw_processed_2[, 26] %>% rename(학번 = `4. 학번 (예: 2020123456)`), 
                   raw_processed_2[, 31:207])

# par_list와 정신건강 간 학번 상이 여부 확인
check_2 <- 
  raw_2 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_2

# raw_processed_2 %>%
#   filter(`4. 학번 (예: 2020123456)` %in% check_2) %>%
#   View()


# ---------
# 적성검사 
# ---------
raw_processed_3 <- 
  d_3 %>% 
  filter(!is.na('Timing - 클릭 수...529')) %>% 
  distinct(Name, .keep_all = T)

# 변수명 변경
raw_processed_3_questions <- raw_processed_3[, 42:529]

names(raw_processed_3_questions) <- 
  readxl::read_xlsx('index.xlsx', sheet = 'quest_solve') %>% 
  filter(assessment == 'aptitude_ver3') %>% 
  pull(questions)

raw_3 <- bind_cols(raw_processed_3[, 26] %>% rename(학번 = `4. 학번 (예: 2020123456)`),
                   raw_processed_3_questions)

# par_list와 정신건강 간 학번 상이 여부 확인
check_3 <- 
  raw_3 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_3

# raw_processed_3 %>%
#   filter(`4. 학번 (예: 2020123456)` %in% check_3) %>%
#   View()

# ----------
# 정서교양 
# ----------
raw_processed_4 <- 
  d_4 %>% 
  filter(!is.na('Timing - 클릭 수...77')) %>% 
  distinct(Name, .keep_all = T)

raw_4 <- bind_cols(raw_processed_4[, 22] %>% rename(학번 = `4. 학번 (예: 2020123456)`),
                   raw_processed_4[, 30:57], raw_processed_4[, 62:73])

# par_list와 정신건강 간 학번 상이 여부 확인
check_4 <- 
  raw_4 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_4

# raw_processed_4 %>%
#   filter(`4. 학번 (예: 2020123456)` %in% check_4) %>%
#   View()


# -----
# MMPI 
# -----
raw_processed_6 <-
  d_6 %>% 
  filter(!is.na(T_TRT2)) %>% 
  distinct(Name, .keep_all = T)

raw_6 <- bind_cols(raw_processed_6[, 4] %>% rename(학번 = 개인고유번호), 
                   raw_processed_6[, 13:31], raw_processed_6[, 134:157])

# par_list와 정신건강 간 학번 상이 여부 확인
check_6 <- 
  raw_6 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_6

# raw_processed_6 %>%
#   filter(개인고유번호 %in% check_6) %>%
#   View()

# ---------
# 적응기제 
# ---------
raw_7 <- 
  d_7 %>% 
  rename(Name = `이름+전화번호`) %>% 
  select(-"결과지 없는 경우 체크") %>% 
  left_join(par_list %>% select(Name, 학번), by = "Name") %>% 
  pivot_longer(-c(Name,학번), names_to = "적응기제", values_to = "value") %>% 
  left_join(weight, by = c("적응기제"="구분")) %>% 
  transmute(학번, 적응기제, final_value = value+점수) %>% 
  pivot_wider(names_from = 적응기제, values_from = final_value)

check_7 <- 
  raw_7 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_7

# ===========
# merge data
# ===========

# ------------
# 자기보고식 
# ------------
raw_processed_num <- 
  par_list %>% 
  left_join(raw_1, by = "학번") %>% 
  left_join(raw_2, by = "학번")


data_processed_num <- 
  raw_processed_num %>% 
  pivot_longer(-c(Name:적응기제), names_to = "item", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(substr(value, 1, 1))) %>% 
  inner_join(index, by = "item") %>%
  mutate(value = case_when(r == "r" ~ likert_max + likert_min - value,
                           TRUE ~ value)) %>% 
  group_by(학번, scale) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "scale", values_from = "value")


# -------------
# 문제풀이 형
# -------------
raw_processed_solve <- 
  raw_3 %>% 
  full_join(raw_4, by = '학번') %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(-학번, names_to = 'questions', values_to = 'value') %>% 
  mutate(value = substr(value, 1, 1),
         value = case_when(value %in% c('①', '➀') ~ '1',
                           value %in% c('②', '➁') ~ '2',
                           value %in% c('③', '➂') ~ '3',
                           value %in% c('④', '➃') ~ '4',
                           value %in% c('⑤', '➄') ~ '5',
                           T ~ value)) %>% 
  left_join(
    readxl::read_xlsx('index.xlsx', sheet = 'quest_solve') %>% 
      select(-assessment), by = 'questions') %>% 
  filter(!is.na(answer)) %>% 
  filter(!is.na(value)) %>% 
  mutate(correct = case_when(value == answer ~ 1,
                             T ~ 0)) %>% 
  group_by(학번, scale) %>% 
  summarise(value = sum(correct)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = 'scale', values_from = 'value') %>% 
  mutate(학번 = as.numeric(학번)) %>% 
  select(학번, 공간지각, 공통성, 동형찾기, 모양, 산수, 상식, 숫자, 어휘, 짝짓기, 정서조절, 정서지각)

raw_processed_solve %>% 
  googlesheets4::sheet_write(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
    sheet = 'aptitdue_emotion'
  )

# ----------
# merge all
# ----------
final_data_num <- 
  par_list %>% 
  left_join(data_processed_num, by = '학번') %>% 
  # left_join(
  #   googlesheets4::read_sheet(
  #     'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0',
  #     sheet = 'aptitude_emotion_converted', by = '학번')
  # )
  left_join(raw_6, by = '학번') %>% 
  left_join(par_selfCI_rated %>% 
            pivot_wider(names_from = valence, values_from = value_sum), by = "Name") %>% 
  left_join(raw_7, by = '학번')

# ----------
# save data
# ----------
# googlesheet로
final_data_num %>% 
  googlesheets4::sheet_write(
  'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
  sheet = 'final_num'
)


# ============== #
# final_data_chr #
# ============== #

# -------------
# import data
# -------------
par_list
  # <- 
  # googlesheets4::read_sheet(
  #   'https://docs.google.com/spreadsheets/d/1xVAX1lmceFKo4phHKhwbNWgxGc6innwvyCE6rye51EQ/edit#gid=240874620', 
  #   sheet = '등록') %>% 
  # rename(이메일 = '이메일...8') %>% 
  # select(Name, 학번, 나이, 핸드폰, 이메일, 자기이해, 적성검사, 스트레스, 적응기제)

d_5 <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1rIVEzPhPfj2TWKk9LKOBLwc1GfLJPh322rpIw6x2HV0/edit#gid=0', 
    sheet = '스트레스')


# ===========================================
# 데이터 1차 전처리(raw의 복수, 무응답 제거) 
# ===========================================

# ---------
# 스트레스
# ---------
raw_processed_5 <-
  d_5 %>% 
  filter(!is.na("30. 가족이나 친구들이 인정하는 나의 강점은")) %>% 
  distinct(Name, .keep_all = T)

raw_5 <- bind_cols(raw_processed_5[, 26] %>% rename(학번 = `4. 학번 (예: 2020123456)`), 
                   raw_processed_5[, 31:111])

# par_list와 정신건강 간 학번 상이 여부 확인
check_5 <- 
  raw_5 %>% 
  left_join(par_list, by = "학번") %>% 
  select(학번, Name) %>% 
  filter(is.na(Name)) %>% 
  pull(학번)

check_5

# raw_processed_5 %>% 
#   filter(`4. 학번 (예: 2020123456)` %in% check_5) %>% 
#   View()

# ----------
# merge data
# ----------

final_data_chr <- 
  par_list %>% 
  left_join(raw_5, by = "학번") %>% 
  left_join(par_mw_group, by = "학번")

# ----------
# save data
# ----------
# googlesheet로
final_data_chr %>% 
  googlesheets4::sheet_write(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
    sheet = 'final_chr'
  )
