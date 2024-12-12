# --------
# Setting
# --------
#JAVA 8로 환경 변수 설정
# Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk1.8.0_231.jdk/Contents/Home') #ZSU_17
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home') #ZSU_15

#JAVA 8로 환경 변수 설정되어 있는지 확인
Sys.getenv('JAVA_HOME')

pacman::p_load('rJava', 'mailR', 'devtools', 'tidyverse', 'googlesheets4')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('email_inhalt.R')

# -----------------
# list to send mail
# -----------------

d <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1xVAX1lmceFKo4phHKhwbNWgxGc6innwvyCE6rye51EQ/edit#gid=240874620',
    sheet = '등록') %>% 
  mutate(신청일 = lubridate::as_date(신청일)) %>% 
  filter(신청일 >= lubridate::today())

# =============
# 공통필수검사 
# =============

# -------
# check 
# -------

par <- d %>% 
  transmute(email = 이메일...8)

par <- par %>% mutate(email = gsub(' ', '', email))

par <- par %>% 
  mutate(email = gsub(' ', '', email),
         email = gsub(',', '.', email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, '\\.con'))

par <- par %>%
  mutate(email = gsub('\\.con', '\\.com', email))

#check email
rbind(
  par %>% filter(str_detect(email, 'kroea\\.')),
  par %>% filter(str_detect(email, 'gamil\\.')),
  par %>% filter(str_detect(email, 'kprea\\.')),
  par %>% filter(str_detect(email, 'korra\\.')),
  par %>% filter(str_detect(email, 'korrea\\.')),
  par %>% filter(str_detect(email, 'koreq\\.')),
  par %>% filter(str_detect(email, 'korae\\.')),
  par %>% filter(str_detect(email, '\\.ke')),
  par %>% filter(str_detect(email, 'naveer\\.')),
  par %>% filter(str_detect(email, 'navaer\\.')),
  par %>% filter(str_detect(email, '\\.acc\\.kr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.jr')),
  par %>% filter(str_detect(email, 'korea\\.com')),
  par %>% filter(str_detect(email, 'korea\\.c\\.kr')),
  par %>% filter(str_detect(email, 'ac\\.mr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.r')),
  par %>% filter(str_detect(email, 'navee\\.')),
  par %>% filter(str_detect(email, 'portal\\.korea')),
  par %>% filter(str_detect(email, 'korea\\.co\\.kr')),
  par %>% filter(str_detect(email, '[가-힣]')),
  par %>% filter(!str_detect(email, '@')),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

par <- 
  rbind(tibble(email = 'js94park@naver.com', n = 0), par) %>% 
  mutate(title = '[고려대학교 학생상담센터] 대학원생 심리지원 검사 안내 - 공통 필수검사',
         inhalt = everyone_mh)


# ----------------------------
# send only to the first email
# ----------------------------

{
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[1], # 메일제목
            body         = par$inhalt[1],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            html = TRUE,
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            # attach.files = c(par$file_directory[1], par$file_directory_2[1]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                    
  
  
  print(paste0(1, 'th email sent to [', par$email[1],'], ', length(par$email) - 1, ' email left'))
}


# -----
# send
# -----
for(i in 1:length(par$email)){
  
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[i], # 메일제목
            body         = par$inhalt[i],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            # attach.files = c(par$file_directory[i], par$file_directory_2[i]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                
  
  
  print(paste0(i, 'th email sent to [', par$email[i],'], ', length(par$email) - i, ' email left'))
  
}


# =========
# 자기이해 
# =========

# -------
# check 
# -------

par <- d %>% 
  transmute(email = 이메일...8, 자기이해) %>% 
  filter(자기이해 == 1)

par <- par %>% mutate(email = gsub(' ', '', email))

par <- par %>% 
  mutate(email = gsub(' ', '', email),
         email = gsub(',', '.', email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, '\\.con'))

par <- par %>%
  mutate(email = gsub('\\.con', '\\.com', email))

#check email
rbind(
  par %>% filter(str_detect(email, 'kroea\\.')),
  par %>% filter(str_detect(email, 'gamil\\.')),
  par %>% filter(str_detect(email, 'kprea\\.')),
  par %>% filter(str_detect(email, 'korra\\.')),
  par %>% filter(str_detect(email, 'korrea\\.')),
  par %>% filter(str_detect(email, 'koreq\\.')),
  par %>% filter(str_detect(email, 'korae\\.')),
  par %>% filter(str_detect(email, '\\.ke')),
  par %>% filter(str_detect(email, 'naveer\\.')),
  par %>% filter(str_detect(email, 'navaer\\.')),
  par %>% filter(str_detect(email, '\\.acc\\.kr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.jr')),
  par %>% filter(str_detect(email, 'korea\\.com')),
  par %>% filter(str_detect(email, 'korea\\.c\\.kr')),
  par %>% filter(str_detect(email, 'ac\\.mr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.r')),
  par %>% filter(str_detect(email, 'navee\\.')),
  par %>% filter(str_detect(email, 'portal\\.korea')),
  par %>% filter(str_detect(email, 'korea\\.co\\.kr')),
  par %>% filter(str_detect(email, '[가-힣]')),
  par %>% filter(!str_detect(email, '@')),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

par <- 
  rbind(tibble(email = 'js94park@naver.com', 자기이해 = 1, n = 0), par) %>% 
  mutate(title = '[고려대학교 학생상담센터] 대학원생 심리지원 검사 안내 - 자기이해 프로그램',
         inhalt = self_understand)

# par %>% writexl::write_xlsx('자기이해_발송명단.xlsx')


# ----------------------------
# send only to the first email
# ----------------------------

{
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[1], # 메일제목
            body         = par$inhalt[1],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            html = TRUE,
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            # attach.files = c(par$file_directory[1], par$file_directory_2[1]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                    
  
  
  print(paste0(1, 'th email sent to [', par$email[1],'], ', length(par$email) - 1, ' email left'))
}

# -----
# send
# -----
for(i in 1:length(par$email)){
  
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[i], # 메일제목
            body         = par$inhalt[i],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            # attach.files = c(par$file_directory[i], par$file_directory_2[i]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                
  
  
  print(paste0(i, 'th email sent to [', par$email[i],'], ', length(par$email) - i, ' email left'))
  
}


# =============
# 스트레스 관리 
# =============

# -------
# check 
# -------

par <- d %>% 
  transmute(email = 이메일...8, 스트레스) %>% 
  filter(스트레스 == 1)

par <- par %>% mutate(email = gsub(' ', '', email))

par <- par %>% 
  mutate(email = gsub(' ', '', email),
         email = gsub(',', '.', email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, '\\.con'))

par <- par %>%
  mutate(email = gsub('\\.con', '\\.com', email))

#check email
rbind(
  par %>% filter(str_detect(email, 'kroea\\.')),
  par %>% filter(str_detect(email, 'gamil\\.')),
  par %>% filter(str_detect(email, 'kprea\\.')),
  par %>% filter(str_detect(email, 'korra\\.')),
  par %>% filter(str_detect(email, 'korrea\\.')),
  par %>% filter(str_detect(email, 'koreq\\.')),
  par %>% filter(str_detect(email, 'korae\\.')),
  par %>% filter(str_detect(email, '\\.ke')),
  par %>% filter(str_detect(email, 'naveer\\.')),
  par %>% filter(str_detect(email, 'navaer\\.')),
  par %>% filter(str_detect(email, '\\.acc\\.kr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.jr')),
  par %>% filter(str_detect(email, 'korea\\.com')),
  par %>% filter(str_detect(email, 'korea\\.c\\.kr')),
  par %>% filter(str_detect(email, 'ac\\.mr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.r')),
  par %>% filter(str_detect(email, 'navee\\.')),
  par %>% filter(str_detect(email, 'portal\\.korea')),
  par %>% filter(str_detect(email, 'korea\\.co\\.kr')),
  par %>% filter(str_detect(email, '[가-힣]')),
  par %>% filter(!str_detect(email, '@')),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

par <- 
  rbind(tibble(email = 'js94park@naver.com', 스트레스 = 1, n = 0), par) %>% 
  mutate(title = '[고려대학교 학생상담센터] 대학원생 심리지원 검사 안내 - 스트레스 관리 프로그램',
         inhalt = stress_manage)

# par %>% writexl::write_xlsx('스트레스관리프로그램_발송명단.xlsx')

# ----------------------------
# send only to the first email
# ----------------------------

{
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[1], # 메일제목
            body         = par$inhalt[1],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            html = TRUE,
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            # attach.files = c(par$file_directory[1], par$file_directory_2[1]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                    
  
  
  print(paste0(1, 'th email sent to [', par$email[1],'], ', length(par$email) - 1, ' email left'))
}

# -----
# send
# -----
for(i in 1:length(par$email)){
  
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[i], # 메일제목
            body         = par$inhalt[i],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            # attach.files = c(par$file_directory[i], par$file_directory_2[i]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                
  
  
  print(paste0(i, 'th email sent to [', par$email[i],'], ', length(par$email) - i, ' email left'))
  
}

# =========
# 적응기제 
# =========

# -------
# check 
# -------

par <- d %>% 
  transmute(email = 이메일...8, 적응기제) %>% 
  filter(적응기제 == 1)

par <- par %>% mutate(email = gsub(' ', '', email))

par <- par %>% 
  mutate(email = gsub(' ', '', email),
         email = gsub(',', '.', email)) %>% 
  mutate(n = 1:n())

#.con
par %>% filter(str_detect(email, '\\.con'))

par <- par %>%
  mutate(email = gsub('\\.con', '\\.com', email))

#check email
rbind(
  par %>% filter(str_detect(email, 'kroea\\.')),
  par %>% filter(str_detect(email, 'gamil\\.')),
  par %>% filter(str_detect(email, 'kprea\\.')),
  par %>% filter(str_detect(email, 'korra\\.')),
  par %>% filter(str_detect(email, 'korrea\\.')),
  par %>% filter(str_detect(email, 'koreq\\.')),
  par %>% filter(str_detect(email, 'korae\\.')),
  par %>% filter(str_detect(email, '\\.ke')),
  par %>% filter(str_detect(email, 'naveer\\.')),
  par %>% filter(str_detect(email, 'navaer\\.')),
  par %>% filter(str_detect(email, '\\.acc\\.kr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.jr')),
  par %>% filter(str_detect(email, 'korea\\.com')),
  par %>% filter(str_detect(email, 'korea\\.c\\.kr')),
  par %>% filter(str_detect(email, 'ac\\.mr')),
  par %>% filter(str_detect(email, '\\.ackr')),
  par %>% filter(str_detect(email, 'ac\\.r')),
  par %>% filter(str_detect(email, 'navee\\.')),
  par %>% filter(str_detect(email, 'portal\\.korea')),
  par %>% filter(str_detect(email, 'korea\\.co\\.kr')),
  par %>% filter(str_detect(email, '[가-힣]')),
  par %>% filter(!str_detect(email, '@')),
  
  par %>% mutate(string_length = nchar(email)) %>% filter(string_length < 12) %>% select(-string_length)
) %>% 
  arrange(n) %>% 
  unique() %>% 
  View()

par <- 
  rbind(tibble(email = 'js94park@naver.com', 적응기제 = 1, n = 0), par) %>% 
  mutate(title = '[고려대학교 학생상담센터] 대학원생 심리지원 검사 안내 - 적응기제 프로그램',
         inhalt = aptitude)

# par %>% writexl::write_xlsx('적응기제프로그램_발송명단.xlsx')

# ----------------------------
# send only to the first email
# ----------------------------

{
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[1],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[1], # 메일제목
            body         = par$inhalt[1],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = ''***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            html = TRUE,
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            # attach.files = c(par$file_directory[1], par$file_directory_2[1]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                    
  
  
  print(paste0(1, 'th email sent to [', par$email[1],'], ', length(par$email) - 1, ' email left'))
}


# -----
# send
# -----
for(i in 1:length(par$email)){
  
  send.mail(from         = 'psy_test@naver.com',                      # 보내는 사람 주소
            to           = par$email[i],                              # 받는 사람 주소
            # cc           = 'js94park@naver.com',                    # 참조
            # bcc           = 'js94park@naver.com',                   # 숨은 참조
            subject      = par$title[i], # 메일제목
            body         = par$inhalt[i],                         # 메일내용
            smtp         = list(host.name = 'smtp.naver.com',         # 메일서버 연동 정보
                                port = 587,
                                user.name = 'psy_test@naver.com',
                                passwd = '***************',
                                ssl = TRUE),
            encoding     = 'utf-8',                                   # 인코딩(고정값)
            authenticate = TRUE,                                      # 인증사용 여부(고정값)
            send         = TRUE,
            html = TRUE,
            # attach.files = c(par$file_directory[i], par$file_directory_2[i]),
            # file.names = c('option'),
            # file.descriptions = c('option'),
            debug = F)                                                
  
  
  print(paste0(i, 'th email sent to [', par$email[i],'], ', length(par$email) - i, ' email left'))
  
}

