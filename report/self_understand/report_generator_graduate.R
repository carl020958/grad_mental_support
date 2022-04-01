pacman::p_load('rmarkdown', 'tidyverse')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---------
# data set
# ---------

{
  d <- 
    googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
    sheet = 'final_num')
  
  long <- d %>% 
    pivot_longer(cesd_r:CI_WARM, names_to = 'cate', values_to = 'value') %>% 
    mutate(value = as.numeric(value)) %>% 
    group_by(cate) %>% 
    mutate(percentile = round(percent_rank(value)*100,1),
           meanByCate = mean(value, na.rm=T),
           medianByCate = median(value, na.rm=T)) %>% 
    ungroup() 
  
  d_chr <- 
    googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
    sheet = 'final_chr')
}

par <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1xVAX1lmceFKo4phHKhwbNWgxGc6innwvyCE6rye51EQ/edit#gid=240874620',
    sheet = '등록')

par <- 
  par %>% 
  rename(자기이해_해석상담일 = '자기이해 \n해석상담일') %>% 
  select(Name, 자기이해_해석상담일) %>% 
  filter(!is.na(자기이해_해석상담일)) %>% 
  mutate(date = lubridate::as_date(stringr::str_sub(자기이해_해석상담일, 1, 10))) %>% 
  filter(date > lubridate::today()-1)

# generate report
for(i in 1:length(par$Name)) {
  parID <- par$Name[i]
  render(
    'reportPaper_graduate.Rmd',
    # file 2
    output_file =  paste(parID, ".html", sep = ''),
    output_dir = './html'
  )
}


# ---------------------------
# delete file in googledrive
# ---------------------------
drive_list <- 
  googledrive::drive_ls('https://drive.google.com/drive/u/0/folders/1fLkcIh_SVr3hzFL-yoloTZV6aCvI1y1U')

toUpload_html_list <- 
  tibble(file_list = list.files(path = 'html',
                                pattern = '*.html',
                                full.names = F)) %>% 
  mutate(file_list = stringi::stri_trans_nfc(file_list)) %>% 
  pull(file_list)

delete_list <- 
  drive_list %>% 
  filter(name %in% toUpload_html_list) %>% 
  pull(id)

for(i in 1:length(delete_list)){
  
  googledrive::drive_rm(googledrive::as_id(delete_list[i]))
  print(paste0(i, 'th file deleted, ', length(delete_list) - i, ' files left'))
  
}

# ---------------------------
# upload file in googledrive
# ---------------------------
all_files <-
  tibble(path = list.files(path = 'html', full.names = T)) %>%
  mutate(path = stringi::stri_trans_nfc(path)) %>%
  pull(path)

for(i in all_files){
  print(i)
  googledrive::drive_upload(
    media = i,
    path = googledrive::as_id('https://drive.google.com/drive/u/0/folders/1fLkcIh_SVr3hzFL-yoloTZV6aCvI1y1U')
  )
}

toMove_html_list <- 
  tibble(file_list = list.files(path = 'html',
                                pattern = '*.html',
                                full.names = T)) %>% 
  mutate(file_list = stringi::stri_trans_nfc(file_list)) %>% 
  pull(file_list)

# ---------
# move file
# ---------
for (i in 1:length(toMove_html_list)){
  
  file.copy(
    toMove_html_list[i], 
    '/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/자기이해/html/uploaded')
  file.remove(toMove_html_list[i])
  print(paste0(i, 'th file moved, ', length(toMove_html_list) - i, ' files left'))
  
}

