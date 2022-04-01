pacman::p_load('rmarkdown', 'tidyverse')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --------
# data set
# --------

{
  d <-
    googlesheets4::read_sheet(
      'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
      sheet = 'final_num')
  
  long <- 
    d %>%
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
  
  expert_percent <-
    d %>%
    select(Name, 이타주의:행동화) %>%
    pivot_longer(-Name, names_to = "cate", values_to = "value") %>%
    group_by(Name) %>%
    mutate(percentile = (value/sum(value))*100) %>%
    mutate(percentile = round(percentile, 1)) %>%
    ungroup() %>%
    transmute(Name, am = cate, percentile)
}

# participant
par <- d %>% filter(!is.na(행동화))

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

# ----------------------------
# upload file in googledrive 
# ----------------------------
all_files <-
  tibble(path = list.files(path = 'html', full.names = T)) %>%
  mutate(path = stringi::stri_trans_nfc(path)) %>%
  pull(path)

for(i in all_files){
  print(i)
  googledrive::drive_upload(
    media = i,
    path = googledrive::as_id('https://drive.google.com/drive/u/0/folders/1KUlzSzPk92Fpd6xy1Zk-IKYEVxjkradw')
  )
}

