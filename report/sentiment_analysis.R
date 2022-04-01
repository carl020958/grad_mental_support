setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load('tidyverse')

# ------------
# import data
# ------------
done_list <- 
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=1391046735',
    sheet = 'sentiment') %>% 
  select(Name) %>% 
  unique() %>% 
  pull()

d <-
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=1391046735',
    sheet = 'final_chr') %>%
  select(-mh_group) %>% 
  filter(!Name %in% done_list)

d_long <- 
  d %>% 
  select(-c(학번, 나이, 핸드폰, 이메일, 자기이해, 적성검사, 스트레스, 적응기제)) %>% 
  pivot_longer(-Name, names_to = '항목', values_to = '응답') %>% 
  filter(!is.na(응답))

# -----------------------------------
# import sentiment analysis function
# -----------------------------------
# python path
reticulate::use_python("/usr/local/bin/python3", required=T)

# #check
# reticulate::py_config()

# python script path
reticulate::source_python('/Users/jisu/PycharmProjects/grad_support/venv/sentiment_analysis.py')

# -------------
# run analysis
# -------------
df_sentiment <- 
  d_long %>%
  rowwise() %>% 
  mutate(sentiment = get_sentiment(응답)) %>% 
  mutate(magnitude = get_magnitude(응답)) %>% 
  ungroup()

final_df <- 
  bind_rows(
  googlesheets4::read_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=1391046735',
    sheet = 'sentiment'),
  df_sentiment)

final_df %>% 
  googlesheets4::write_sheet(
    'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=1391046735',
    sheet = 'sentiment')

# back up
# save(final_df, file = 'sentiment.Rdata')
