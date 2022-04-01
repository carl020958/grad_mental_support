###21_1_graduate_support_stress_manage

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('flexdashboard', 'highcharter', 'tidyverse', 'kableExtra', 'plotly')

# -----------
# 수검자 세팅
# -----------
# parID <- '이송원0144'
parName <- substr(parID, 1, nchar(parID)-4)

# -----------
# 디자인 세팅
# -----------
colSet <- c('#ff7c80','#ffcd2d','#f8cbad','#99ccff','#dae9f6','#99cc00')

# ---------
# 함수 세팅 
# ---------
# .percent <- function(x){ x / 100}

.left_color_bar <- 
  function(color, width){
    paste0('<span style="display: inline-block; direction: ltr; 
         unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; 
         background-color:', color,'; width: ', width, '%">', width,'</span>')
  }

# ------------
# 문항 꾸러미
# ------------
# SCT
sct_theme <- c('어머니', '아버지', '가족', 
               '여성', '남성', '이성 및 결혼', 
               '친구', '권위자', 
               '두려움', '죄책감',
               '과거', '미래', '자신의 능력', '목표')

# 정서교양-SCT
emo_sct_theme <- c('정서적 웰빙', '사회적 웰빙', '심리적 웰빙', '긍정성', '심리적 어려움')

# MMPI

# 정신건강
mw <- c('mw_emotional', 'mw_psychological', 'mw_social', 'md')

# 자아상 
CI <- c('CI_DEP', 'CI_ANX', 'CI_ANG', 'CI_POSI', 'CI_ATTRACTIVE', 'CI_TRUST', 'CI_COMPETENCE', 'CI_WARM')


# ---------
# data set
# ---------
# 
# {
#     
#     index <- 
#       readxl::read_xlsx(
#         '/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/index.xlsx',
#         sheet = 'chr')
#     
#     # SCT 문항
#     sct_question <-
#       index %>%
#       filter(scale == 'sct') %>%
#       pull(question)
# 
#     # 정서교양 SCT 문항
#     emo_sct_question <-
#       index %>%
#       filter(scale == 'emotion_sct') %>%
#       pull(question)
# 
#     
#     d <-
#       googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#       sheet = 'final_num')
#     
#     
#     sentiment_df <- 
#       googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#       sheet = 'sentiment')
#       
#       
#     long <- d %>%
#       pivot_longer(cesd_r:CI_WARM, names_to = 'cate', values_to = 'value') %>%
#       mutate(value = as.numeric(value)) %>%
#       group_by(cate) %>%
#       mutate(percentile = round(percent_rank(value)*100,1),
#              meanByCate = mean(value, na.rm=T),
#              medianByCate = median(value, na.rm=T)) %>%
#       ungroup()
# 
#     d_chr <-
#       googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#       sheet = 'final_chr')
# 
# }



# =====
# plots
# =====

# ----
# SCT
# ----
{
  # participant sentiment percentile by sct theme
  par_sct <- 
    sentiment_df %>%
      left_join(index %>% select(question, group), by = c('항목' = 'question')) %>% 
      group_by(Name, group) %>% 
      summarise(sentiment_mean_by_theme = mean(as.numeric(sentiment), na.rm = T),
                magnitude_mean_by_theme = mean(as.numeric(magnitude), na.rm = T)) %>% 
      ungroup() %>% 
      group_by(group) %>% 
      mutate(sentiment_percentile_by_theme = round(percent_rank(sentiment_mean_by_theme)*100,1),
             magnitude_percentile_by_theme = round(percent_rank(magnitude_mean_by_theme)*100,1)) %>% 
      ungroup() %>% 
      filter(Name == parID & group %in% sct_theme) %>% 
      arrange(match(group, sct_theme))


  # SCT
  sct_kable <-
    par_sct %>% 
    mutate(sentiment_percentile_by_theme = 
             # formattable::color_bar('#3CB371', fun = .percent)(sentiment_percentile_by_theme),
             .left_color_bar('#3CB371',sentiment_percentile_by_theme),
           magnitude_percentile_by_theme = 
             # formattable::color_bar('#20B2AA', fun = .percent)(magnitude_percentile_by_theme)) %>%
             .left_color_bar('#20B2AA', magnitude_percentile_by_theme)) %>%
    transmute(주제 = group, 
                '긍정성(%)' = sentiment_percentile_by_theme, 
                '정서강도(%)' = magnitude_percentile_by_theme) %>% 
    knitr::kable(format = 'html',
                 escape = F,
                 align = c('c', 'l', 'l')) %>%
    kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
    kable_styling(fixed_thead = T) %>%
    column_spec(1,
                width = '2.2cm',
                color = '#000000',
                background = c(
                  '#F8F8F7', '#EFEFEC', '#E5E5E2', # 어머니, 아버지, 가족
                  '#DCDCD7', '#D3D3CD', '#CACAC2', # 여성, 남성, 이성 및 결혼
                  '#C1C1B8', '#B8B8AD', # 친구, 권위자
                  '#AFAFA3', '#A5A598', # 두려움, 죄책감
                  '#9C9C8E', '#939383', '#8A8A79', '#808070' # 과거, 미래, 자신의 능력, 목표
                )
                ) %>%
    column_spec(2, popover = paste0('백분위: ', par_sct$sentiment_percentile_by_theme, '%')) %>%
    column_spec(3, popover = paste0('백분위: ', par_sct$magnitude_percentile_by_theme, '%')) %>%
    row_spec(0, color = 'white', background = '#990000') %>%
    column_spec(c(2:3), color = '#000000', bold = T) %>%
    footnote(
    general =
    c('* 긍정성 백분위 점수가 높을수록 이 검사를 응답한 다른 사람에 비해 해당 대상에 대해 더 긍정적으로 느낌을 의미합니다.
  * 정서강도 백분위 점수가 높을수록 이 검사를 응답한 다른 사람에 비해 해당 대상에 대해 정서적으로 더 강하게 느낌을 의미합니다.')
      )
  
  # SCT 응답
  sct_reply <-
    sentiment_df %>%
    filter(Name == parID & 항목 %in% sct_question) %>%
    select(-c(Name, sentiment, magnitude)) %>%
    left_join(index %>% select(question, group), by = c('항목' = 'question')) %>%
    arrange(match(group, sct_theme)) %>%
    mutate(항목 = str_sub(항목, 4, nchar(항목))) %>%
    mutate(항목 = case_when(str_sub(항목, 1, 1) == ' ' ~ str_sub(항목, 2, nchar(항목)),
                            T ~ 항목)) %>% 
    transmute(주제 = group, 항목, 응답) %>%
    knitr::kable(format = "html",
                 escape = F,
                 align = c('c', 'c', 'l')) %>% 
    kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
    kable_styling(fixed_thead = T) %>%
    column_spec(1,
                # width = '2.2cm',
                color = '#000000',
                background = c(
                  rep('#F8F8F7', 4), # 어머니
                  rep('#EFEFEC', 4), # 아버지
                  rep('#E5E5E2', 4), # 가족
                  rep('#DCDCD7', 3), # 여성
                  rep('#D3D3CD', 3), # 남성
                  rep('#CACAC2', 4), # 이성 및 겴혼
                  rep('#C1C1B8', 4), # 친구
                  rep('#B8B8AD', 2), # 권위자
                  rep('#AFAFA3', 4), # 두려움
                  rep('#A5A598', 4), # 죄책감
                  rep('#9C9C8E', 3), # 과거
                  rep('#939383', 5), # 미래
                  rep('#8A8A79', 4), # 자신의 능력
                  rep('#808070', 3) # 목표
                )
    ) %>%
    row_spec(0, color = 'white', background = '#990000') %>%
    column_spec(3, color = '#000000', bold = T) %>%
    collapse_rows(columns = 1)
}
    
# -------------
# 정서교양 SCT
# -------------
{
  # participant sentiment percentile by emo_sct theme
  par_emo_sct <- 
    sentiment_df %>%
    left_join(index %>% select(question, group), by = c('항목' = 'question')) %>% 
    group_by(Name, group) %>% 
    summarise(sentiment_mean_by_theme = mean(as.numeric(sentiment), na.rm = T),
              magnitude_mean_by_theme = mean(as.numeric(magnitude), na.rm = T)) %>% 
    ungroup() %>% 
    group_by(group) %>% 
    mutate(sentiment_percentile_by_theme = round(percent_rank(sentiment_mean_by_theme)*100,1),
           magnitude_percentile_by_theme = round(percent_rank(magnitude_mean_by_theme)*100,1)) %>% 
    ungroup() %>% 
    filter(Name == parID & group %in% emo_sct_theme) %>% 
    arrange(match(group, emo_sct_theme))
  
  
  # 정서교양 SCT 주제별
  emo_sct_kable <-
    par_emo_sct %>% 
    mutate(sentiment_percentile_by_theme = 
             # formattable::color_bar('#3CB371', fun = .percent)(sentiment_percentile_by_theme),
             .left_color_bar('#3CB371',sentiment_percentile_by_theme),
           magnitude_percentile_by_theme = 
             # formattable::color_bar('#20B2AA', fun = .percent)(magnitude_percentile_by_theme)) %>%
             .left_color_bar('#20B2AA', magnitude_percentile_by_theme)) %>%
    transmute(주제 = group, 
                '긍정성(%)' = sentiment_percentile_by_theme, 
                '정서강도(%)' = magnitude_percentile_by_theme) %>% 
    knitr::kable(format = "html",
                 escape = F,
                 align = c('c', 'l', 'l')) %>%
    kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
    kable_styling(fixed_thead = T) %>%
    column_spec(1,
                width = '2.5cm',
                color = '#000000',
                background = c(
                  '#F8F8F7', # 정서적 웰빙
                  '#DCDCD7', # 사회적 웰빙
                  '#C1C1B8', # 심리적 웰빙
                  '#A5A598' # 심리적 어려움
                )
    ) %>%
    column_spec(2, popover = paste0('백분위: ', par_emo_sct$sentiment_percentile_by_theme, '%')) %>%
    column_spec(3, popover = paste0('백분위: ', par_emo_sct$magnitude_percentile_by_theme, '%')) %>%
    row_spec(0, color = 'white', background = '#990000') %>%
    column_spec(c(2:3), color = '#000000', bold = T) %>%
    footnote(
    general =
      c('* 긍정성 백분위 점수가 높을수록 이 검사를 응답한 다른 사람에 비해 해당 대상에 대해 더 긍정적으로 느낌을 의미합니다.
* 정서강도 백분위 점수가 높을수록 이 검사를 응답한 다른 사람에 비해 해당 대상에 대해 정서적으로 더 강하게 느낌을 의미합니다.')
    )

  
  # 정서교양 SCT 응답
  emo_sct_reply <-
    sentiment_df %>%
    filter(Name == parID & 항목 %in% emo_sct_question) %>%
    select(-c(Name, sentiment, magnitude)) %>%
    left_join(index %>% select(question, group), by = c('항목' = 'question')) %>%
    arrange(match(group, emo_sct_theme)) %>%
    mutate(항목 = str_sub(항목, 4, nchar(항목))) %>%
    mutate(항목 = case_when(str_sub(항목, 1, 1) == ' ' ~ str_sub(항목, 2, nchar(항목)),
                          T ~ 항목)) %>% 
    transmute(주제 = group, 항목, 응답) %>%
    knitr::kable(format = "html",
                 escape = F,
                 align = c('c', 'c', 'l')) %>% 
    kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
    kable_styling(fixed_thead = T) %>%
    column_spec(1,
                # width = '2.2cm',
                color = '#000000',
                background = c(
                  rep('#F8F8F7', 3), # 정서적 웰빙
                  rep('#DCDCD7', 5), # 사회적 웰빙
                  rep('#C1C1B8', 6), # 심리적 웰빙
                  rep('#A5A598', 2)  # 심리적 어려움
                )
    ) %>%
    row_spec(0, color = 'white', background = '#990000') %>%
    column_spec(3, color = '#000000', bold = T) %>%
    collapse_rows(columns = 1)
}  

# -----
# MMPI
# -----

# MMPI-II 타당도 척도
{
  par_mmpi_validity <-
    d %>% 
    filter(Name == parID) %>% 
    pivot_longer(-c(Name:적응기제), names_to = 'cate', values_to = 'value') %>% 
    filter(cate %in% c('T_VRIN',	'T_TRIN',	'T_F',	'T_Fb',	'T_Fp',	'T_FBS',	'T_L',	'T_K',	'T_S')) %>% 
    arrange(match(cate, c('T_VRIN',	'T_TRIN',	'T_F',	'T_Fb',	'T_Fp',	'T_FBS',	'T_L',	'T_K',	'T_S')))
  
  
  hline <- 
    function(y = 65, color = 'black') {
    list(
      type = 'line', 
      x0 = 0, 
      x1 = 1, 
      xref = 'paper',
      y0 = y, 
      y1 = y, 
      line = list(color = color,
                  width = 1)
    )
  }

  hline2 <- 
    function(y = 50, color = 'black') {
      list(
        type = 'dot', 
        x0 = 0, 
        x1 = 1, 
        xref = 'paper',
        y0 = y, 
        y1 = y, 
        line = list(color = color,
                    width = 0.5)
      )
    }
  
  
  mmpi_validityPlot <- 
    plot_ly(par_mmpi_validity) %>%
    
    add_trace(x = ~cate,
              y = ~value,
              name = 't 점수',
              type = 'scatter',
              mode = 'lines + markers',
              yaxis = '',
              line = list(color = '#990000'),
              marker = list(color = '#990000'),
              hoverinfo = 'text',
              text = ~paste(value,'T')) %>%
    
    
    layout(
      title = '',
      
      bargap = 0.6,
      
      xaxis = list(title = '',
                   ticktext = list('VRIN', 'TRIN', 'F', 'Fb', 'Fp', 'FBS', 'L', 'K', 'S'),
                   tickvals = list(0,1,2,3,4,5,6,7,8),
                   showline = T
      ),
      
      yaxis = list(side = 'left',
                   title = '',
                   overlaying = 'y',
                   showgrid = T,
                   zeroline = F,
                   range = c(20,120)),
      
      legend = list(x = 100, 
                    y = 100),
      
      shapes = list(hline(65),
                    hline2(50))) %>%
    
    config(displayModeBar = F)
  }

# MMPI-II 임상 척도
{
  par_mmpi_clincial <-
    d %>% 
    filter(Name == parID) %>% 
    pivot_longer(-c(Name:적응기제), names_to = 'cate', values_to = 'value') %>% 
    filter(cate %in% c('T_KHs',	'T_D',	'T_Hy',	'T_KPd',	'Tg_Mf',	'T_Pa',	'T_KPt',	'T_KSc',	'T_KMa',	'T_Si')) %>% 
    arrange(match(cate, c('T_KHs',	'T_D',	'T_Hy',	'T_KPd',	'Tg_Mf',	'T_Pa',	'T_KPt',	'T_KSc',	'T_KMa',	'T_Si')))
  
  mmpi_clinicalPlot <- 
    plot_ly(par_mmpi_clincial) %>%
    
    add_trace(x = ~cate,
              y = ~value,
              name = 't 점수',
              type = 'scatter',
              mode = 'lines + markers',
              yaxis = '',
              line = list(color = '#990000'),
              marker = list(color = '#990000'),
              hoverinfo = 'text',
              text = ~paste(value,'T')) %>%
    
    
    layout(
      title = '',
      
      bargap = 0.6,
      
      xaxis = list(title = '',
                   ticktext = list('Hs', 'D', 'Hy', 'Pd', 'Mf', 'Pa', 'Pt', 'Sc', 'Ma', 'Si'),
                   tickvals = list(0,1,2,3,4,5,6,7,8,9),
                   showline = T
      ),
      
      
      yaxis = list(side = 'right',
                   title = '',
                   overlaying = 'y',
                   showgrid = T,
                   zeroline = F,
                   range = c(20,120)),
      
      legend = list(x = 100, 
                    y = 100),
      
      shapes = list(hline(65),
                    hline2(50))) %>%
    
    config(displayModeBar = F)
}

# MMPI-II 타당도 및 임상 척도
{
  mmpi_ori_value <- 
    d %>% 
    filter(Name == parID) %>% 
    pivot_longer(-c(Name:적응기제), names_to = 'cate', values_to = 'value') %>% 
    filter(cate %in% c('VRIN',	'TRIN',	'F',	'Fb',	'Fp',	'FBS',	'L',	'K',	'S',	
                        'Hs',	'D',	'Hy',	'Pd',	'Mf',	'Pa',	'Pt',	'Sc',	'Ma',	'Si')) %>% 
    arrange(match(cate, c('VRIN',	'TRIN',	'F',	'Fb',	'Fp',	'FBS',	'L',	'K',	'S',	
                          'Hs',	'D',	'Hy',	'Pd',	'Mf',	'Pa',	'Pt',	'Sc',	'Ma',	'Si'))) %>% 
    pivot_wider(names_from = cate, values_from = value) 
  
  mmpi_t_value <- 
    d %>% 
    filter(Name == parID) %>% 
    pivot_longer(-c(Name:적응기제), names_to = 'cate', values_to = 'value') %>% 
    filter(cate %in% c('T_VRIN',	'T_TRIN',	'T_F',	'T_Fb',	'T_Fp',	'T_FBS',	'T_L',	'T_K',	'T_S',
                       'T_KHs',	'T_D',	'T_Hy',	'T_KPd',	'Tg_Mf',	'T_Pa',	'T_KPt',	'T_KSc',	'T_KMa',	'T_Si')) %>% 
    arrange(match(cate, c('T_VRIN',	'T_TRIN',	'T_F',	'T_Fb',	'T_Fp',	'T_FBS',	'T_L',	'T_K',	'T_S',
                          'T_KHs',	'T_D',	'T_Hy',	'T_KPd',	'Tg_Mf',	'T_Pa',	'T_KPt',	'T_KSc',	'T_KMa',	'T_Si'))) %>% 
    pivot_wider(names_from = cate, values_from = value) 
  
  
  names(mmpi_t_value) <- names(mmpi_ori_value)
  mmpi_value <- rbind(mmpi_ori_value, mmpi_t_value)
  
  mmpi_kbl <- mmpi_value %>%
    select(-c(Name:적응기제)) %>% 
    mutate(구분 = c('원점수', 'T점수')) %>% 
    relocate(구분, .before = VRIN) %>% 
    knitr::kable(align = c('c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c',
                           'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')) %>% 
    kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
    row_spec(0, background = '#990000', color = 'white')
}


# -----------
# 심리적 건강
# -----------

# 정신건강
{
    mw_plot <- highchart() %>%
        hc_colors(
            color = colSet[3]
        ) %>%
        
        hc_chart(polar = T) %>%
        hc_xAxis(
            categories = c('정서적 웰빙', '심리적 웰빙', '사회적 웰빙', '심리적 어려움'),
            tickmarkPlacement = 'on',
            lineWidth = 0,
            labels = list(
                style = list(fontSize= '15px'))
        ) %>%
        hc_yAxis(
            gridLineInterpolation = polygon,
            lineWidth = 0,
            tickInterval = 25,
            min = 0,
            max = 100,
            labels = list(format = '',
                          enabled=F)
        ) %>%
        hc_plotOptions(
            column = list(colorByPoint = F),
            series = list(
                fillOpacity = 0.25,
                dataLabels = list(
                    enabled = T,
                    align = 'center',
                    verticalAlign =  'middle',
                    format = '{point.y}',
                    y = 0,
                    color = 'white',
                    style = list(fontFamily = 'noto')
                    
                ),
                dashStyle = 'ShortDot'
                
                
            )
        ) %>% 
        
        hc_add_series(
            name = '본인',
            data = long %>%
                filter(Name == parID & cate %in% mw) %>% 
                arrange(match(cate, mw)),
            hcaes(cate, percentile),
            marker = list(radius = 12),
            pointWidth = 20,
            pointPlacement = 'on',
            type = 'area',
            zIndex= 2,
            dataLabels = list(
                enabled = T,
                align= 'center',
                verticalAlign= 'middle',
                format = '{point.y:.0f}',
                y = 0,
                color='white',
                style = list(
                    fontFamily= 'noto', fontSize='20px')
            ),
            
            tooltip=list(crosshairs = T,
                         enabled= T,
                         headerFormat = '<b>본인</b><br>',
                         pointFormat = '원점수: <b>{point.value}</b><br>
                           백분위: <b>{point.percentile:.0f}%</br>') 
            
        ) %>%
        
        hc_add_series(
            name = '평균',
            data = long %>%
                filter(Name == parID & cate %in% mw) %>% 
                arrange(match(cate, mw)),
            hcaes(cate, 50),
            marker = list(radius = 2,
                          symbol = 'circle'),
            pointWidth = 20,
            pointPlacement = 'on',
            type = 'line',
            dataLabels = list(
                enabled = F),
            tooltip=list(crosshairs = T,
                         enabled= T,
                         headerFormat = '<b>타인 평균</b><br>',
                         pointFormat = '평균 점수: <b>{point.medianByCate}</b><br>
                           백분위: <b>50%</br>'),
            color = 'grey',
            zIndex = 1
        )
}


# 우울
{
  cesd_Plot <-
    highchart() %>%
    
    hc_chart(type = 'bar') %>%
    
    hc_colors(color = c(
        colSet[1])
    ) %>% 
    
    hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0,
        colorByPoint =F)
    )  %>%
    
    hc_xAxis(
        crosshair = F,
        
        title = list(text='우울'),
        tickmarkPlacement = 'on',
        lineWidth = 0,
        labels = list(
            enabled = F,
            style = list(fontSize= '15px'))
    ) %>%
    
    hc_yAxis(
        crosshair = T,
        min = 0,
        max = 100,
        labels = list(
            format = '{value}%'
        )
    ) %>%
    
    hc_add_series(
        name = '본인',
        data = long %>% filter(Name == parID & cate == 'cesd_r'),
        hcaes(cate, percentile),
        marker = list(radius = 12),
        type = 'column',
        dataLabels = list(
            format = '{point.y:.0f}',
            enabled = T,
            align= 'center',
            verticalAlign= 'middle',
            color='white',
            style = list(
                fontFamily= 'noto',
                fontSize='30px')
        
    )) %>%
    
    hc_tooltip(crosshairs = T,
               enabled= T,
               headerFormat = '<b>우울</b><br>',
               pointFormat = '본인점수: <b>{point.value}점</b><br>
               타인평균: {point.meanByCate:.1f}점')
}

# ------
# 자기상
# ------

# CI 결과물 주소
CI_list_self <- data.frame(
    # CI_path = list.files(
    # "/Users/jisu/Dropbox_Carl/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/cis", 
    # full.names = T) #ZSU 17
    CI_path = list.files(
      "/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/cis", 
      full.names = T) #ZSU 15
    ) %>%
    as_tibble() %>%
    mutate(CI_path = as.character(CI_path)) %>%
    mutate(index = str_extract(pattern = "(?<=participant\\_).*(?=_autoscaled)", string =  CI_path)) %>% 
    mutate(index = stringi::stri_trans_nfc(index))

posi_self_CI_list <- 
    data.frame(
      # CI_path = list.files(
      # "/Users/jisu/Dropbox_Carl/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/posi_self",
      # full.names = T) #ZSU 17
      CI_path = list.files(
        "/Users/zsu/Dropbox/JISU/Korea Uni/대학원/Kolab_Projekt/2021/대학원생_심리지원/report/common/CI/posi_self",
        full.names = T) #ZSU 15
    ) %>%
    as_tibble() %>%
    mutate(CI_path = as.character(CI_path),
           index = str_extract(pattern = "(?<=posi_self\\_).*(?=_autoscaled)", string = CI_path)) %>% 
    mutate(index = stringi::stri_trans_nfc(index))

self_CI_path <- d %>% 
    select(Name) %>% 
    left_join(CI_list_self %>% rename(Name = index), by = "Name")

posi_CI_path <- d %>% 
    select(Name) %>% 
    left_join(posi_self_CI_list %>% rename(Name = index), by = "Name")

# 자아상 8정서
{
    ciPlot <-
        highchart() %>%
        # hc_title(text = paste0(parName, '님 자기상 관련 평가')) %>%
        hc_colors(color = c(colSet[1], colSet[1], colSet[1], 
                            colSet[4], colSet[4], colSet[4], colSet[4], colSet[4]
                            )) %>% 
        hc_plotOptions(column = list(
            pointPadding = 0.2,
            borderWidth = 0,
            colorByPoint =T)
        )  %>%
        hc_xAxis(
            categories = c('우울', '불안', '분노', '긍정', '매력', '신뢰', '유능감', '따뜻함'),
            tickmarkPlacement = 'on',
            lineWidth = 0,
            labels = list(
                style = list(fontSize= '15px'))
        ) %>%
        hc_yAxis(
            min = 0,
            max = 100,
            crosshair = T,
            endOnTick = T,
            tickInterval = 10,
            labels = list(
                #            rotation = -45,
                format = '{value}%',
                style = list(fontSize = '12px'))
        ) %>%
        hc_plotOptions(column = list(
            pointPadding = 0.2,
            borderWidth = 0,
            colorByPoint =T),
            series = list(
                dataLabels = list(
                    enabled = T,
                    style = list(
                        fontFamily= 'noto', fontSize='25px'),
                    align= 'center',
                    verticalAlign= 'below',
                    format = '{point.y:.1f}',
                    color='white'))
        )  %>%
        hc_add_series(
            name = '본인',
            data =
                long %>%
                filter(Name == parID, cate %in% CI) %>%
                arrange(match(cate, CI)),
            hcaes(cate, percentile),
            marker = list(radius = 12),
            type = 'column',
            tooltip = list(
                crosshairs = T,
                enabled = T,
                headerFormat = '',
                # pointFormat = '점수: <b>{point.value:.2f}점</b><br>
                #     백분위: <b>{point.percentile}%</b>'
                pointFormat = '백분위: <b>{point.percentile}%</b>')
        )
        
}
