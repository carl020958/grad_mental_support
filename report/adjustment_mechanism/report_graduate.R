###21_1_graduate_support_aptitude_mechanism

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('flexdashboard', 'tidyverse', 'plotly', 'highcharter')

# -------------
# 수검자 세팅
# -------------
# parID <- '홍길동1234'
parName <- substr(parID, 1, nchar(parID)-4)

# ------------
# 디자인 세팅
# ------------
colSet <- c('#ff7c80','#ffcd2d','#f8cbad','#99ccff','#dae9f6','#99cc00')

# -----------
#문항 꾸러미
# -----------
# 적응기제
adp_mech <- c('이타주의',	'유머',	'억제',	'예상',	'승화',	'이지화',	'억압',	'전위',	
              '반동형성',	'해리',	'투사',	'공상',	'신체화',	'수동공격',	'행동화')

# 정신건강
mw <- c('mw_emotional', 'mw_psychological', 'mw_social', 'md')

# 자아상 
CI <- c('CI_DEP', 'CI_ANX', 'CI_ANG', 'CI_POSI', 'CI_ATTRACTIVE', 'CI_TRUST', 'CI_COMPETENCE', 'CI_WARM')

#  # --------
#  # data set
#  # --------
# {
#     d <-
#         googlesheets4::read_sheet(
#         'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#         sheet = 'final_num')
#     
#     long <- 
#         d %>%
#         pivot_longer(cesd_r:CI_WARM, names_to = 'cate', values_to = 'value') %>%
#         mutate(value = as.numeric(value)) %>%
#         group_by(cate) %>%
#         mutate(percentile = round(percent_rank(value)*100,1),
#                meanByCate = mean(value, na.rm=T),
#                medianByCate = median(value, na.rm=T)) %>%
#         ungroup()
#     
#   d_chr <-
#     googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#       sheet = 'final_chr')
# 
#     expert_percent <-
#       d %>%
#       select(Name, 이타주의:행동화) %>%
#       pivot_longer(-Name, names_to = "cate", values_to = "value") %>%
#       group_by(Name) %>%
#       mutate(percentile = (value/sum(value))*100) %>%
#       mutate(percentile = round(percentile, 1)) %>%
#       ungroup() %>%
#       transmute(Name, am = cate, percentile)
# }


# ======
# Plots
# ======

# --------
# 적응기제
# --------
# 적응기제 전문가 평정
{
  am_df <- 
    expert_percent %>% 
    filter(Name == parID) %>%
    pivot_wider(names_from = am, values_from = percentile)
  
  am_tib <- am_df
  
  Name <- c("부적응적인 삶을 사는 사람", "적응적인 삶을 사는 사람")
  이타주의 <- c(4,9); 유머 <- c(2,5); 억제 <- c(9,12); 예상 <- c(2,9); 승화 <- c(6,7);
  이지화 <- c(11,12); 억압 <- c(11,9); 전위 <- c(4,9); 반동형성 <- c(11,9);
  해리 <- c(11,9); 투사 <- c(6,2); 공상 <- c(6,2); 신체화 <- c(6,2); 수동공격 <- c(7,2); 행동화 <- c(4,2)
  
  am_example <- 
    data.frame(
      Name, 
      이타주의, 유머, 억제, 예상, 승화, 
      이지화, 억압, 전위, 반동형성, 
      해리, 투사, 공상, 신체화, 수동공격, 행동화)
  
  am_tib <- 
    rbind(am_example, am_df) %>% 
    # mutate(Name = gsub("[0-9]", "", Name))
    mutate(Name = ifelse(str_length(Name) > 12, Name, "본인"))
  
  # 그래프 순서
  am_tib <- rbind(am_tib %>% slice(1), am_tib %>% slice(3), am_tib %>% slice(2))
  
  # 그래프 순서
  # am_tib$Name <- factor(am_tib$Name, levels = c(am_tib[1,1],am_tib[3,1],am_tib[2,1]))
  
  altr <- am_tib %>% pull(이타주의)
  humor <- am_tib %>% pull(유머)
  suppr <- am_tib %>% pull(억제)
  antic <- am_tib %>% pull(예상)
  sublim <- am_tib %>% pull(승화)
  intel <- am_tib %>% pull(이지화)
  repres <- am_tib %>% pull(억압)
  displa <- am_tib %>% pull(전위) 
  reac_for <- am_tib %>% pull(반동형성)
  dissoc <- am_tib %>% pull(해리) 
  projecti <- am_tib %>% pull(투사)
  fanta <- am_tib %>% pull(공상) 
  somatizat <- am_tib %>% pull(신체화)
  pass_agg <- am_tib %>% pull(수동공격) 
  act_out <- am_tib %>% pull(행동화)
  
  am_y <- am_tib %>% pull(Name)
  
  am_labels <- c("행동화", "해리", "공상", "수동공격", "투사", "신체화", 
                 "전위", "이지화", "반동형성", "억압", 
                 "이타주의", "예상", "유머", "승화", "억제")

  am_p <- 
    plot_ly(
      am_tib, 
      type = 'bar', 
      orientation = 'h',
      wdith = 2500,
      marker = list(line = list(color = '#FFFFFF', width = 1))
      ) %>% 
    
    add_trace(
      x = ~ 행동화, 
      y = ~ Name, 
      marker = list(color = '#FD787A'),
      hovertemplate = paste(
        '적응기제: <b>행동화</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 해리, 
      y = ~ Name, 
      marker = list(color = '#FE8B8D'),
      hovertemplate = paste(
        '적응기제: <b>해리</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 공상, 
      y = ~ Name, 
      marker = list(color = '#FE9FA0'),
      hovertemplate = paste(
        '적응기제: <b>공상</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 수동공격, 
      y = ~ Name, 
      marker = list(color = '#FEB2B3'),
      hovertemplate = paste(
        '적응기제: <b>수동공격</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 투사, 
      y = ~ Name, 
      marker = list(color = '#FEC5C6'),
      hovertemplate = paste(
        '적응기제: <b>투사</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 신체화, 
      y = ~ Name, 
      marker = list(color = '#FFD9D9'),
      hovertemplate = paste(
        '적응기제: <b>신체화</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 전위, 
      y = ~ Name, 
      marker = list(color = '#85AC9A'),
      hovertemplate = paste(
        '적응기제: <b>전위</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 이지화, 
      y = ~ Name, 
      marker = list(color = '#9DBCAD'),
      hovertemplate = paste(
        '적응기제: <b>이지화</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 반동형성, 
      y = ~ Name, 
      marker = list(color = '#B4CCC1'),
      hovertemplate = paste(
        '적응기제: <b>반동형성</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 억압, 
      y = ~ Name, 
      marker = list(color = '#CBDCD4'),
      hovertemplate = paste(
        '적응기제: <b>억압</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 이타주의, 
      y = ~ Name, 
      marker = list(color = '#5C9ED7'),
      hovertemplate = paste(
        '적응기제: <b>이타주의</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 예상, 
      y = ~ Name, 
      marker = list(color = '#7BB1DF'),
      hovertemplate = paste(
        '적응기제: <b>예상</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 유머, 
      y = ~ Name, 
      marker = list(color = '#9BC3E7'),
      hovertemplate = paste(
        '적응기제: <b>유머</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 승화, 
      y = ~ Name, 
      marker = list(color = '#BAD6EE'),
      hovertemplate = paste(
        '적응기제: <b>승화</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    add_trace(
      x = ~ 억제, 
      y = ~ Name, 
      marker = list(color = '#DAE9F6'),
      hovertemplate = paste(
        '적응기제: <b>억제</b><br>',
        '비율: <b>%{x: .1f}%</b><extra></extra>'
      )) %>% 
    
    layout(xaxis = 
             list(title = '',
                  showgrid = F,
                  showline = F,
                  showticklabels = F,
                  zeroline = F,
                  domain = c(0.15, 1)),
           yaxis = 
             list(title = '',
                  showgrid = F,
                  showline = F,
                  showticklabels = F,
                  zeroline = F),
           barmode = 'stack',
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = 0, r = 0, t = 50, b = 50),
           showlegend = F) %>%
    
    #y축 이름
    add_annotations(
      xref = 'paper', yref = 'y', x = 0.14, y = am_y,
      xanchor = 'right',
      text = am_y,
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE, align = 'right') %>% 
    
    #퍼센트 
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out / 2, y =  am_y,
      text = paste('행동화<br>', am_tib[,'행동화'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc / 2, y =  am_y,
      text = paste('해리<br>', am_tib[,'해리'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta / 2, y =  am_y,
      text = paste('공상<br>', am_tib[,'공상'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg / 2, y =  am_y,
      text = paste('수동공격<br>', am_tib[,'수동공격'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      ref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti / 2, y =  am_y,
      text = paste('투사<br>', am_tib[,'투사'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat / 2, 
      y =  am_y,
      text = paste('신체화<br>', am_tib[,'신체화'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa / 2, y =  am_y,
      text = paste('전위<br>', am_tib[,'전위'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel / 2, y =  am_y,
      text = paste('이지화<br>', am_tib[,'이지화'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for / 2, y =  am_y,
      text = paste('반동형성<br>', am_tib[,'반동형성'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres / 2, y =  am_y,
      text = paste('억압<br>', am_tib[,'억압'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres + altr / 2, y =  am_y,
      text = paste('이타주의<br>', am_tib[,'이타주의'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres + altr + antic / 2, y =  am_y,
      text = paste('예상<br>', am_tib[,'예상'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres + altr + antic 
      + humor / 2, y =  am_y,
      text = paste('유머<br>', am_tib[,'유머'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres + altr + antic 
      + humor + sublim / 2, y =  am_y,
      text = paste('승화<br>', am_tib[,'승화'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>% 
    
    add_annotations(
      xref = 'x', yref = 'y',
      x = act_out + dissoc + fanta + pass_agg + projecti + somatizat 
      + displa + intel + reac_for + repres + altr + antic 
      + humor + sublim + suppr / 2, y =  am_y,
      text = paste('억제<br>', am_tib[,'억제'], '%'),
      font = list(family = 'Arial', size = 8, color = '#000000'),
      showarrow = FALSE) %>%
    
  config(displayModeBar = FALSE)
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


# 우울(CESD_R)
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

# -----------
# 자기상
# -----------

# CI 결과물 주소
{
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
}

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
