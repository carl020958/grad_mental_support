###21_1_graduate_support_self_understand

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('flexdashboard', 'highcharter', 'tidyverse', 'kableExtra')

# -----------
# 수검자 세팅
# -----------
# parID <- '김송정0003'
parName <- substr(parID, 1, nchar(parID)-4)

# -----------
# 디자인 세팅
# -----------
colSet <- c('#ff7c80','#ffcd2d','#f8cbad','#99ccff','#dae9f6','#99cc00')

# ------------
# 문항 꾸러미
# ------------
# 정신건강
mw <- c('mw_emotional', 'mw_psychological', 'mw_social', 'md')

# 자아상 
CI <- c('CI_DEP', 'CI_ANX', 'CI_ANG', 'CI_POSI', 'CI_ATTRACTIVE', 'CI_TRUST', 'CI_COMPETENCE', 'CI_WARM')


# # --------
# # data set
# # --------
# 
# {
#     d <-
#         googlesheets4::read_sheet(
#         'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#         sheet = 'final_num')
# 
#     long <- d %>%
#         pivot_longer(cesd_r:CI_WARM, names_to = 'cate', values_to = 'value') %>%
#         mutate(value = as.numeric(value)) %>%
#         group_by(cate) %>%
#         mutate(percentile = round(percent_rank(value)*100,1),
#                meanByCate = mean(value, na.rm=T),
#                medianByCate = median(value, na.rm=T)) %>%
#         ungroup()
# 
#     d_chr <-
#         googlesheets4::read_sheet(
#         'https://docs.google.com/spreadsheets/d/1EuH7sXnIxn1pNKCK9GPqJ5OuwaZ36rXQ-b834X56hlY/edit#gid=0',
#         sheet = 'final_chr')
# 
#     adaptation <-
#       googlesheets4::read_sheet(
#       'https://docs.google.com/spreadsheets/d/1Q-Tc8DbamCbtF87go_erc0EAnVwEg4vD1exg1Nq5c-o/edit#gid=1758163881',
#       sheet = '설문지 응답 시트1')
# 
# }



# ======
# Plots
# ======

# -------------------
# 적응기제 사전질문지
# -------------------

# 적응기제 사전질문지
{
adapation_DT <- 
  bind_cols(adaptation[, 1], adaptation[, 10:64]) %>% 
  filter(Name == parID) %>% 
  select(-Name) %>% 
  pivot_longer(everything(), names_to = '문항', values_to = '응답') %>% 
  knitr::kable(align = c('l', 'l')) %>% 
  kable_styling(bootstrap_options = c('condensed', 'responsive', 'hover', 'striped')) %>%
  kable_styling(fixed_thead = T) %>% 
  column_spec(1, width = '8cm') %>% 
  column_spec(2, bold = T) %>% 
  row_spec(0, color = 'white', background = '#990000')
  # scroll_box(width = '100%', height = '3000px')

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

# -------
# 자기상
# -------

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


