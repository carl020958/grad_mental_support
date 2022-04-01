###21_1_graduate_support_self_understand
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load('flexdashboard', 'highcharter', 'tidyverse')

# -------------
# 수검자 세팅
# -------------
# parID <- '김송정0003'
parName <- substr(parID, 1, nchar(parID)-4)

# ------------
# 디자인 세팅
# ------------
colSet <- c('#ff7c80','#ffcd2d','#f8cbad','#99ccff','#dae9f6','#99cc00')

# -------------
# 문항 꾸러미
# -------------
# 성격의 자화상
personality_portrit <- c('안전형_Bright',	'안전형_Dark',	'수도형_Bright',	'수도형_Dark',	'개성형_Bright',	'개성형_Dark',	'모험형_Bright',	'모험형_Dark',	'열정형_Bright',	'열정형_Dark',	'사교형_Bright',	'사교형_Dark',	'스타형_Bright',	'스타형_Dark',	'모범형_Bright',	'모범형_Dark',	'가족형_Bright',	'가족형_Dark',	'지지형_Bright',	'지지형_Dark')

# 정신건강
mw <- c('mw_emotional', 'mw_psychological', 'mw_social', 'md')

# 자아상 
CI <- c('CI_DEP', 'CI_ANX', 'CI_ANG', 'CI_POSI', 'CI_ATTRACTIVE', 'CI_TRUST', 'CI_COMPETENCE', 'CI_WARM')


# ---------
# data set
# ---------
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
# }



# ------
# plots
# ------

# 성격의 자화상
{
    personality_portrit_df <- long %>%
        filter(Name == parID & cate %in% personality_portrit) %>% 
        mutate(cate = str_replace(cate, '_', ''))
    
    pp_strength_df <- personality_portrit_df %>% 
        filter(grepl('Bright', cate)) %>% 
        mutate(cate = str_replace(cate, 'Bright', '')) 
    
    pp_weakness_df <-   personality_portrit_df %>% 
        filter(grepl('Dark', cate)) %>% 
        mutate(cate = str_replace(cate, 'Dark', '')) %>% 
        mutate(percentile = percentile*-1)
    
    ppPlot <- highchart() %>%
        hc_chart(type= 'bar') %>%
        hc_title(text= '') %>%
        hc_subtitle(text= '') %>%
        
        hc_xAxis(
            
            list(categories=pp_strength_df$cate,
                 reversed=FALSE,
                 labels=list(step= 1)),
            
            list(categories= pp_weakness_df$cate,
                 opposite= TRUE,
                 reversed= FALSE,
                 linkedTo= 0,
                 labels=list(step= 1))
        )%>%
        
        hc_tooltip(
            shared = FALSE,
            formatter = JS("function () {
                   return this.point.category + '<br/>' +
                   '<b>' + this.series.name + '</b> ' +
                   '<b>' + ' 백분위:' + '</b> ' +
                   Highcharts.numberFormat(Math.abs(this.point.y), 1) + '%';}")
        )%>%
        
        hc_yAxis(title= list(text= ''),
                 crosshair = T,
                 endonTick = T,
                 tickInterval = 10,
                 min = -100,
                 max = 100,
                 labels=list(formatter=JS("function () {
               return Math.abs(this.value) + '%';
             }"))
        )%>%
        
        
        hc_colors(color = c(colSet[1], colSet[4]
        )) %>%
        
        
        hc_plotOptions(column = list(
            colorByPoint = T)
        )  %>%
        
        hc_plotOptions(series=list(stacking= 'normal')
        )%>%
        
        
        hc_series(
            list(name= 'Dark Side',
                 data= pp_weakness_df$percentile),
            
            list(name= 'Bright Side',
                 data= pp_strength_df$percentile
            ))
}

### 정신건강
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


### 우울(CESD_R)
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

### CI 결과물 주소
### 자기상
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

### 자아상 8정서
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


