# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
library(tidyverse)
library(htmlwidgets)
library(lubridate)
Sys.setlocale("LC_TIME", "C")

company_list <- list("apple", "microsoft", "amazon", "tesla", "alphabet", "google", "meta", "facebook", "nvidia", "pepsi", "costco", "broadcom", "cisco", "adobe", "comcast", "intel", "qualcomm", "texas instruments", "t-mobile", "advanced micro devices", "honeywell")

esg_list <- list('esg', 'sustainability', 'csr')

# 기업별로 
for (i in 1:length(company_list[1])) {
  
  # esg 키워드별로
  for (j in 1:length(esg_list)) {
    print("j: ", j)
    #print(j)
    # 첫페이지에서 기사 총 개수, 페이지수 등을 구한다
    html1 <- paste("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[j]), "&page=",as.character(1),"&sort=date", sep="")
    f_times1 <- read_html(html1)
    
    # 기사 총 개수와 페이지 수 구하기
    article_total <- f_times1 %>% 
      html_node(".search-results__heading-title") %>% 
      html_text()
    article_total
    article_total <- strsplit(article_total, split= " ") 
    
    cur_start <- regexpr('???', article_total[[1]][3])
    
    # 현재 아티클 개수 
    current_num <- substr(article_total[[1]][3], cur_start[1]+1, nchar(article_total[[1]][3]))
    current_num <- as.numeric(current_num)
    current_num
    # 전체 아티클 개수
    total_start <- regexpr('P', article_total[[1]][5])
    total_num <- substr(article_total[[1]][5], 1, total_start[1]-1)
    #total_num <- article_total[[1]][5]
    total_num <- as.numeric(total_num)
    total_num
    
    # 페이지 수
    total_pg <- total_num %/% 25 + 1
    total_pg
    #current_pg <- current_num %/% 25
    #current_pg
    
    # 빈 df 만들어놓기
    df <- data.frame()

    current_pg <- 1
    
    # 검색 시 페이지별로
    #for (k in range(20)) {
    if (is.na(current_pg)) {
      print('No!!,,,')  
    } elif (current_pg <= total_pg) {
    #if (current_pg <= total_pg) {
      print('current_pg: ')
      print(current_pg)
      #current_pg <- 1
      hhtml <- paste("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[3]), "&page=",as.character(current_pg),"&sort=date", sep="")
      f_times <- read_html(hhtml)
      
      current_pg <- current_pg + 1
      ####### 컨텐츠
      # 제목
      titles <- f_times %>% 
        html_nodes(".js-teaser-heading-link") %>% 
        html_text()
      titles
      
      # 너무 긴 제목은 제거
      #for (e in seq(length(titles))) {
      #  if (!is.na(e)) {
      #    print('Not again,,,')
      #  } while (nchar(titles[e])>161) {
      #    titles <- titles[-e]
      #  }
      #}

      # 내용
      texts <- f_times %>% 
        html_nodes(".js-teaser-standfirst-link") %>% 
        html_text()
      texts
        
      # 날짜
      dates <- f_times %>% 
        html_nodes(".o-teaser__timestamp-date") %>% 
        html_text()
      dates
      
      # 날짜 March -> 3으로 변환
      date_list <- c()
      
      for (m in dates) {
        new_dates <- mdy(m)
        date_list <- c(date_list, as.character(new_dates))
      }
      
      # 데이터프레임화 및 csv 저장
      plus_df <- data.frame(Title=titles, Text=texts, Date=date_list)
      #plus_df <- data.frame(Title=titles, Date=date_list)
      df <- rbind(df, plus_df)
      print('dfdf')
    }
    
    strn <- paste("C:\\Users\\user\\Documents\\kaist_r_esg_ft\\", as.character(i), "_", company_list[i], "_", esg_list[j], ".csv", sep="")
    write_csv(df, file=strn)
    print('df ready')
  }
}


titles
texts

date_list

esg_list[3]
# 기업별 변경
#n <- n + 1

# 애플로 페이지수변경, 전체 df 잘 출력되는지 확인
# 그 다음 전체 기업, esg+들로


#empty_df <- tibble()
#dfff <- rbind(empty_df, df)
#dfff

length(titles)

titles[1]
nchar(titles[15])

for (e in seq(length(titles))) {
  if (nchar(titles[e])>160) {
    titles <- titles[-e]
  }
}
length(titles)
length(texts)
length(date_list)
total_pg
total_num
current_pg


#j부분 이상
#sustain만 나온다

#두번째로 appl+sustain이 486인데 크롤링이 다 되지가 않는다

# NA/오류 개빡친다
# https://greedhead.net/how-to-solve-missing-value-where-true-false-needed-in-r/
# All you need to do embed your “if statement” or “while statement” in another “if statement” that puts the value through the is.na() function to see if its value is “NA” or not. This will allow you to avoid this error message, as illustrated below.

# 데이터 저장 시 총 기사수 등도 저장하기


# 부탁?할거
# 기업 리스트, 영어 소문자로 통일
# esg 키워드
# 기업은 업종별로 최대 10-20개
# 데이터는 총 만개 미만
# 그러니까 기업 50*200개 데이터만으로도 충분하지 않나


