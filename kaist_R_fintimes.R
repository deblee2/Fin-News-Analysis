# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
library(tidyverse)
library(htmlwidgets)
library(lubridate)
Sys.setlocale("LC_TIME", "C")
fileEncoding = "euc-kr"

company_list <- list("apple", "microsoft", "amazon", "tesla", "alphabet", "google","facebook", "nvidia", "pepsi", "costco", "broadcom", "cisco", "adobe", "comcast", "intel", "qualcomm", "texas instruments", "t-mobile", "advanced micro devices", "honeywell")

esg_list <- list('esg', 'sustainability', 'csr')

# 기업별로 seq() 
#for (i in 1:length(company_list[1])) {
for (i in c(7:10)) {
  #i <- 3
  # esg 키워드별로
  for (j in 1:length(esg_list)) {
    print("j: ")
    print(j)
    #print(j)
    # 첫페이지에서 기사 총 개수, 페이지수 등을 구한다
    html1 <- paste("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[j]), "&page=",as.character(1),"&sort=date", sep="")
    print(html1)
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
    print('current_num')
    print(current_num)
    # 전체 아티클 개수
    total_start <- regexpr('P', article_total[[1]][5])
    total_num <- substr(article_total[[1]][5], 1, total_start[1]-1)

    total_num <- as.numeric(total_num)
    print('total_start')
    print(total_start)
    # 페이지 수
    total_pg <- total_num %/% 25 + 1
    print('total_pg: ')
    print(total_pg)
    # 빈 df 만들어놓기
    df <- data.frame()

    current_pg <- 1
    
    # 검색 시 페이지별로
    #for (k in range(20)) {
    #if (is.na(current_pg)) {
    #  print('No!!,,,')  
    #} else if (current_pg <= total_pg) {
    if (!is.na(current_pg) & !is.na(total_pg)) {
      while (current_pg <= total_pg) {
        hhtml <- paste("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[j]), "&page=",as.character(current_pg),"&sort=date", sep="")
        f_times <- read_html(hhtml)
        
        current_pg <- current_pg + 1
        print('current_pg')
        print(current_pg)
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
        df <- rbind(df, plus_df)
        #print('dfdf')
      } 
      #else if (is.na(current_pg)) {
      #  print('The value is NA')
      #}
    }
    strn <- paste("C:\\Users\\user\\Documents\\kaist_r_esg_ft\\", as.character(i), "_", company_list[i], "_", esg_list[j], ".csv", sep="")
    write_csv(df, file=strn)
    print('df ready')
  }
}

