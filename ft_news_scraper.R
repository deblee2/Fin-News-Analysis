Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

#file.edit('~/.Renviron')

# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
library(tidyverse)
library(htmlwidgets)
library(lubridate)
library(tidyr)
Sys.setlocale("LC_TIME", "C")
fileEncoding = "UTF-8"

# 회사 리스트
company_list <- list("tesla", "volkswagen")

#ESG 딕셔너리
esg_data <- read.csv(file="C:\\Users\\user\\Documents\\[FOLDER]\\[Dictionary] ESG Dictionary - Sheet1.csv", header=T)

esg_list <- list(esg_data)
esg_list <- esg_list[[1]]

# 빈 df 만들어놓기
df <- data.frame()
title_list <- c()
text_list <- c()
date_list <- c()

#기업별
for (i in c(1:2)) {
  #esg class별로
  for (k in c(1:3)) {
    # subclass 별로
    for (j in c(1:length(esg_list[[3]]))) {
      print("j: ")
      print(j)

      html1 <- paste0("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[[k]][j]), "&page=",as.character(1),"&sort=date")
      print(html1)
      f_times1 <- read_html(html1)
      
      # 기사 총 개수와 페이지 수 구하기
      xpath_dir = paste('//*[@id="site-content"]/div/div[1]/div[2]/h2') 
      how_many <- html_node(f_times1, xpath=xpath_dir) %>%
        html_text()
      
      how_many <- str_replace(how_many, "Viewing results ", "")
      how_many <- str_replace(how_many, "Powered By Algolia", "")
      how_many1 <- strsplit(how_many, " ")
      
      start_num <- gsub("‒.*", "", how_many1[[1]][1])
      
      end_of_pg_num <- gsub("*.‒", "", how_many[[1]][1])
      end_of_pg_num <- gsub("\\s{1}.*", "", end_of_pg_num)
      
      total_num <- how_many1[[1]][3]
      
      # 페이지 수
      total_pg <- as.numeric(total_num) %/% 25 + 1
      print('total_pg: ')
      print(total_pg)

      current_pg <- 1

      
      if (!is.na(current_pg) & !is.na(total_pg)) {
        # 검색 시 페이지별로
        while (current_pg <= total_pg) {
          title_list <- c()
          text_list <- c()
          date_list <- c()
          hhtml <- paste0("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[[k]][j]), "&page=",as.character(current_pg),"&sort=date")
          f_times <- read_html(hhtml)
          
          current_pg <- current_pg + 1
          
          # 한 페이지에서 하나의 기사씩 크롤링(한 번에 다 크롤링하면 오류 처리를 할 수가 없음)
          art_num = as.numeric(end_of_pg_num) - as.numeric(start_num) + 1 
          
          for (m in c(1:art_num)) {
            # 본문
            text_dir = paste0('//*[@id="site-content"]/div/ul/li[',as.character(m),']/div/div/div/div[1]/p', sep="")
            texts <- f_times %>%
              html_node(xpath=text_dir) %>%
              html_text()
            text_list <- c(text_list, texts)
        
            # 본문 존재하지 않으면 SKIP
            if (exists("texts")) {
              # 제목
              title_dir = paste('//*[@id="site-content"]/div/ul/li[', as.character(m), ']/div/div/div/div[1]/div[2]', sep="") 
              titles <- f_times %>%
                html_node(xpath=title_dir) %>%
                html_text()
              title_list <- c(title_list, titles)
            
            # 날짜
              date_dir <- paste('//*[@id="site-content"]/div/ul/li[',as.character(m),']/div/div/div/div[1]/div[3]', sep="")
              dates <- f_times %>%
                html_node(xpath=date_dir) %>% 
                html_text()
              date_list <- c(date_list, as.character(dates))
              
              # 회사 이름, ESG 클래스, ESG 서브클래스 리스트화
              company_list_tog <- rep(company_list[i], length(date_list))
              subclass_list_tog <- rep(esg_list[[k]][j], length(date_list))
              esg_list_tog <- rep(colnames(esg_data)[k], length(date_list))
            }
          }
          # 데이터프레임화 및 병합
          plus_df <- data.frame(Company=unlist(company_list_tog), Esg=unlist(esg_list_tog), Subclass=unlist(subclass_list_tog), Title=title_list, Text=text_list, Date=date_list)
          df <- rbind(df, plus_df)
        } 
      }
    }
  }
}


# 날짜 March -> 3으로 변환
dff <- drop_na(df)
nrow(dff)
dff$Date <- mdy(dff$Date)

# csv 저장 
strn <- paste("C:\\Users\\user\\Documents\\[FOLDER]\\", company_list[1], "_e", ".csv", sep="")
write_csv(dff, file=strn)