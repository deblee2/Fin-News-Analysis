Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

file.edit('~/.Renviron')

# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
library(tidyverse)
library(htmlwidgets)
library(lubridate)
Sys.setlocale("LC_TIME", "C")
fileEncoding = "UTF-8"

company_list <- list("tesla", "volkswagen")

#esg list data download
esg_data <- read.csv(file="C:\\Users\\user\\Documents\\kaist_r_esg_0511\\[Dictionary] ESG Dictionary - Sheet1.csv", header=T)

#esg_data[[1]] <- esg_data[[1]][c(1:17)]
#esg_data[[3]] <- esg_data[[3]][c(1:17)]
#colnames(esg_data)[1]

esg_list <- list(esg_data)
esg_list <- esg_list[[1]]
esg_list[[1]]
length(esg_list[[1]])
#esg_list[[1]]
#esg_list <- esg_list[[1]][-17]

length(esg_list[[3]][15])


# 기업별로 seq() 
#for (i in 1:length(company_list[1])) {
for (i in 1:2) {
  # esg class별로
  for (k in 1:3) {
    for (j in 1:length(esg_list[[k]])) {
      if (is.na(esg_list[[k]][j])) {
        next
      }
      print("j: ")
      print(j)
      #print(j)
      # 첫페이지에서 기사 총 개수, 페이지수 등을 구한다
      html1 <- paste0("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[[k]][j]), "&page=",as.character(1),"&sort=date")
      print(html1)
      f_times1 <- read_html(html1)
      
      # 기사 총 개수와 페이지 수 구하기
      article_total <- f_times1 %>% 
        html_node(".search-results__heading-title") %>% 
        html_text()
      article_total
      article_total <- strsplit(article_total, split= " ") 
      
      cur_start <- regexpr('-', article_total[[1]][3])
      #print('cur_start')
      #print(cur_start)
      
      # 현재 아티클 개수 
      current_num <- substr(article_total[[1]][3], cur_start[1]+1, nchar(article_total[[1]][3]))
      #current_num <- as.numeric(current_num)
      print('current_num')
      print(current_num)
      # 전체 아티클 개수
      total_start <- regexpr('P', article_total[[1]][5])
      total_num <- substr(article_total[[1]][5], 1, total_start[1]-1)
      
      total_num <- as.numeric(total_num)
      print('total_start')
      print(total_start)
      print('total_num')
      print(total_num)
      
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
          hhtml <- paste0("https://www.ft.com/search?q=", as.character(company_list[i]), "%2B", as.character(esg_list[[k]][j]), "&page=",as.character(current_pg),"&sort=date")
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
          text_list <- list(texts)
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
          
          if (length(text_list)<1) {
            print('Text is empty')
            text_list_log <- list(rep(c(0), length(titles)))
          } else {
            text_list_tog <- list(texts)
          }
          
          # company_list, esg_list 리스트 전환
          company_list_tog <- rep(company_list[i], length(date_list))
          esg_list_tog <- rep(esg_list[[k]][j], length(date_list))
          subclass_list_tog <- rep(colnames(esg_data)[j], length(date_list))
          
          # 데이터프레임화 및 csv 저장
          #plus_df <- data.frame(Company=unlist(company_list_tog), Esg=unlist(esg_list_tog), Subclass=unlist(subclass_list_tog), Title=titles, Text=unlist(text_list_log), Date=date_list)
          plus_df <- data.frame(Company=company_list_tog, Esg=esg_list_tog, Subclass=subclass_list_tog, Title=titles, Text=text_list_tog, Date=date_list)
          df <- bind_rows(df, plus_df)
          #print('dfdf')
        } 
        #else if (is.na(current_pg)) {
        #  print('The value is NA')
        #}
      }
    }
  }
  # esg subclass별로
  
}

strn <- paste("C:\\Users\\user\\Documents\\kaist_r_esg_0511\\", "esg_scraping", "_", as.character(i), ".csv", sep="")
write_csv(df, file=strn)
print('df ready')


length(list(company_list_tog))
length(list(esg_list_tog))


df






''
company_list_tog


#company_list[1]
#esg_list[1]
#date_list

#rep(company_list[1], each=length(date_list))

company_list_tog <- list(rep(company_list[i], length(date_list)))
unlist(company_list_tog)

length(titles)
titles

length(text_list_log)
unlist(text_list_log)

company_list
''

esg_list[[1]][4]
