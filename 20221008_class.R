# 2022.10.08 실습


## 심화

strs <- c("비비디바비디부 BiBiDi", "케세라세라", "15881234", "똑딱 똑딱 똑똑 딱딱", "묻고\n답하기",
          "Hello, 뿜빠라뿜빰뿌뿌뿌", "빠라라라라람바 빠라라라라람바", "지국총 지국총 어사와", 
          "아흐다롱디리, 으허허", "63빌딩", "파크원빌딩 현대백화점 6층...", "롯데백화점빌딩 건너편 starbuks",
          "브루펜", "부루펜", "브루팬", "부르팬")

pattern <- "현대|롯데"

grep(pattern, strs, value="TRUE")
grep(pattern, strs)

pattern2 <- "브|부"
pattern3 <- "^브|^부"
grep(pattern2, strs, value="TRUE")
grep(pattern3, strs, value="TRUE")

pattern4 <- "(지국총[[:space:]]*){2,}"
grep(pattern4, strs, value = TRUE)

pattern5 <- "라..."
grep(pattern5, strs, value = TRUE)

pattern6 <- "라{4}"
grep(pattern6, strs, value = TRUE)


## stringr

library(stringr)
ls(pos = "package:stringr", pattern = "^str_") %>% 
  str_subset("<", negate = TRUE)



##

library(tidyverse)
library(rvest)

course <- "https://choonghyunryu.github.io/NLP_TA/"
html <- rvest::read_html(course, encoding = "UTF-8")

# 문장 추출
str_docs <- html %>%
  html_elements("p") %>%
  html_text()

# 문장 조회
str_docs

#내용 없는 문장 제거
str_docs <- str_docs[str_detect(str_docs, "^$", negate = TRUE)]  
str_docs

# 개행문자 제거
str_docs <- str_squish(str_docs)
str_docs


#테이블 텍스트 수집하기

tabs <- html %>%
  html_elements("table")
tabs

# 평가 계획 테이블 추출하기
tab_evaluate <- tabs[[1]] %>% 
  html_table()
tab_evaluate

