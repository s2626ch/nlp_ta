# 정제를 위한 메타 조건 설정

meta_path <- system.file("meta", package = "bitTA")
fname <- glue::glue("{meta_path}/preparation_filter.csv")

## 데이터 필터링 메타 신규 등록
set_meta("filter", fname, fileEncoding = "utf8")

get_meta("filter")

doc_content <- buzz$CONTENT
is.character(doc_content)

length(doc_content)

sum(is.na(doc_content))

doc_after_character <- filter_text(doc_content) # 오류

library(dplyr)

buzz %>% 
  filter(filter_text(CONTENT, verbos = FALSE)) %>%   오류
  select(KEYWORD, SRC, CONTENT)

# replace_text()를 이용한 텍스트 대체
  
  meta_path <- system.file("meta", package = "bitTA")
  fname <- glue::glue("{meta_path}/preparation_replace.csv")
  set_meta("replace", fname, fileEncoding = "utf8")
  
  # 등록된 문자열 대체 룰 확인하기
  get_meta("replace")
  
  
  doc_content <- buzz$CONTENT
  
  stringr::str_detect(doc_content, "남편") %>% 
    sum(na.rm = TRUE)
  
  stringr::str_detect(doc_content, "신랑") %>% 
    sum(na.rm = TRUE)

  buzz_after <- buzz %>% 
    mutate(CONTENT = replace_text(CONTENT, verbos = TRUE))  # 오류
  
  stringr::str_detect(buzz_after$CONTENT, "남편") %>% 
    sum(na.rm = TRUE)
  
  
  ##############################################################################
  # 실습
  ##############################################################################
  
  head(buzz$CONTENT)
  
  ## 필터링 패턴 정의
  pathterns_filter <- c(
    "힐링애드",
    "베베쿡",
    "추천 아이디"
  )
  
  ## 패턴 매치 건수 확인
  pathterns_filter %>% 
    map_int(
      function(x) {
        str_detect(buzz$CONTENT, x) %>% 
          sum(na.rm = TRUE)
      }
    )
  
  ## buzz를 대상으로 제거해서 buzz_filter에 할당
  buzz_filter <- buzz %>% 
    filter(str_detect(CONTENT, pathterns_filter[1], negate = TRUE))
  
  ## buzz_filter를 대상으로 제거해서 buzz_filter에 할당
  buzz_filter <- buzz_filter %>% 
    filter(str_detect(CONTENT, pathterns_filter[2], negate = TRUE))
  
  ## buzz_filter를 대상으로 제거해서 buzz_filter에 할당
  buzz_filter <- buzz_filter %>% 
    filter(str_detect(CONTENT, pathterns_filter[3], negate = TRUE))
  
  pathterns_filter %>% 
    map_int(
      function(x) {
        str_detect(buzz_filter$CONTENT, x) %>% 
          sum(na.rm = TRUE)
      }
    )
  ## 3.2
  ## 필터링 전 데이터셋에서 패턴 매치된 데이터 건수
  pathterns_filter %>% 
    map_int(
      function(x) {
        str_detect(buzz$CONTENT, x) %>% 
          sum(na.rm = TRUE)
      }
    )  
  
  NROW(buzz)
  NROW(buzz_filter)  
  