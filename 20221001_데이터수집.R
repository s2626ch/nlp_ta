
search_naver <- function(query = NULL, chunk = 100, chunk_no = 1,
                         sort = c("date", "sim"), do_done = FALSE,
                         max_record = 1000L, client_id = NULL,
                         client_secret = NULL, verbose = TRUE) {
  if (is.null(query)) {
    stop("검색 키워드인 query를 입력하지 않았습니다.")
  }
  
  if (chunk < 1 & chunk > 100) {
    stop("chunk 요청 변수값이 허용 범위(1~100)인지 확인해 보세요.")
  }
  
  if (chunk_no < 1 & chunk_no > 100) {
    stop("chunk_no 요청 변수값이 허용 범위(1~1000)인지 확인해 보세요.")
  }
  
  sort <- match.arg(sort)
  
  get_list <- function(doc) {
    doc %>%
      XML::getNodeSet("//item") %>%
      XML::xmlToDataFrame() %>%
      rename("publish_date" = pubDate) %>%
      mutate(publish_date = as.POSIXct(publish_date,
                                       format = "%a, %d %b %Y %H:%M:%S %z")) %>%
      mutate(title_text = stringr::str_remove_all(
        title, "&\\w+;|<[[:punct:]]*b>")) %>%
      mutate(title_text = stringr::str_remove_all(
        title_text, "[[:punct:]]*")) %>%
      mutate(description_text = stringr::str_remove_all(
        description,
        "&\\w+;|<[[:punct:]]*b>|[“”]"))
  }
  
  searchUrl <- "https://openapi.naver.com/v1/search/news.xml"
  
  query <- query %>%
    enc2utf8() %>%
    URLencode()
  
  url <- glue::glue("{searchUrl}?query={query}&display={chunk}&start={chunk_no}&sort={sort}")
  
  doc <- url %>%
    httr::GET(
      httr::add_headers(
        "X-Naver-Client-Id"     = client_id,
        "X-Naver-Client-Secret" = client_secret
      )
    ) %>%
    toString() %>%
    XML::xmlParse()
  
  total_count <- doc %>%
    XML::getNodeSet("//total") %>%
    XML::xmlValue() %>%
    as.integer()
  
  if (verbose) {
    glue::glue("* 검색된 총 기사 건수는 {total_count}건입니다.\n\n") %>%
      cat()
    
    glue::glue("  - ({chunk}/{min(total_count, max_record)})건 호출을 진행합니다.\n\n") %>%
      cat()
  }
  
  search_list <- doc %>%
    get_list()
  
  records <- NROW(search_list)
  
  if (!do_done | records >= total_count | records >= max_record) {
    return(search_list)
  } else {
    total_count <- min(total_count, max_record)
    
    cnt <- total_count %/% chunk
    
    if (total_count %% chunk == 0) {
      cnt <- cnt - 1
    }
    
    idx <- (seq(cnt) + 1)
    
    add_list <- idx[idx <= 1000] %>%
      purrr::map_df({
        function(x) {
          if (verbose) {
            glue::glue("  - ({chunk * x}/{total_count})건 호출을 진행합니다.\n\n") %>%
              cat()
          }
          
          glue::glue(
            "{searchUrl}?query={query}&display={chunk}&start={x}&sort={sort}"
          ) %>%
            httr::GET(
              httr::add_headers(
                "X-Naver-Client-Id"     = RUejkNX1Hh6g6fjZLWy8,
                "X-Naver-Client-Secret" = yRXMhui8FV
              )
            ) %>%
            toString() %>%
            XML::xmlParse() %>%
            get_list()
        }
      })
    
    search_list %>%
      bind_rows(
        add_list
      ) %>%
      return()
  }
}

library(dplyr)

client_id <- "RUejkNX1Hh6g6fjZLWy8"
client_secret <- "yRXMhui8FV"

search_list <- search_naver(
  "기록정보과학전문대학원", client_id = client_id, client_secret = client_secret
)

search_list <- search_naver(
  "\"기록정보과학전문대학원\"", client_id = client_id, client_secret = client_secret
)


dim(search_list)

head(search_list)

View(search_list)

# 실습

client_id <- "RUejkNX1Hh6g6fjZLWy8"
client_secret <- "yRXMhui8FV"

keyword <- "노원구 아파트 시세"

news_nowon_date <- search_naver(
  keyword, client_id = client_id, client_secret = client_secret
)

dim(news_nowon_date)
head(news_nowon_date)

news_nowon_sim <- search_naver(
  keyword, client_id = client_id, client_secret = client_secret, sort = "sim",

)

agg_news <- news_nowon_sim %>% 
  mutate(publish_date2 = lubridate::date(publish_date)) %>% 
  group_by(publish_date2) %>% 
  tally() 

buzz <- agg_news %>% 
  filter(n >= 90)
buzz

