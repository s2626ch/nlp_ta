
# bitTA를 응용한 형태소 분석

library(bitTA)
args(morpho_mecab)

morpho_mecab("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.", type = "morpheme")
morpho_mecab("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.", type = "noun")

docs <- c("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
          "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다.")
morpho_mecab(docs,  type = "morpheme")
morpho_mecab(docs,  type = "morpheme", indiv = FALSE)
morpho_mecab(docs,  type = "noun", indiv = FALSE)
morpho_mecab(docs,  type = "noun2", indiv = FALSE)
morpho_mecab(docs, indiv = FALSE, type = "verb")

# 워드 클라우드

library(dplyr)


str(president_speech)

president_speech$doc[1:100] %>% 
  morpho_mecab(indiv = FALSE) %>% 
  table() %>% 
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

president_speech$doc[1:100] %>% 
  morpho_mecab(indiv = FALSE) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  .[-c(1:10)] %>% 
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

# 사용자 사전
str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
morpho_mecab(str)

dic_path <- system.file("dic", package = "bitTA")
dic_file <- glue::glue("{dic_path}/buzz_dic.dic")

morpho_mecab(str, user_dic = dic_file) ################## 오류. 질문할 것

# tidy verse

library(tidyverse)
available.packages(repos = "https://cran.rstudio.com/") %>% 
  row.names() %>% 
  str_subset("tidy")

library(bitTA)
library(tidyverse)
library(tidytext)

str(president_speech)
president_speech$doc[1:2]

nho_noun_indiv <- president_speech %>%
  filter(president %in% "노무현") %>%
  filter(str_detect(category, "^외교")) %>%
  tidytext::unnest_tokens(
    out = "speech_noun",
    input = "doc",
    token = morpho_mecab
  )

nho_noun_indiv 

president_speech %>%
  filter(president %in% "노무현") %>%
  filter(str_detect(category, "^외교")) %>%
  tidytext::unnest_tokens(
    out = "speech_noun",
    input = "doc",
    token = morpho_mecab,
    user_dic = user_dic
  )

tokenize_noun_ngrams(president_speech$doc[1:2])

str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
tokenize_noun_ngrams(str)

tokenize_noun_ngrams(str, stopwords = '구매') ################ 오류. 질문

# 감정 분석

library(bitTA)

polarity

args(get_opinion)

buzz

head(buzz$CONTENT)

# 디폴트
get_opinion(buzz$CONTENT[1]) 
# unigram
get_opinion(buzz$CONTENT[1], n = 1)
# bigram
get_opinion(buzz$CONTENT[1], n = 2)
# trigram
get_opinion(buzz$CONTENT[1], n = 3)

buzz$CONTENT[1:5] %>% 
  purrr::map_df(get_opinion)

buzz$CONTENT[5] %>% 
  stringr::str_wrap(100) %>% 
  cat()

get_opinion(buzz$CONTENT[1:5])

## 군산대학교 한국어감성사전 분석

sentiment_dic

get_polarity(buzz$CONTENT[1])

buzz$CONTENT[1:5] %>% 
  purrr::map_df(get_polarity)

get_polarity(buzz$CONTENT[1:5], indiv = FALSE)

get_polarity(buzz$CONTENT[1:50], n = 1)
get_polarity(buzz$CONTENT[1:50], n = 2)

library(tidyverse)
library(bitTA)
library(wordcloud2)
library(tidytext)

# 빈도계산
polarity_word_counts <- buzz %>% 
  filter(row_number() <= 200) %>% 
  mutate(morpho_content = get_spacing(CONTENT)) %>%   
  mutate(morpho_content = morpho_mecab(CONTENT, type = "morpheme") %>% 
           purrr::map_chr(paste, collapse = " ") %>% 
           stringr::str_remove_all("\"")) %>% 
  tidytext::unnest_ngrams(
    unigram,
    morpho_content,
    n = 1
  ) %>% 
  left_join(
    sentiment_dic,
    by = c("unigram" = "word")
  ) %>% 
  filter(!is.na(word_root)) %>% 
  filter(!str_length(unigram) == 1) %>% 
  mutate(polarity_class = ifelse(polarity > 0, "긍정", "부정")) %>% 
  count(unigram, polarity_class, sort = TRUE) 

polarity_word_counts

# 시각화
polarity_word_counts %>%
  filter(n > 2) %>%
  mutate(n = ifelse(polarity_class == "부정", -n, n)) %>%
  mutate(unigram = reorder(unigram, n)) %>%
  ggplot(aes(unigram, n, fill = polarity_class)) +
  geom_col() +
  coord_flip() +
  labs(y = "긍부정 극성의 발현 빈도")


# 워드클라우드
polarity_word_counts %>% 
  select(unigram, n) %>% 
  filter(row_number() > 3) %>%
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

polarity_word_counts %>% 
  reshape2::acast(unigram ~ polarity_class, value.var = "n", fill = 0) %>% 
  wordcloud::comparison.cloud(colors = c("blue", "red"), max.words = 150,
                              title.size = 2)

buzz


buzz %>% 
  group_by(SECTION) %>% 
  tally()

term_noun <- buzz %>% 
  filter(SECTION %in% "맘스홀릭") %>% 
  filter(row_number() == 1) %>% 
  select(CONTENT) %>% 
  pull() %>% 
  morpho_mecab()
term_noun

term_noun %>% 
  table() 

# 품사 토큰화
term_morpheme <- buzz %>% 
  filter(SECTION %in% "맘스홀릭") %>% 
  filter(row_number() == 1) %>% 
  select(CONTENT) %>% 
  pull() %>% 
  morpho_mecab(type = "morpheme")

term_morpheme

term_noun %>% 
  table() %>% 
  wordcloud2::wordcloud2(fontFamily = "NanumSquare")

dic_stopwords <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "년", "월", "일", "때", "전")

is_stopwords <- term_noun %in% dic_stopwords
is_stopwords

clean_term_noun <- term_noun[!is_stopwords]
clean_term_noun

clean_term_noun %>% 
  table() %>% 
  wordcloud2(fontFamily = "NanumSquare")

buzz %>% 
  filter(SECTION %in% "맘스홀릭") %>% 
  filter(row_number() == 1) %>% 
  select(CONTENT) %>% 
  get_polarity()

