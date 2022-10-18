
# bitTA를 응용한 형태소 분석

library(bitTA)
args(morpho_mecab)
morpho_mecab("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.", type = "morpheme")

docs <- c("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
          "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다.")
morpho_mecab(docs,  type = "morpheme")
morpho_mecab(docs,  type = "morpheme", indiv = FALSE)


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


library(bitTA)


# 감정 분석
args(get_opinion)
buzz
head(buzz$CONTENT)
get_opinion(buzz$CONTENT[1])
get_opinion(buzz$CONTENT[1], n = 1)
get_opinion(buzz$CONTENT[1], n = 2)
get_opinion(buzz$CONTENT[1], n = 3)

## 군산대학교 한국어감성사전 분석
get_polarity(buzz$CONTENT[1])

library(tidyverse)
library(bitTA)
library(wordcloud2)
library(tidytext)


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

polarity_word_counts %>%
  filter(n > 2) %>%
  mutate(n = ifelse(polarity_class == "부정", -n, n)) %>%
  mutate(unigram = reorder(unigram, n)) %>%
  ggplot(aes(unigram, n, fill = polarity_class)) +
  geom_col() +
  coord_flip() +
  labs(y = "긍부정 극성의 발현 빈도")




buzz %>% 
  group_by(SECTION) %>% 
  tally()

term_noun <- buzz %>% 
  filter(SECTION %in% "맘스홀릭") %>% 
  filter(row_number() == 1) %>% 
  select(CONTENT) %>% 
  pull() %>% 
  morpho_mecab()


term_noun %>% 
  table() 

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

