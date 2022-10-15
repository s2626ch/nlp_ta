
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
