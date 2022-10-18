poem_star <- c(
  "가슴 속에 하나 둘 새겨지는 별을",
  "이제 다 못 헤는 것은",
  "쉬이 아침이 오는 까닭이요,",
  "내일 밤이 남은 까닭이요,",
  "아직 나의 청춘이 다하지 않은 까닭입니다."
)

poem_star

grep("까닭", poem_star)

grep("까닭", poem_star, value = TRUE)

target_text <- "오랜 만에 찾은 회색공간(gray space)에서는 파리가 날리고 있었다.
회색을 나타내는 grey와 공간을 나타내는 space가 만나서 만들어진 회색공간이다."

gsub("grey", "gray", target_text)

gsub("[a-zA-z*]", "", target_text)

gsub("[[a-zA-Z]]*[[:space:]]*|[\\(\\)]*", "", target_text)