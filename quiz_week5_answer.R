##### 퀴즈 1 #####

library(chromote) # 크롬 브라우저를 R에서 원격 제어해주는 패키지(자바스크립트 렌더링 가능).
library(rvest)   
library(tidyverse)  

# 조선일보 기사 웹스크래핑 R 스크립트 다운받기.
url <- "https://github.com/cognitivepsychology/NLP_2025/raw/main/chosun_article_scraping.R"
download.file(url = url,
              destfile = "chosun_article_scraping.R",
              mode = "wb")

# 크롬 세션 시작하기(백그라운드에서 크롬 실행).
b <- ChromoteSession$new()

# 크롤링할 조선일보 기사 URL 지정하기.
url <- "https://www.chosun.com/culture-life/culture_general/2025/09/03/ECP5JDW2YZHNNFRXSVBVWXLR44/?utm_source=bigkinds&utm_medium=original&utm_campaign=news"

# 지정한 URL로 크롬 세션을 이동하기(브라우저에서 페이지 열기와 동일).
b$Page$navigate(url)

# 자바스크립트로 본문이 렌더링될 시간을 확보하기 위해 잠시 대기하기(인터넷이 빠른 분들은 2, 3 정도로 더 줄여도 돼요).
Sys.sleep(5)

# 현재 페이지의 DOM(Document Object Model) 트리(HTML 문서를 컴퓨터가 다루기 쉽게 계층구조[Tree]로 바꿔놓은 것)를 가져옴.
html <- b$DOM$getDocument()

# DOM 트리의 루트 노드(root node: 트리의 가장 꼭대기에 있는 최상위 노드)를 기준으로 전체 HTML 소스를 추출하기.
content <- b$DOM$getOuterHTML(nodeId = html$root$nodeId) # getOuterHTML(): DOM 노드의 전체 HTML 코드를 문자열로 가져오는 함수. 
# nodeId: DOM 트리 안에서 각 노드(태그, 텍스트 등)를 하나 하나 구분하기 위해 부여되는 고유번호.

# 추출한 HTML 소스를 read_html()로 불러들여 R 객체로 변환하기. 여기부터는 rvest의 시간입니다!
page <- read_html(content$outerHTML)

# 기사본문이 들어 있는 <p> 태그들을 CSS 선택자로 지정하여 추출하기.
article <- page %>%
  html_elements("p.article-body__content.article-body__content-text") %>% # F12로 신문기사 본문이 들어 있음을 확인한 <p> 태그 내 클래스 넣기.
  html_text2()   # 텍스트만 깔끔하게 추출하기(공백/행갈이 제거).

# 여러 문단으로 추출된 기사본문을 띄어쓰기 단위로 하나의 문자열로 합치기.
article_text <- str_c(article, collapse = " ")

# 벡터 형식의 기사본문을 티블 형식으로 변환하기.
article_text.1 <- tibble(text = article_text)

# 조선일보 기사본문 텍스트 정규화하기.
chosun_rcpp <- article_text.1 %>% # 메타데이터 정보가 포함된 XML 텍스트 티블.
  mutate(text = enc2utf8(text)) %>% # R 내장함수인 enc2utf8을 통해 text 열의 텍스트를 UTF-8 형식으로 인코딩하기(그래야 한글이 깨지지 않음!). 
  unnest_tokens(morp, text, token = RcppMeCab::pos, to_lower = F) %>% # RcppMeCab 패키지의 pos 함수를 사용하여 토큰화. 이때 형태소 태깅이 대문자를 유지하도록 함.
  filter(!str_detect(morp, "J[A-Z]{1,3}|E[A-Z]{1,3}|S[A-Z]{1,3}")) %>%  # 조사([EX] JKS, JSO), 어미([EX] EF, EP), 기호([EX] SF, SP) 제외.
  separate(morp, sep = "/", into = c("morp_untagged", "pos")) # 형태소와 품사 태그를  "/"를 기준으로 분리하여 두 개의 칼럼 생성.
chosun_rcpp


##### 퀴즈 2 #####

library(rvest)   
library(tidyverse)  
library(bitNLP)

url <- "https://www.clien.net/service/board/use/19072031?type=recommend"
page <- read_html(url, encoding = "UTF-8")  # 인코딩 명시

# 댓글이 들어 있는 태그들을 CSS 선택자로 지정하여 추출하기.
comments <- page %>%
  html_nodes(".comment_view") %>%
  html_text2()

# 벡터 형식의 기사본문을 티블 형식으로 변환하기.
comment_text <- tibble(text = comments)

# morpho_mecab 함수의 type 논항 디폴트가 명사(noun)이므로, 형태소(morpheme) 단위로 토큰화하려면 자체 제작 함수를 만들어야 함.
morpho_mecab_1 <- function(text, type = "morpheme"){ 
  result <- morpho_mecab(text, type = type) # morpho_mecab 함수를 사용하여 주어진 텍스트를 형태소 단위로 토큰화.
  str_c(as.character(result), "/", names(result), collapse = " ") # 형태소/품사태그([EX] "학교/NNG")의 형태로 토큰화. 벡터를 구성하는 개별 형태소([EX] "학교")에 부여된 명칭이 품사 태그([EX] "NNG")이므로, 이를 토큰화에 이용할 수 있음.
}

# 클리앙 게시글 댓글 텍스트 정규화하기.
comment_mecab <- comment_text %>%
  mutate(text_1 = map_chr(text, ~ morpho_mecab_1(.x))) %>% # 각 원소별로 함수를 적용해 문자열을 한 개씩 만듦 → 그 결과 한 줄의 문자형 벡터를 생성함.
  mutate(text_1 = map_chr(text_1, ~ {
    # 제거하고자 하는 품사 태그(기호, 어미, 조사)를 정규표현식으로 정의하기.
    pat <- "/(S|E|J)[A-Z]{1,3}" 
    # 지정된 정규표현식에 해당하는 형태소만 str_remove_all 함수를 통해 제거하고 나머지 형태소만 취하기. str_remove_all()은 주어진 문자열에서 특정 패턴을 가진 부분을 제거하는 기능 수행.
    extracted_tags <- str_remove_all(.x, pattern = str_c("[^\\s]+", pat)) # "[^\\s]+"는 공백이 아닌 문자가 하나 이상 반복되는 것을 의미. 
    # 추출된 결과에서 품사 태그 제거.
    extracted_tags_removed <- str_remove_all(unlist(extracted_tags), "/[A-Z]{1,5}+|\\+[A-Z]{1,5}") # str_remove_all()은 결과를 리스트로 반환하므로, unlist 함수를 사용해 결과를 벡터로 변환해야 함.
    str_c(extracted_tags_removed, collapse = " ") 
  }))
comment_mecab
