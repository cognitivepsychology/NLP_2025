# 필요한 패키지 불러오기
library(chromote)   # 크롬 브라우저를 R에서 원격 제어해주는 패키지(자바스크립트 렌더링 가능). 크롬만 깔려 있으면 윈도우/맥 사용자 모두 가능해요!
library(rvest)      # 웹 스크래핑용 패키지.
library(tidyverse)  # 문자열 처리용 하위 패키지(stringr) 사용.

# 설명: 조선일보 기사 페이지는 <p> 노드 안에 들어 있는 기사본문이 자바스크립트가 실행된 뒤에야 삽입되는 구조입니다. 이른바 동적 로딩이라고 하죠. 
# 설명: 자바스크립트가 개입된다? 무조건 동적 로딩! 동적 로딩으로 웹 페이지에서 텍스트가 제시되는 경우, rvest 패키지만으로는 스크래핑이 불가능합니다.
# 설명: 그래서 자바스크립트 렌더링 도구가 필요한데, 이때 chromote 패키지를 사용하면 <p>기사본문</p>들을 제대로 가져올 수 있습니다.

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

# 기사 본문이 들어 있는 <p> 태그들을 CSS 선택자로 지정하여 추출하기.
# F12를 눌러 확인하면 클래스 이름이 "article-body__content.article-body__content-text"인 <p> 태그 안에 본문기사가 들어 있음을 확인 가능.
article <- page %>%
  html_elements("p.article-body__content.article-body__content-text") %>% # F12로 신문기사 본문이 들어 있음을 확인한 <p> 태그 내 클래스 넣기.
  html_text2()   # 텍스트만 깔끔하게 추출하기(공백/행갈이 제거).

# 여러 문단으로 추출된 기사 본문을 하나의 문자열로 합치기.
# collapse="\n\n": 문단 사이에 빈 줄 하나씩 넣어 구분하기(그냥 보기 좋으라고..).
article_text <- str_c(article, collapse = "\n\n")

# 최종 결과 출력
cat(article_text)
