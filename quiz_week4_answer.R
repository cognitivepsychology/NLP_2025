##### 퀴즈 1 #####

# jsonlite 패키지 불러오기.
library(jsonlite)
library(tidyverse)

# json_week2 폴더 밑에 하위 폴더 published 만들기

dir.create("json_week2/quiz")

# JSON 파일 목록 불러오기.
json.list <- list.files(path = "json_week2/quiz", pattern = "^SDRW", recursive = T)
json.list.1 <- str_c("json_week2/quiz/", json.list)

# map 함수를 사용하여 목록 내 파일 일괄적으로 읽어들이기.
json.list.2 <- map(json.list.1, function(x) fromJSON(x, simplifyDataFrame = T))
json.list.2[[1]]

# 5개 JSON 파일에 대해 세 번째 리스트의 utterance, speaker, setting, topic 정보만 티블 형식으로 추출하도록 for 구문 작성.
json_text <- list()
json_metadata <- list()
json_setting <- list()
json_topic <- list()
for(i in seq_along(json.list.2)){
  json_text[[i]] <- map_dfr(json.list.2[[i]][[3]]$utterance, data.frame)
  json_metadata[[i]] <- map_dfr(json.list.2[[i]][[3]]$metadata$speaker, data.frame)
  json_setting[[i]] <- map_dfr(json.list.2[[i]][[3]]$metadata$setting, data.frame)
  json_topic[[i]] <- map_dfr(json.list.2[[i]][[3]]$metadata$topic, data.frame)
}

# 5개 구어 텍스트의 utterance와 그에 해당하는 speaker, setting, topic 메타데이터를 매칭하기.
json_join <- list()
for(i in seq_along(json.list.2)){
  json_join[[i]] <- json_metadata[[i]] %>%
    left_join(json_text[[i]], join_by(id == speaker_id)) %>% # utterance 정보 데이터프레임의 id와 speaker 정보 데이터프레임의 speaker_id를 연결고리로 하여 두 데이터프레임을 서로 합치기.
    bind_cols(json_setting[[i]], json_topic[[i]]) # setting 정보 데이터프레임과 topic 정보 데이터프레임까지 가로로 연결하여 합치기.
}

# 리스트 형식으로 추출된 결과물을 티블 형식으로 변환하기.
json_text.1 <- map_dfr(json_join, as_tibble) %>%
  rename(text = form, # form이라는 칼럼 제목을 text로 바꾸기.
         relation = `.x..i.....15`, # `.x..i.....15`라는 칼럼 제목을 relation으로 바꾸기.
         topic = `.x..i.....16`) %>% # `.x..i.....16`이라는 칼럼 제목을 topic으로 바꾸기.
  select(-id.y)
json_text.1


##### 퀴즈 2 #####

# xml2 패키지 불러오기.
library(xml2)
library(tidyverse)

# xml_week2 폴더 밑에 하위 폴더 quiz 만들기.
dir.create("xml_week2/quiz")

# xml 파일 목록 불러온 뒤 파일 경로 생성하기.
xml_list <- list.files(path = "xml_week2/quiz", pattern = "sjml$", recursive = T)
xml_list.1 <- str_c("xml_week2/quiz/", xml_list)

# 여러 개의 XML 파일을 한꺼번에 불러와 티블로 만들기.
quiz_xml_result <- map_dfr(xml_list.1, ~{
  tryCatch( # 연속작업 실행 중 오류가 생길 시 이를 건너뛰고 다음 작업으로 이행하도록 해줌.
    expr = { # 함수에 대한 기술.
      xml_sjml <- read_xml(.x)
      xml_sjml_tb <- tibble(
        title = xml_text(xml_find_all(xml_sjml, "//title")), 
        category = xml_text(xml_find_all(xml_sjml, "//category")),
        author_age = as.numeric(xml_attr(xml_find_all(xml_sjml, "//author"), "age")),
        author_occupation = xml_attr(xml_find_all(xml_sjml, "//author"), "occupation"),
        author_sex = xml_attr(xml_find_all(xml_sjml, "//author"), "sex"),
        author_submission = xml_attr(xml_find_all(xml_sjml, "//author"), "submission"),
        author_handwriting = xml_attr(xml_find_all(xml_sjml, "//author"), "handwriting"),
        text_date = xml_attr(xml_find_all(xml_sjml, "//text"), "date"), # text 노드의 data 속성의 결과값 칼럼 만들기.
        text_subclass = xml_attr(xml_find_all(xml_sjml, "//text"), "subclass"), # text 노드의 subcless 속성의 결과값 칼럼 만들기.
        text = xml_find_all(xml_sjml, "//text/p") %>% 
          xml_text() %>% 
          str_c(collapse = " ") # 행갈이 단위로 분절된 텍스트를 띄어쓰기 단위로 이어주기. 
      )
      message(str_c("성공: ", .x , "에서 텍스트를 추출함.")) # 성공 메시지 출력하기.
      
      # 성공 시 결과 티블을 반환함.
      return(xml_sjml_tb)
    },
    error = function(err) {
      message(str_c("실패: ", .x, "에서 오류가 발생하여 건너뜀.")) # 문제가 있는 xml 파일은 텍스트 추출을 건너뜀.
      
      # 오류 시 빈 티블을 반환함.
      return(tibble())
    }
  )
})

# 최종결과 저장하기.
save(quiz_xml_result, file="quiz_xml_result.rda")

# 최종결과 확인하기.
quiz_xml_result


##### 퀴즈 3 #####

library(rvest)
library(epubr)
library(tidyverse)

# 다운로드할 파일들이 들어 있는 URL을 url 객체에 할당하기.
main_url <- "https://www.gutenberg.org"

# URL에서 HTML 내용 읽어들이기.
main_page <- read_html(main_url)

# "a" 노드 중에서도 속성이 "href"인 링크만 추출하기.
book_links <- main_page %>%
  html_nodes("a") %>%
  html_attr("href") 

# 추출된 링크들 중에서 "/ebooks/" 뒤에 숫자가 붙는 링크만 필터링하기. 해당 링크만 실제 ebook 소개 페이지로 접속 가능.
book_links_tb <- as_tibble(book_links) %>%
  rename(link = value) %>%
  filter(str_detect(link, "^/ebooks/[0-9]{1,10}")) %>% # "/ebooks/" 뒤에 숫자(한 자리에서 열 자리까지)가 붙는 링크만 필터링하기.
  mutate(link = str_c("https://www.gutenberg.org", link)) %>% # 다운로드가 가능한 온전한 링크 주소 칼럼 생성하기.
  distinct() # 중복되는 행 제거하기.

# 최종결과 확인하기.
book_links_tb

# book_links_tb 티블의 link 칼럼 각 행을 처음부터 끝까지 돌며 find_epub_links() 함수를 적용한 뒤 그 결과를 데이터프레임 형식으로 저장
epub_download_url <- map_dfr(book_links_tb$link, find_epub_links)

library(rvest)
library(epubr)

# 다운로드할 파일들이 들어 있는 URL을 url 객체에 할당하기.
main_url <- "https://www.gutenberg.org"

# URL에서 HTML 내용 읽어들이기.
main_page <- read_html(main_url)

# "a" 노드 중에서도 속성이 "href"인 링크만 추출하기.
book_links <- main_page %>%
  html_nodes("a") %>%
  html_attr("href") 

# 추출된 링크들 중에서 "/ebooks/" 뒤에 숫자가 붙는 링크만 필터링하기. 해당 링크만 실제 ebook 소개 페이지로 접속 가능.
book_links_tb <- as_tibble(book_links) %>%
  rename(link = value) %>%
  filter(str_detect(link, "^/ebooks/[0-9]{1,10}")) %>% # "/ebooks/" 뒤에 숫자(한 자리에서 열 자리까지)가 붙는 링크만 필터링하기.
  mutate(link = str_c("https://www.gutenberg.org", link)) %>% # 다운로드가 가능한 온전한 링크 주소 칼럼 생성하기.
  distinct() # 중복되는 행 제거하기.

# ebook 상세 페이지의 epub 다운로드 링크 찾아내 티블 형식으로 저장하기.
find_epub_links <- function(link){
  links <- read_html(link) %>%
    html_nodes("a") %>%
    html_attr("href")
  
  epub_links <- as_tibble(links) %>%
    rename(link = value) %>%
    filter(str_detect(link, "ebooks/[0-9]{1,10}.epub.images$")) %>%# epub.images로 끝나는 링크만 필터링하기.
    mutate(link = str_c("https://www.gutenberg.org", link))
  epub_links
}

# book_links_tb의 link 칼럼 첫 행부터 끝 행까지 find_epub_links() 함수를 적용한 다음, 이를 데이터프레임 형식으로 저장하기.
epub_download_url <- map_dfr(book_links_tb$link, find_epub_links)

# ebook을 다운받아 저장할 폴더 만들기.
download_directory <- "epub_week2/quiz"
dir.create(download_directory)

# 다운로드 링크와 파일 경로를 포함하는 티블 생성하기.
downloads_df <- epub_download_url %>%
  mutate(file_name = basename(link) %>%
           str_remove("\\.images$"), # 링크의 맨 끝 부분만 가져오되, ".images" 부분은 제거하기.
         file_path = file.path(download_directory, file_name) 
  )

# tidyverse의 walk2 함수를 통해 개별 링크마다 파일명을 작성하여 epub_week2/quiz 폴더에 저장하기.
walk2(
  .x = downloads_df$link, 
  .y = downloads_df$file_path, 
  .f = ~ download.file(url = .x, destfile = .y, mode = "wb") # 주의! mode = "wb"(binary 형식)로 지정하지 않으면 파일이 열리지 않음!
)

# epubr 패키지의 epub 함수를 적용하여 epub 파일들로부터 텍스트 추출하기.
epub.files <- downloads_df$file_path # 파일 경로(파일명)만 추출하기.
epub.df <- map_dfr(epub.files, epub) # 파일 경로 벡터에 대해 epub 함수 적용하기.
epub.df$data


## 서명, 언어, 작가명, 주제 정보를 data와 매칭시키기.

# for 구문을 사용하여 data 칼럼 수정 작업 하기.
for (i in 1:nrow(epub.df[, c(4,5,8,9,10)])) { # epub.df 티블에서 title, language, creator, subject, data 정보가 포함된 4, 5, 8, 9, 10번 칼럼만 추출하기.
  title <- epub.df$title[i] # 현재 행의 title 값 가져오기.
  language <- epub.df$language[i] # 현재 행의 language 값 가져오기.
  creator <- epub.df$creator[i] # 현재 행의 creator 값 가져오기.
  subject <- epub.df$subject[i] # 현재 행의 subject 값 가져오기.
  
  data_tibble <- epub.df$data[[i]] # 현재 행의 data 칼럼 티블 가져오기.
  new_data_tibble <- data_tibble %>% 
    mutate(
      title = title, # data 티블에 새 칼럼 title 추가하기.
      language = language, # data 티블에 새 칼럼 language 추가하기.
      creator = creator, # data 티블에 새 칼럼 creator 추가하기.
      subject = subject
    ) %>%
    relocate(title, language, creator, subject) # title, language, creator, subject 칼럼이 맨 앞에 오도록 순서 조정하기.
  
  epub.df$data[[i]] <- new_data_tibble # 수정된 티블을 data 칼럼에 할당하기.
}

week4.quiz.data.tibble <- map_dfr(epub.df$data, as_tibble) %>% # 리스트 형식인 epub.df$data를 행 단위로 쌓은 티블 형식으로 변환하기.
  filter(str_detect(section, "cov|Cov|toc|TOC|pg-header|pg-footer") == F) # cover, 목차, pg-header, pg-footer 관련 정보는 제거하기.
save(week4.quiz.data.tibble, file="week4.quiz.data.tibble.rda") # 나중을 위해 객체 저장하기.
write_csv(week4.quiz.data.tibble, "week4.quiz.data.tibble.csv") # csv 파일 형태로 저장하기.

# 최종결과 확인하기.
week4.quiz.data.tibble %>%
  View()
