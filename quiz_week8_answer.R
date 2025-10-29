## 퀴즈 1 ##

# 1. 패키지 불러오기
library(tidyverse)
library(reticulate) # kiwipiepy 사용을 위해
library(kiwipiepy)  # kiwipiepy 사용을 위해
library(quanteda)   # DFM 생성을 위해

# 2. 데이터 불러오기
# (가정) "clien_posts.rda" 파일에 clien_df 객체가 저장되어 있음
load("clien_posts.rda")

# 3. kiwipiepy 형태소 분석 함수 정의

library(reticulate) # 가상환경 생성 패키지 로드하기.

use_condaenv("my_env") # 가상환경 불러오기.

# kiwipiepy 라이브러리에서 'Kiwi'라는 분석기 클래스를 불러와 초기화하기
kiwi <- import("kiwipiepy")$Kiwi()

ibrary(reticulate) # 가상환경 생성 패키지 로드하기.

use_condaenv("my_env") # 가상환경 불러오기.

# kiwipiepy 라이브러리에서 'Kiwi'라는 분석기 클래스를 불러와 초기화하기
kiwi <- import("kiwipiepy")$Kiwi()

# kiwipiepy의 analyze() 분석기로 문서별 명사 형태소 분석 함수 만들기.
analyze_nng_kiwi <- function(text) {
  # kiwipiepy의 analyze() 함수로 분석하기.
  results <- kiwi$analyze(text)
  
  # purrr 패키지의 map_chr 함수를 사용해 파이썬 객체에서 형태소와 품사 태그 추출.
  # .x는 각 형태소 객체를 의미하며, '$form'과 '$tag'로 내용(문자열 벡터)을 가져옴.
  tagged_tokens <- map_dfr(results[[1]][[1]], ~tibble(
    morph = .x$form, # 형태소 추출.
    pos = .x$tag # 품사 추출.
  ))
  # 불용어 정의하기.
  stop_words_article <- c("게시판", "클리앙")
  filtered_tokens <- tagged_tokens %>%
    filter(str_detect(pos, "NNG|NNP")) %>% # 일반명사, 고유명사만 추출하기
    filter(!morph %in% stop_words_article)
  return(list(nng = filtered_tokens$morph)) # 필터링된 토큰(일반명사 & 고유명사) 리스트 만들기. 리스트 내 요소들에 대한 명명은 nng로!
}

# 4. 명사 추출 및 text_noun 칼럼 생성
clien_df_processed <- clien_df %>%
  mutate(pos_result = map(text, analyze_nng_kiwi))

clien_df_processed %>%
  pull(category) %>%
  unique()

# 5. 명사 리스트를 공백으로 연결된 텍스트로 변환
clien_df_processed <- clien_df_processed %>%
  mutate(text_noun = map_chr(pos_result, ~ str_c(.x$nng, collapse = " ")))

# 6. quanteda 코퍼스 객체 생성
# text_field는 명사만 추출한 text_noun을 사용
clien_corpus <- corpus(clien_df_processed, 
                       docid_field = "doc_id", 
                       text_field = "text_noun")

# 7. 토큰화(이미 명사만 추출했으므로 remove_punct 정도만)
clien_tokens <- tokens(clien_corpus, remove_punct = TRUE)

# 8. DFM(Document-Feature Matrix) 생성
clien_dfm <- dfm(clien_tokens)

# 9. DFM 객체 확인
clien_dfm

# 출력 예시
# Document-feature matrix of: 1,709 documents, 10,165 features (99.61% sparse) and 3 docvars.


## 퀴즈 2 ##

# (문항 1에서 clien_dfm이 생성되었다고 가정)

# 1. 패키지 불러오기
library(topicmodels) # LDA 모델링
library(broom)       # tidy() 함수
library(tidyverse)   # 시각화 패키지 ggoplot2 포함 
library(tidytext)    # reorder_within()
library(showtext)    # 한글 폰트 등록

# (사전 설정) 한글 폰트
font_add_google("Nanum Gothic", "NanumGothic")
showtext_auto()

# 2. LDA 모델 생성 (k=4)

clien_dfm <- clien_dfm[rowSums(clien_dfm) > 0, ] # 각 문서의 단어 수를 계산한 후 단어가 하나 이상 있는 문서만 남기기. dfm 객체에 빈 문서가 포함되어 있을 경우 LDA 분석 불가.

set.seed(1234) # 결과 재현을 위한 시드 설정
clien_lda <- LDA(clien_dfm, 
                 k = 4, 
                 method = "Gibbs", 
                 control = list(seed = 1234, 
                                burnin = 1000, 
                                iter = 3000, 
                                thin = 100))

# 3. 주제-단어 확률 (Beta) 추출
clien_topics_beta <- tidy(clien_lda, matrix = "beta")

# 4. 주제별 상위 10개 단어 추출
clien_top_terms <- clien_topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>% # beta 값 기준 상위 10개
  ungroup() %>%
  arrange(topic, -beta) # 주제별, beta 내림차순 정렬

# 5. ggplot2 시각화
clien_top_terms %>% 
  ggplot(aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free", ncol = 2) + # 2x2 격자
  coord_flip() +
  scale_x_reordered() + # x축 단어가 "치료__1"처럼 단어와 주제가 서로 붙은 채로 제시되지 않도록 해줌.
  labs(
    title = "Clien LDA (k=4) 주제별 상위 10개 단어 (Beta)",
    x = "단어 (Term)",
    y = "주제별 단어 확률 (Beta)"
  ) +
  theme(text = element_text(family = "NanumGothic")) # (폰트 재확인)

# 6. 해석 단계
# Topic 1: 충전, 배터리, 교체 => 아이폰? 전기차? 소모임
# Topic 2: 차, 차량, 주행, 엔진 => 자동차 소모임
# Topic 3: 투자, 수익, 주식, 회사, 계좌 => 주식 소모임
# Topic 4: 맥북, 맥, 애플 => 맥 소모임


## 퀴즈 3 ##

# (문항 2에서 clien_lda 모델이 생성되었다고 가정)

# 1. 패키지 불러오기
library(broom)
library(tidyverse)
library(showtext)

# (사전 설정) 한글 폰트
font_add_google("Nanum Gothic", "NanumGothic")
showtext_auto()

# 2. 베타(Beta) 값 tidy 객체로 추출
clien_topics_beta <- tidy(clien_lda, matrix = "beta")

# 3. wide form으로 변환 (topic1, topic2, topic3, topic4 칼럼 생성)
clien_topics_wide <- clien_topics_beta %>%
  pivot_wider(
    names_from = topic,
    values_from = beta,
    names_prefix = "topic",
    values_fill = 0 # 해당 주제에 단어가 없으면 0으로 채움
  )

# 4. 수강생이 직접 결과를 보고 모델이 추정한 주제가 무엇에 관한 것인지 확인하는 단계
# 문항 2의 결과, '휴대폰/전기차' = topic1, '주식' = topic3 이라고 가정

# 5. 최소 비중 필터링
clien_topics_filtered <- clien_topics_wide %>%
  filter(topic1 > .001 | topic3 > .001)

# 6. 로그비(log ratio) 계산
clien_log_ratio <- clien_topics_filtered %>%
  mutate(log_ratio = log2(topic1 / topic3)) 

# 7. '전기차' 소모임에서 두드러지는 단어 10개(로그비 상위)
top_ecar_terms <- clien_log_ratio %>%
  slice_max(log_ratio, n = 10, with_ties = FALSE)

# 8. '주식' 소모임에서 두드러지는 단어 10개(로그비 하위)
top_stock_terms <- clien_log_ratio %>%
  slice_min(log_ratio, n = 10, with_ties = FALSE)

# 9. 두 데이터 합치기
top_terms_combined <- bind_rows(top_ecar_terms, top_stock_terms)

# 10. 시각화
ggplot(top_terms_combined, aes(x = reorder(term, log_ratio), y = log_ratio, fill = log_ratio > 0)) + # 양수/음수로 색상 구분
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "darkblue")) +
  labs(
    title = "주제 간 베타 로그비 (Topic 1: '전기차' vs Topic 3: '주식')",
    subtitle = "값이 클수록 '전기차' 소모임, 작을수록 '주식 소모임에 특징적인 단어",
    x = "단어 (Term)",
    y = "Log2 Ratio (Beta_전기차 / Beta_주식)"
  ) +
  theme(text = element_text(family = "NanumGothic"))

# 토픽 1과 토픽 3은 그 성격이 뚜렷하게 구분된다. 그 이유는 다음과 같다.
# 토픽 1은 전기차 소모임으로 보인다. 차, 충전, 배터리, 엔진, 차량 같은 단어들이 토픽 3 대비 베타 ratio에서 플러스 방향으로 높은 값을 기록했기 때문이다.
# 토픽 3은 주식 소모임으로 보인다. 계좌, 주식, 수익, 기억, 투자 같은 단어들이 토픽 1 대비 베타 ratio에서 마이너스 방향으로 높은 값을 기록했기 때문이다.
