### 문제 1: caret 기본값 모델 훈련(초매개변수 미설정)


# 0. 공통 패키지 로드 및 함수 정의

library(jsonlite)
library(tidyverse)
library(reticulate)
library(quanteda)
library(caret)
library(e1071) # caret의 naive_bayes가 의존함

use_condaenv("my_env")
kiwi <- import("kiwipiepy")$Kiwi()
kiwi_tokenizer <- function(text) {
  results <- kiwi$analyze(text)
  tagged_tokens <- map_dfr(results[[1]][[1]], ~tibble( # results는 여러 겹의 리스트로 되어 있는데, 이때 results의 첫 번째 리스트의 첫 번째 요소를 tibble() 함수의 입력으로 사용하겠다는 뜻!
    morph = .x$form, # 형태소 추출.
    pos = .x$tag # 품사 추출.
  ))
  stop_words_pos <- c("이/VCP", "것/NNB", "곳/NNG", "곳/NNB", "하/VX", "하/VV", "수/NNB", "되/VV", "듯/NNB",
                      "하/XSA", "들/XSN", "되/XSA", "되/XSV", "하/XSV", "이/VCP", "이/VV", "적/XSN")
  filtered_tokens <- tagged_tokens %>%
    filter(!str_detect(pos, "^J"), # J로 시작하는 조사 제거([EX] JKS, JKC).
           !str_detect(pos, "^E"), # E로 시작하는 어미 제거([EX] EF, EP).
           !str_detect(pos, "SF|SP|SS") # 구두점 제거. "ㅋ"나 "ㅎ" 같은 자모는 SW로 분류되므로 S로 시작되는 모든 기호를 지워선 안 됨.
    )%>%
    mutate(tagged_morph = str_c(morph, "/", pos)) %>% # 형태소와 품사 태그를 '/'로 합쳐서 새로운 칼럼 만들기.
    filter(!tagged_morph %in% stop_words_pos) %>% # "형태소/품사태그" 칼럼에 대해 불용어 목록으로 필터링하기.
    select(morph) %>% # morph 칼럼만 남기고 나머지 칼럼 제거하기.
    pull() # morph 칼럼 벡터화하기.
  return(filtered_tokens)
}

# 1. 데이터 로드, 필터링, 레이블 생성

review_df <- list.files(path = "ML_text/week_9_quiz", pattern = "json$", recursive = T)
review_df.1 <- str_c("ML_text/week_9_quiz/", review_df)

# map_dfr 함수를 사용하여 목록 내 파일 일괄적으로 읽어들이기.
review_df.2 <- map_dfr(review_df.1, ~ fromJSON(.x, simplifyDataFrame = T)) %>%
  as_tibble()

# 데이터 구조 확인
review_df.2 
review_df_filtered <- review_df.2 %>%
  select(RawText, GeneralPolarity) %>%
  filter(GeneralPolarity != "0") %>%
  mutate(review_label = factor(GeneralPolarity, 
                               levels = c("1", "-1"), 
                               labels = c("Pos", "Neg"))) %>%
  select(-GeneralPolarity) %>%
  rename(text = RawText)

review_df_filtered %>%
  group_by(review_label) %>%
  count()

review_df_filtered_tokenized <- review_df_filtered %>%
  mutate(text = stri_enc_toutf8(text),
         text = future_map(text, kiwi_tokenizer),
         text = future_map_chr(text, ~str_c(.x, collapse = " ")) 
  )
save(review_df_filtered_tokenized, file="review_df_filtered_tokenized.rda")

# 2. 훈련/테스트 데이터프레임 분할 (원본 데이터 기준)
set.seed(123)
train_idx <- createDataPartition(review_df_filtered$review_label, p = 0.7, list = FALSE)

train_data <- review_df_filtered[train_idx, ]
test_data <- review_df_filtered[-train_idx, ]

# 3. 레이블 벡터 생성
train_y <- train_data$review_label
test_y <- test_data$review_label

# 4. 훈련용 DFM 생성 및 변환
train_corpus <- corpus(train_data, text_field = "text")
train_tokens <- tokens(train_corpus,
                       remove_punct = TRUE)
train_dfm <- dfm(train_tokens)

train_dfm_trimmed <- dfm_trim(train_dfm, min_termfreq = 10, verbose = TRUE)
train_dfm_tfidf <- dfm_tfidf(train_dfm_trimmed)

# 5. 테스트용 DFM 생성 및 변환 (훈련용 DFM 기준)
test_corpus <- corpus(test_data, text_field = "text")
test_tokens <- tokens(test_corpus, 
                      remove_punct = TRUE)
test_dfm <- dfm(test_tokens)

# 6.훈련용 피처와 일치시키기
test_dfm_matched <- dfm_match(test_dfm, features = featnames(train_dfm_tfidf))
test_dfm_tfidf <- dfm_tfidf(test_dfm_matched) # 일치된 DFM에 TF-IDF 적용

# 7. DFM을 데이터프레임으로 변환
train_x <- quanteda::convert(train_dfm_tfidf, to = "data.frame") %>%
  select(-doc_id) # doc_id 컬럼 제거
test_x <- quanteda::convert(test_dfm_tfidf, to = "data.frame") %>%
  select(-doc_id) # doc_id 컬럼 제거
colnames(train_x)

# 8. trainControl 설정
ctrl_cv <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = multiClassSummary, # F1, Precision 등 계산
  classProbs = TRUE,                   # 확률 계산 활성화
  sampling = "down"                    # 다운샘플링 적용
)

# 9. caret 훈련
set.seed(123)
nb_model_default <- train(
  x = train_x,  
  y = train_y,  
  method = "naive_bayes",
  trControl = ctrl_cv
)

# 10. 훈련 결과 확인(caret이 자동 선택한 초매개변수 확인)
nb_model_default

# 10. 테스트셋 예측 및 혼동 행렬
nb_pred_default <- predict(nb_model_default, newdata = test_x)
nb_cm_default <- confusionMatrix(data = nb_pred_default, reference = test_y, positive = "Pos")
nb_cm_default


### 문제 2: caret 활용 초매개변수 튜닝
  
# 1. 명시적 튜닝 그리드 생성
grid_tuning <- expand.grid(
  laplace = 0, 
  usekernel = TRUE, 
  adjust = c(0.5, 1.0, 1.5) # adjust 값 3개 탐색
)

# 2. caret 훈련(tuneGrid 지정)
set.seed(123) # 교차검증 재현성
nb_model_tuned <- train(
  x = train_x,  
  y = train_y,  
  method = "naive_bayes",
  trControl = ctrl_cv,
  tuneGrid = grid_tuning 
)

# 3. 훈련 결과 확인(최적의 adjust 값 확인)
nb_model_tuned # laplace=0, usekernel=TRUE, adjust=0.5

# 4. 테스트셋 예측 및 혼동 행렬
nb_pred_tuned <- predict(nb_model_tuned, newdata = test_x)
nb_cm_tuned <- confusionMatrix(data = nb_pred_tuned, reference = test_y, positive = "Pos")


### 문제 3: 모델 성능 비교 분석 및 해석
  
# 1. 훈련 성능 비교
model_list <- list(Default = nb_model_default, Tuned = nb_model_tuned)
results <- resamples(model_list)
summary(results)

# 2. 테스트 성능 비교
confusionMatrix(data = nb_pred_default, reference = test_y)
confusionMatrix(data = nb_pred_tuned, reference = test_y)
comparison_table <- tibble(
  Model = c("Default", "Tuned"),
  Accuracy = c(
    nb_cm_default$overall['Accuracy'],
    nb_cm_tuned$overall['Accuracy']
  ),
  Precision_Pos = c(
    nb_cm_default$byClass['Precision'],
    nb_cm_tuned$byClass['Precision']
  ),
  Recall_Pos = c(
    nb_cm_default$byClass['Recall'],
    nb_cm_tuned$byClass['Recall']
  ),
  F1_Score_Pos = c(
    nb_cm_default$byClass['F1'],
    nb_cm_tuned$byClass['F1']
  )
)

nb_model_default$bestTune # laplace=0, usekernel=TRUE, adjust=1.

# 3. 결과 해석
# 디폴트 모델에서 caret이 자동 선택한 최적의 초매개변수는 nb_model_default$bestTune 출력 결과 usekernel=TRUE, adjust=1였음. 
# comparison_table의 테스트셋 성능을 보면, 튜닝 모델의 테스트셋 Accuracy는 Default 모델과 동일했음.
# F1-Score 역시 두 모델이 동일했음. comparison_table에서는 두 모델 모두 일견 성능이 우수해 보이나, Kappa가 0이고 Specificity 또한 0이었음. 이는 우연 수준 이상의 성능을 보여주지 못했으며, 특히 부정 극성 분류는 성능이 0일 정도로 매우 취약함을 알 수 있음.
# 튜닝 모델이 찾은 최적의 adjust 값은 0.5로서, 디폴트 모델이 자동으로 선택한 adjust 값 1과 달랐음. 그럼에도 불구하고 튜닝 모델은 디폴트 모델과 비교하여 데이터의 분포를 더 잘 반영하지 못했음.
# 이는 디폴트 모델이 이미 최적의 값(usekernel=T, adjust=1)을 찾았기 때문에, 튜닝 모델의 성능과 큰 차이가 없었음을 의미함. 따라서 나이브 베이즈 모델은 adjust 값의 변동에도 불구하고 디폴트와 튜닝 모델 모두 부정 극성을 올바르게 분류하는 데 실패한 것으로 결론 내릴 수 있음.
