### 퀴즈 1 ###

library(tidyverse)
library(caret)
library(e1071)
library(showtext)

ctrl_svm <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = multiClassSummary, # F1, Precision 등 계산
  classProbs = TRUE,                   # 확률 계산 활성화
  sampling = "down"                    # 다운샘플링 적용
)

# 명시적 튜닝 그리드 생성
grid_tuning <- expand.grid(
  C =c(0.1, 5, 10) # Cost 값 3개 탐색
)

# caret 훈련(tuneGrid 지정)
set.seed(123) # 교차검증 재현성
svm_model_tuned <- train(
  x = train_x,  
  y = train_y,  
  method = "svmLinear",
  trControl = ctrl_svm,
  tuneGrid = grid_tuning 
)
save(svm_model_tuned, file="svm_model_tuned.rda")

# 훈련 결과 확인(최적의 Cost 값 확인)
svm_model_tuned # C = 0.1

# 테스트셋 예측 및 혼동 행렬
svm_pred_tuned <- predict(svm_model_tuned, newdata = test_x)
svm_cm_tuned <- confusionMatrix(data = svm_pred_tuned, reference = test_y, positive = "Pos")
svm_cm_tuned

### 퀴즈 2 ###

nb_metrics <- tibble(
  Model = "Naive Bayes",                                          # 모델 이름 지정.
  Accuracy = nb_cm_tuned$overall["Accuracy"],                     # 전체 정확도를 추출함.
  Precision = nb_cm_tuned$byClass["Precision"],                   # 정밀도를 추출함.
  Recall = nb_cm_tuned$byClass["Recall"],                         # 재현율을 추출함.
  Specificity = nb_cm_tuned$byClass["Specificity"],               # 특이도를 추출함.
  `Balanced Accuracy` = nb_cm_tuned$byClass["Balanced Accuracy"], # 균형 정확도를 추출함.
  F1 = nb_cm_tuned$byClass["F1"]                                  # F1-Score를 추출함.
)

svm_metrics <- tibble(
  Model = "SVM",
  Accuracy = svm_cm_tuned$overall["Accuracy"],
  Precision = svm_cm_tuned$byClass["Precision"],
  Recall = svm_cm_tuned$byClass["Recall"],
  Specificity = svm_cm_tuned$byClass["Specificity"],               
  `Balanced Accuracy` = svm_cm_tuned$byClass["Balanced Accuracy"], 
  F1 = svm_cm_tuned$byClass["F1"]
)

comparison_data <- bind_rows(nb_metrics, svm_metrics) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

font_add_google("Nanum Gothic", "nanumgothic")
showtext_auto()

nb_svm_plot <- comparison_data %>%
  ggplot(aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, position = position_dodge(0.9), size = 7) +
  labs(
    title = "나이브 베이즈 vs. SVM 모델 성능 비교",
    subtitle = "테스트 데이터셋에 대한 평가",
    x = "평가 지표",
    y = "점수",
    fill = "모델"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    text = element_text(family = "nanumgothic"), 
    plot.title = element_text(size = 25, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )
ggsave("nb_svm_plot.png", nb_svm_plot, width = 7, height = 7)

# 나이브 베이즈 튜닝 모델과 서포트 벡터 머신 튜닝 모델의 여섯 가지 성능지표 중 가장 큰 차이를 보이는 지표는 Specificity와 Balanced Accuracy임. 그 이유는 나이브 베이즈 튜닝 모델이 부정 극성을 올바르게 분류하는 데 완전히 실패한 반면(Specificity = 0), 서포트 벡터 머신 튜닝 모델은 부정 극성을 올바르게 분류하는 데 좋은 성능(Specificity = 0.9634)을 나타냈기 때문. 덧붙여, 서포트 벡터 머신 튜닝 모델은 Kappa 지수도 0.7832에 달할 만큼 좋은 분류 성능을 보였으며, Accuracy가 NIR(No Information Rate)을 통계적으로 유의미한 수준으로 능가하는 것으로 나타남. 이에 따라 서포트 벡터 머신 튜닝 모델은 나이브 베이즈 튜닝 모델에 비해 부정 극성 분류 성능이 탁월하게 높은 까닭에 Accuracy, Specificity, Balanced Accuracy 모두 높은 수치를 나타냈다고 결론 내릴 수 있음.

### 퀴즈 3 ###

library(tidyverse)          
library(quanteda)           
library(jsonlite)           
library(reticulate)         
library(furrr)
library(stringi)
library(cluster)
library(FactoMineR)
library(factoextra)
options(tibble.width = Inf)

## 데이터 전처리하기

# JSON 파일 목록 불러오기.
unsupervised.list <- list.files(path = "ML_text/unsupervised_quiz", pattern = "json$", recursive = T)
unsupervised.list.1 <- str_c("ML_text/unsupervised_quiz/", unsupervised.list)

safe_read_json <- function(file) {
  tryCatch({
    json <- fromJSON(file)
    tibble(
      doc_id = json$dataset$name,
      true_topic = json$info$annotations$subject,
      text = json$info$annotations$text
    )
  }, error = function(e) {
    message(" Error in file: ", file)
    return(NULL)
  })
}

# map_dfr 함수를 사용하여 목록 내 파일 일괄적으로 읽어들이기.
unsupervised.list.2 <- map_dfr(unsupervised.list.1, safe_read_json)

# 데이터셋의 토픽 종류 확인
unsupervised.list.2 %>%
  pull(true_topic) %>%
  unique()

unsupervised.list.2 %>%
  count(true_topic) %>%
  View()

set.seed(123)
unsupervised.dataset <- unsupervised.list.2 %>%
  filter(true_topic %in% c("식음료", "가족", "미용", "교통", "군대")) %>%
  group_by(true_topic) %>%
  sample_n(1000) %>%
  ungroup()
  
use_condaenv("my_env")

kiwi <- import("kiwipiepy")$Kiwi()
kiwi_tokenizer <- function(text) {
  results <- kiwi$analyze(text)
  if(length(results) == 0 || length(results[[1]]) == 0) {
    return(character(0))
  }
  tagged_tokens <- map_dfr(results[[1]][[1]], ~tibble( 
    morph = .x$form, # 형태소
    pos = .x$tag     # 품사
  ))
  
  stop_words_pos <- c("이/VCP", "것/NNB", "서/NNB", "곳/NNG", "곳/NNB", "하/VX", "하/VV", "수/NNB", "되/VV", "듯/NNB",
                      "하/XSA", "들/XSN", "되/XSA", "되/XSV", "하/XSV", "이/VCP", "이/VV", "적/XSN",
                      "그/MM", "보/VX")
  token_vector <- tagged_tokens %>%
    # filter()와 정규표현식(str_detect)으로 불필요한 품사 제거
    filter(!str_detect(pos, "^J"), # J로 시작하는 조사(JKS, JKC...) 제거
           !str_detect(pos, "^E"), # E로 시작하는 어미(EF, EP...) 제거
           !str_detect(pos, "SF|SP|SS"), # 구두점(마침표, 쉼표 등) 제거
           !str_detect(morph, "\\#[ㄱ-ㅎ가-힣]{1,20}|\\*{1,3}|1|2|3|4|5") # 개인신상을 특정할 수 있는 고유명사 대신 붙은 "#"이 앞에 있는 어절과 ** 그리고 대화자 번호 id(1, 2, 3, 4, 5...) 제거.
    ) %>%
    mutate(tagged_morph = str_c(morph, "/", pos)) %>% 
    filter(!tagged_morph %in% stop_words_pos) %>%
    pull(morph) 
  return(token_vector)
}  

unsupervised.dataset_tokenized_quiz <- unsupervised.dataset %>%
  mutate(text = stri_enc_toutf8(text),
         text = future_map(text, kiwi_tokenizer),
         text = future_map_chr(text, ~str_c(.x, collapse = " ")) 
  )
save(unsupervised.dataset_tokenized_quiz, file="unsupervised.dataset_tokenized_quiz.rda")

uns_corpus <- corpus(
  unsupervised.dataset_tokenized_quiz, # 입력 데이터(tibble 또는 data.frame)
  docid_field = "doc_id", # 문서 ID로 사용할 열 이름
  text_field = "text" # 실제 텍스트 내용이 담긴 열 이름
)

uns_tokens <- tokens(uns_corpus, remove_punct = TRUE) 

# docvars()(document variables) 함수로 Corpus에 메타데이터(정답 주제)를 저장.
docvars(uns_corpus, "true_topic") <- unsupervised.dataset_tokenized_quiz$true_topic

uns_dfm <- dfm(uns_tokens)

# 말뭉치 등장 빈도 최저한도 설정
uns_dfm_trimmed_min <- dfm_trim(uns_dfm, 
                                min_termfreq = 5, # 전체에서 최소 5회 이상 등장.
                                min_docfreq = 2, # 최소 2개 문서 이상 등장.
                                docfreq_type = "count") 

uns_dfm_trimmed_final <- dfm_trim(
  uns_dfm_trimmed_min, 
  # 전체 문서의 70%를 초과하여 등장하는 단어는 제거.
  max_docfreq = 0.70, 
  docfreq_type = "prop"
  )

tfidf_dfm <- dfm_tfidf(uns_dfm_trimmed_final)

# 기본행렬(matrix) 포맷으로 변환
tfidf_matrix <- as.matrix(tfidf_dfm)

# scale() 함수를 통해 단어들의 TF-IDF 값들을 표준화해줌.
tfidf_matrix_scaled_quiz <- scale(as.matrix(tfidf_dfm))
save(tfidf_matrix_scaled_quiz, file="tfidf_matrix_scaled_quiz.rda")

uns_nbclust_quiz <- fviz_nbclust(
  tfidf_matrix_scaled_quiz, # 입력 데이터(숫자 행렬).
  FUNcluster = pam, # 사용할 군집 알고리즘 지정(K-Medoids).
  method = "silhouette", # K 평가 방법(실루엣 계수).
  k.max = 5 # 테스트할 최대 K 값(실제 대화유형이 5개이므로 5로 설정).
  )
save(uns_nbclust_quiz, file="uns_nbclust_quiz.rda")
ggsave("uns_nbclust_quiz_plot.png", uns_nbclust_quiz, width=7, height=10)

pam_result_k3_quiz <- pam(tfidf_matrix, # 입력 데이터(숫자 행렬)
                          k = 3 # 앞서 찾은 최적의 K 값(3개)
                          )
save(pam_result_k3_quiz, file="pam_result_k3_quiz.rda")

# 군집 할당 결과 확인
pam_result_k3_quiz$clustering

# 군집 시각화
uns_cluster_k3_quiz <- fviz_cluster(
  pam_result_k3_quiz, # PAM 모델 결과 객체
  data = tfidf_matrix_scaled_quiz, # 원본 데이터(필수는 아님)
  ellipse.type = "convex", # 군집을 볼록 껍질(convex hull)로 표시
  ggtheme = theme_bw() # 그래프 테마
)
ggsave("uns_cluster_k3_quiz.png", uns_cluster_k3_quiz, width = 10, height = 10)

# Dim1은 전체 데이터셋의 0.3퍼센트의 정보(분산)를 그리고 Dim2는 전체 데이터셋의 0.2퍼센트의 정보(분산)를 설명해줌. 즉 Dim1과 Dim2 두 차원으로는 전체 데이터셋이 가진 0.5%의 정보(분산)밖에 압축하지 못힘. 그리고 1번은 거대군집으로서 사실상 그 안에 2번 군집이 내포되어 있음. 그리고 3번 군집과도 오버랩의 정도가 심함. 따라서 3개의 PAM 군집은 실제로는 분리되어 있지만, 그 분리는 Dim1(0.3%)과 Dim2(0.2%)라는 얕은 차원에서는 일어나지 않고, 우리가 보지 못하는 99%의 고차원(Dim3, Dim4…)에서 일어나고 있음.
