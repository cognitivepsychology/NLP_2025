### 11주차 퀴즈

## 문항 1: 임베딩을 처음부터 학습하는 모델 구축 및 평가


# ===================================================================
# 0. 라이브러리 및 환경설정
# ===================================================================
# 필요한 패키지 로드
library(tidyverse)
library(jsonlite)
library(reticulate)
library(keras)
library(googledrive) # 구글 드라이브 다운로드용
library(stringi)     # 문자열 인코딩 처리

# 가상환경 설정 (본인의 가상환경 이름으로 변경)
# 해당 환경에 kiwipiepy가 설치되어 있어야 함
use_condaenv("my_env")

# ===================================================================
# 1. 데이터 로드 및 전처리
# ===================================================================

# 1-1. 구글 드라이브에서 데이터 다운로드 및 압축 해제
# 구글 드라이브 인증
drive_auth()

# 파일 ID 및 다운로드 경로 설정
file_id <- "1MnyXdJ5W_GfxRTRUB-3iDeaIVRb2WvKG"
download_dest <- "quiz_dataset.zip"
extract_dir <- "DL_text/unsupervised_quiz"

# 파일 다운로드
drive_download(as_id(file_id), path = download_dest, overwrite = TRUE)

# 압축 해제 폴더 생성 및 압축 해제
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
unzip(download_dest, exdir = extract_dir)

# 1-2. 데이터 읽기 함수 정의 (safe_read_json)
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

# 1-3. 데이터 일괄 읽기 및 필터링/샘플링
# 파일 목록 가져오기
file_list <- list.files(extract_dir, pattern = "json$", full.names = TRUE, recursive = TRUE)

# 데이터 읽기 및 병합
full_data <- file_list %>%
  map_dfr(safe_read_json)

# 주제 필터링 및 랜덤 샘플링
target_topics <- c("식음료", "가족", "미용", "교통", "군대")
set.seed(123) # 난수 고정

filtered_data <- full_data %>%
  filter(true_topic %in% target_topics) %>%
  group_by(true_topic) %>%
  sample_n(1000) %>%
  ungroup()

# 1-4. 형태소 분석 및 토큰화 (kiwi_tokenizer)
# Kiwi 초기화 (파이썬 객체 불러오기)
kiwi <- import("kiwipiepy")$Kiwi()

# 토크나이저 함수 정의
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
           !str_detect(morph, "\\#[ㄱ-ㅎ가-힣]{1,20}|\\*{1,3}|1|2|3|4|5") # 특수 패턴 및 숫자 ID 제거
    ) %>%
    mutate(tagged_morph = str_c(morph, "/", pos)) %>%
    filter(!tagged_morph %in% stop_words_pos) %>%
    pull(morph)
  return(token_vector)
}

# 토큰화 실행 및 문자열 결합
processed_data <- filtered_data %>%
  mutate(text = stri_enc_toutf8(text),
         # map을 사용해 토큰화 수행
         text_tokenized = map(text, kiwi_tokenizer),
         # 토큰들을 띄어쓰기로 연결하여 하나의 문자열로 만듦(keras 입력용)
         text_final = map_chr(text_tokenized, ~str_c(.x, collapse = " "))) %>%
  filter(nchar(text_final) > 0) # 빈 텍스트 제거

save(processed_data, file="processed_data.rda")

# ===================================================================
# 2. Keras 전처리(수업자료 5. Keras 전처리 섹션 참조)
# ===================================================================
# 하이퍼파라미터 설정
vocab_size <- 5000
maxlen <- 100
embedding_dim <- 200

# 토크나이저 생성 및 학습
tokenizer <- text_tokenizer(num_words = vocab_size, oov_token = "<OOV>")
fit_text_tokenizer(tokenizer, processed_data$text_final)

# 텍스트 시퀀스 변환 및 패딩
sequences <- texts_to_sequences(tokenizer, processed_data$text_final)
x_data <- pad_sequences(sequences, maxlen = maxlen, padding = "pre")

# 라벨 원-핫 인코딩
# 팩터로 변환하여 숫자로 매핑 (알파벳 순서 기준 0~4 할당됨)
processed_data$label_factor <- factor(processed_data$true_topic)
processed_data$label_numeric <- as.numeric(processed_data$label_factor) - 1

y_data <- to_categorical(processed_data$label_numeric, num_classes = 5)

# 학습/테스트 데이터 분리
set.seed(123)
train_indices <- sample(1:nrow(x_data), size = round(0.8 * nrow(x_data)))
x_train <- x_data[train_indices, ]
y_train <- y_data[train_indices, ]
x_test <- x_data[-train_indices, ]
y_test <- y_data[-train_indices, ]

# ===================================================================
# 3. [실습 7] 시나리오 1: 임베딩을 '처음부터' 학습(train from the scratch)
# ===================================================================
# 이전 모델 메모리 정리
k_clear_session()

# 모델 구축
model_scratch <- keras_model_sequential(name = "Model_Scratch") %>%
  # trainable = TRUE로 설정하여 처음부터 학습
  layer_embedding(input_dim = vocab_size, output_dim = embedding_dim, input_length = maxlen, trainable = TRUE) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = "softmax")

# 모델 컴파일
model_scratch %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# 모델 구조 확인
summary(model_scratch)

# 모델 학습
history_scratch <- model_scratch %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_accuracy",
      patience = 3,
      restore_best_weights = TRUE
    )
  )
)

# 모델 평가
eval_scratch <- model_scratch %>% evaluate(
  x_test,
  y_test,
  verbose = 0
)

# 결과 출력 및 주석 작성 (실행 결과에 따라 값은 다를 수 있음)
eval_scratch
# loss  accuracy 
# 0.1690996  0.9450000 
# 시나리오 1 (처음부터 학습) 모델의 테스트 데이터 정확도는 약 94.5%이며, 손실값은 약 0.17임.


## 문항 2: 사전학습 임베딩 + 미세조정 모델 구축 및 평가

library(tidyverse)
library(reticulate)
library(keras)

# ===================================================================
# 1. 사전학습 Word2Vec 로드 및 임베딩 행렬 구축
# ===================================================================
# gensim 불러오기
gensim <- import("gensim")

# Word2Vec 모델 로드 (본인의 모델 파일 경로로 변경)
w2v_model <- gensim$models$Word2Vec$load("ko.bin")

# 임베딩 행렬 구축
word_index <- tokenizer$word_index
embedding_matrix <- matrix(0, nrow = vocab_size, ncol = embedding_dim)
w2v_vocab_list <- as.character(names(w2v_model$wv$vocab))

for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < vocab_size) {
    if (word %in% w2v_vocab_list) {
      embedding_vector <- as.numeric(w2v_model$wv[[word]])
      embedding_matrix[index, ] <- embedding_vector
    }
  }
}

# ===================================================================
# 2. 사전학습 + 미세조정(pre-trained + fine-tuning)
# ===================================================================
# 이전 모델 메모리 정리
k_clear_session()

# 모델 구축
model_finetune <- keras_model_sequential() %>%
  layer_embedding(input_dim = vocab_size, output_dim = embedding_dim, input_length = maxlen, trainable = TRUE) %>%
  layer_global_average_pooling_1d(name = "sentence_vector_extractor") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = "softmax")

# 임베딩 층 가중치 설정
get_layer(model_finetune, index = 1) %>%
  set_weights(list(embedding_matrix))

# 모델 컴파일
model_finetune %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# 모델 구조 확인
summary(model_finetune)

# 모델 학습
history_finetune <- model_finetune %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 2,
  callbacks = list(
    callback_early_stopping(
      monitor = "val_accuracy",
      patience = 3,
      restore_best_weights = TRUE
    )
  )
)

# 모델 평가
eval_finetune <- model_finetune %>% evaluate(
  x_test,
  y_test,
  verbose = 0
)

# 결과 출력 및 주석 작성
eval_finetune
# loss  accuracy
# 0.236548 0.919000
# 시나리오 2 모델의 테스트 데이터 정확도는 약 92%이며, 손실값은 약 0.4236임.


## 문항 3. 딥러닝 비지도 군집화(K-Medoids & PCA): model_scratch 모델 재활용

# 필요한 패키지 로드
library(tidyverse)
library(reticulate)
library(keras)
library(cluster)
library(FactoMineR)
library(factoextra)

# ===================================================================
# 딥러닝 비지도 군집화(K-Medoids & PCA): model_scratch 모델 재활용 시
# ===================================================================
# 의미 벡터 추출기 모델 생성
extractor_model <- keras_model(
  inputs = model_scratch$input,
  outputs = get_layer(model_scratch, index = 2)$output
)

# 의미 벡터 추출
sentence_vectors <- extractor_model %>% predict(x_data)

# PCA를 이용한 차원 축소 (50차원)
pca_for_cluster <- PCA(sentence_vectors, ncp = 50, graph = FALSE)
pca_vectors_50d <- pca_for_cluster$ind$coord

# K-Medoids 군집화 (k=5)
set.seed(123)
kmed_result <- pam(pca_vectors_50d, k = 5)

# 혼동 행렬 생성 및 출력
m_scratch <- table(True_Label = processed_data$label_numeric, Cluster_ID = kmed_result$clustering)
m_scratch

# Cluster_ID
# True_Label   1   2   3   4   5
# 0          963   2  10  17   8
# 1           25 951   7   4  13
# 2            5   0   1   7 987
# 3            7   1 978  12   2
# 4            9   2   5 983   1

# 시각화를 위한 PCA (2차원)
pca_for_viz <- PCA(sentence_vectors, ncp = 2, graph = FALSE)

# K-Medoids 군집 결과 시각화
graph_k_medoids_1 <- fviz_pca_ind(pca_for_viz,
                                  habillage = as.factor(kmed_result$clustering),
                                  geom.ind = "point",
                                  addEllipses = TRUE,
                                  ellipse.type = "convex",
                                  palette = "jco",
                                  ggtheme = theme_minimal()) +
  labs(title = "딥러닝 '의미 벡터' 기반 K-Medoids 군집화 (model_scratch)")



# 실제 라벨 시각화
graph_pca_true_label_1 <- fviz_pca_ind(pca_for_viz,
                                       geom.ind = "point",
                                       habillage = processed_data$label_factor,
                                       addEllipses = TRUE,
                                       ellipse.type = "convex",
                                       ggtheme = theme_minimal()) +
  labs(title = "PCA 2D 시각화 (실제 주제)")

# 결과 비교 및 주석 작성
# 혼동행렬을 보면, 각 실제 라벨이 특정 군집에 집중되어 나타나는 것을 확인할 수 있음.
# 가족은 주로 1번 군집에, 교통은 2번 군집에, 군대는 5번 군집에, 미용은 3번 군집에, 식음료는 4번 군집에 할당되었음.
# 두 시각화 그래프를 비교해보면, 군집화 결과와 실제 라벨의 분포가 매우 유사함을 알 수 있음.
# 이는 model_scratch가 학습한 의미 벡터가 실제 주제를 잘 반영하고 있음을 의미하며, 딥러닝 기반의 비지도 군집화가 효과적으로 수행되었음을 보여줌.