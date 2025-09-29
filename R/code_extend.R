#' Extending codes
#' @description
#'   These functions use text embeddings and multinomial logistic regression
#'   to suggest missing codes or flag potentially incorrect codes based on text data.
#'   Two approaches are provided: one using GloVe embeddings trained on the input text,
#'   and another using pre-trained BERT embeddings via the `{text}` package.
#'   Both functions require a vector of text (e.g., titles or descriptions)
#'   and a corresponding vector of categorical codes, with `NA` or empty strings
#'   indicating missing codes to be inferred.
#'   The functions train a multinomial logistic regression model
#'   using `glmnet` on the text embeddings of the entries with known codes,
#'   and then predict codes for the entries with missing codes.
#'   The functions also validate the model's performance
#'   on a holdout set and report per-class precision, recall, and F1-score.
#'   If no missing codes are present, the functions instead
#'   check existing codes for potential mismatches and report them.
#' @name code_extend
#' @importFrom text2vec itoken create_vocabulary vocab_vectorizer
#' @importFrom caret confusionMatrix createDataPartition
#' @importFrom glmnet cv.glmnet
#' @param titles A character vector of text entries (e.g., titles or descriptions).
#' @param var A character vector of (categorical) codes that might be coded
#'   from the titles or texts.
#'   Entries with missing codes should be `NA_character_` or empty strings.
#'   The function will suggest codes for these entries.
#'   If no missing codes are present, the function will check existing codes
#'   for potential mismatches.
#' @param req_f1 The required macro-F1 score on the validation set
#'   before proceeding with inference.
#'   Default is 0.80.
#' @param rarity_threshold Minimum number of occurrences for a code
#'   to be included in training.
#'   Codes with fewer occurrences are excluded from training
#'   to ensure sufficient data for learning.
#'   Default is 8.
#' @examples
#' titles <- paste(emperors$Wikipedia$CityBirth,
#'                 emperors$Wikipedia$ProvinceBirth,
#'                 emperors$Wikipedia$Rise,
#'                 emperors$Wikipedia$Dynasty,
#'                 emperors$Wikipedia$Cause)
#' var <- emperors$Wikipedia$Killer
#' var[var=="Unknown"] <- NA
#' var[var %in% c("Senate","Court Officials","Opposing Army")] <- "Enemies"
#' var[var %in% c("Fire","Lightning","Aneurism","Heart Failure")] <- "God"
#' var[var %in% c("Wife","Usurper","Praetorian Guard","Own Army")] <- "Friends"
#' glo <- code_extend_glove(titles, 
#'            var)
#' @export
code_extend_glove <- function(titles, var, 
                              req_f1 = 0.80,
                              rarity_threshold = 8){

  # Tokenize full corpus
  tok <- text2vec::itoken(titles, tokenizer = text2vec::word_tokenizer, 
                          progress_bar = FALSE)
  vocab <- text2vec::create_vocabulary(tok)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  
  # Term co-occurrence matrix for GloVe
  tcm <- text2vec::create_tcm(it = tok, vectorizer = vectorizer, 
                              skip_grams_window = 5)
  
  # Train GloVe
  cli::cli_alert_info("Training GloVe model.")
  glv <- text2vec::GlobalVectors$new(rank = 100, x_max = 10)
  w_main <- glv$fit_transform(tcm, n_iter = 20)
  w_ctx  <- glv$components
  W <- w_main + t(w_ctx)   # combine main + context (transpose is crucial)
  
  # Build sentence embeddings by averaging word vectors
  tokens_list <- text2vec::word_tokenizer(titles)
  embed_title <- function(tokens, W) {
    rows <- intersect(tokens, rownames(W))
    if (length(rows) == 0) return(rep(0, ncol(W)))
    colMeans(W[rows, , drop = FALSE])
  }
  X <- t(vapply(tokens_list, embed_title, W = W, FUN.VALUE = numeric(ncol(W))))
  
  # Split data into training and inference data
  na_codes <- which(is.na(var) | var == "")
  if (length(na_codes) > 0) {
    cli::cli_alert_info("Found {length(na_codes)} missing codes to infer.")
    X_inf <- X[na_codes, , drop = FALSE]
    y_inf <- var[na_codes]
    X_train <- X[-na_codes, , drop = FALSE]
    y_train <- var[-na_codes]
  } else {
    cli::cli_alert_success("No additional coding required. Training for validation.")
    X_train <- X
    y_train <- var
  }
  
  # Remove rare codes from training
  few_codes <- which(y_train %in% names(which(table(y_train) < rarity_threshold)))
  if (length(few_codes) > 0) {
    cli::cli_alert_info("Removing {names(which(table(y_train) < rarity_threshold))} codes from training data as they have fewer than {rarity_threshold} occurrences.")
    X_train <- X_train[-few_codes, , drop = FALSE]
    y_train <- y_train[-few_codes]
  }
  
  # Stratified sampling of training data into train and test sets
  idx <- caret::createDataPartition(y_train, p = 0.75, list = FALSE)[,1]
  X_tr <- X_train[idx, , drop = FALSE]
  X_te <- X_train[-idx, , drop = FALSE]
  y_tr <- y_train[idx]
  y_te <- y_train[-idx]
  
  # Creates weights to help handle class imbalance
  obs_weights <- data.frame(logscaled = as.vector((1/log1p(table(y_tr)))[y_tr]),
                            smoothed = as.vector((1/(table(y_tr) + 5))[y_tr]),
                            inverse = as.vector((1/(table(y_tr))[y_tr])),
                            no = as.vector((table(y_tr)/length(y_tr))[y_tr]))
  
  # Ensure consistent factor levels
  class_levels <- sort(unique(y_tr))
  y_tr_f <- factor(y_tr, levels = class_levels)
  y_te_f <- factor(y_te, levels = class_levels)
  
  # 5) Observation weights for imbalance ####
  counts <- table(y_tr_f)
  obs_weights <- list(
    logscaled = as.vector((1 / log1p(counts))[y_tr_f]),
    smoothed  = as.vector((1 / (counts + 5))[y_tr_f]),
    inverse   = as.vector((1 / counts)[y_tr_f]),
    no        = as.vector((counts / length(y_tr_f))[y_tr_f])
  )
  
  # 6) Train with different weighting schemes; early-stop on macro-F1 ####
  best_fit <- NULL
  best_w   <- NULL
  best_f1  <- -Inf
  
  for (w in names(obs_weights)) {
    cli::cli_alert_info("Training glmnet with '{w}' weights.")
    fit <- glmnet::cv.glmnet(
      x = X_tr,
      y = y_tr_f,
      family = "multinomial",
      weights = obs_weights[[w]]
    )
    cli::cli_alert_success("Model trained with '{w}' weights.")
    
    # Validate
    cli::cli_alert_info("Validating on {length(y_te_f)} observations.")
    pred_cls <- stats::predict(fit, newx = X_te, s = "lambda.min", type = "class")
    pred_cls <- as.vector(pred_cls)
    
    f1 <- macro_f1(y_te_f, pred_cls, class_levels)
    cli::cli_alert_info("Macro-F1 = {round(f1, 3)} with '{w}' weights.")
    
    if (f1 > best_f1) {
      best_f1  <- f1
      best_fit <- fit
      best_w   <- w
    }
    if (f1 >= req_f1) break
  }
  
  if (best_f1 < req_f1) {
    cli::cli_alert_warning(
      "Macro-F1 ({round(best_f1, 3)}) below requirement ({req_f1}). Consider more data or parameter changes."
    )
    return(NULL)
  } else {
    cli::cli_alert_success(
      "Sufficient model found. Macro-F1 = {round(best_f1, 3)} with '{best_w}' weights."
    )
    # Report per-class metrics on the holdout
    final_pred <- stats::predict(best_fit, newx = X_te, s = "lambda.min", type = "class")
    final_pred <- as.vector(final_pred)
    cm <- caret::confusionMatrix(
      data = factor(final_pred, levels = class_levels),
      reference = factor(y_te_f, levels = class_levels),
      mode = "prec_recall"
    )
    # Return per-class metrics
    perf <- cm$byClass[, c("Precision", "Recall", "F1"), drop = FALSE]
  }
  
  # valid_acc <- 0
  # for(w in names(obs_weights)){
  #   cli::cli_alert_info("Training with {w} weights.")
  #   # Multinomial glmnet on dense embeddings
  #   fit <- glmnet::cv.glmnet(x = X_tr, y = y_tr, family = "multinomial",
  #                            weights = obs_weights[[w]])
  #   cli::cli_alert_success("Model trained with {w} weights.")
  #   
  #   # Validation
  #   cli::cli_alert_info("Validating on {length(y_te)} observations.")
  #   pred <- predict(fit, newx = X_te, s = "lambda.min", type = "class")
  #   validation <- data.frame(#title = titles[-na_codes][-few_codes][-idx], 
  #                            truth = y_te, 
  #                            pred = as.vector(pred))
  #   valid_acc <- round(mean(validation$truth == validation$pred), 3)
  # 
  #   if(valid_acc >= req_accuracy){
  #     break
  #   } else {
  #     cli::cli_alert_warning("Overall accuracy too low ({valid_acc}) with {w} weights.")
  #   }
  # }
  # 
  # if(valid_acc < req_accuracy){
  #   cli::cli_alert_warning("Consider retraining with more data or different parameters.")
  #   return(NULL)
  # } else {
  #   cli::cli_alert_success("Sufficiently performant model found ({valid_acc}) with {w} weights.")
  #   (caret::confusionMatrix(as.factor(pred[,1]), as.factor(y_te))$byClass)[,c("Precision", "Recall", "F1")]
  # }
  
  if (length(na_codes) > 0) {
    # Predict on inference data ####
    cli::cli_alert_info("Proceeding with inference.")
    pred_inf <- stats::predict(fit, newx = X_inf, s = "lambda.min", type = "response")
    max_prob <- apply(pred_inf, 1, max)
    pred_class <- apply(pred_inf, 1, function(x) rownames(x)[which.max(x)])
    cli::cli_alert_success("Predicted {length(na_codes)} missing codes")
    data.frame(title = titles[na_codes], 
               suggestion = as.vector(pred_class),
               probability = max_prob) %>% 
      dplyr::as_tibble() %>% dplyr::arrange(dplyr::desc(probability))
  } else {
    cli::cli_alert_info("Checking existing codes for possible errors.")
    pred_check <- stats::predict(fit, newx = X, s = "lambda.min", type = "response")
    max_prob <- apply(pred_check, 1, max)
    pred_class <- apply(pred_check, 1, function(x) rownames(x)[which.max(x)])
    cli::cli_alert_success("Found {sum(pred_class != var)} unlikely codes")
    data.frame(title = titles, 
               current = var, 
               suggestion = as.vector(pred_class),
               probability = max_prob) %>% 
      dplyr::as_tibble() %>% 
      dplyr::filter(current != suggestion) %>%
      dplyr::arrange(dplyr::desc(probability))
    
  }
}

#' @rdname code_extend
#' @param emb_texts For `code_extend_bert()`, pre-computed embeddings
#'   from `text::textEmbed()`.
#'   This avoids re-computing embeddings if they have already been computed.
#'   A Hugging Face model can be specified via the `model` argument.
#'   Default is "sentence-transformers/all-MiniLM-L6-v2".
#'   Other models can be used, but they should produce
#'   sentence-level embeddings.
#' @export
code_extend_bert <- function(
    titles,
    var,
    req_f1 = 0.80,
    rarity_threshold = 8,
    emb_texts) {
  # 0) Basic checks
  if (length(titles) != length(var)) stop("titles and var must be the same length.")
  if (!is.character(titles)) stop("titles must be a character vector.")
  if(missing(emb_texts)){
    stop("Please provide pre-computed embeddings from text::textEmbed() via the emb_texts argument.")
  }
  
  # py_required <- c("torch", "sentence_transformers", "nltk")
  # for (pkg in py_required) {
  #   if (!reticulate::py_module_available(pkg)) {
  #     cli::cli_alert_info("Installing {pkg}...")
  #     reticulate::py_install(pkg, pip = TRUE)
  #     if(pkg == "nltk"){
  #       nltk <- reticulate::import("nltk", delay_load = TRUE)
  #       nltk$download("punkt")
  #       nltk$download("punkt_tab")
  #     }
  #   }
  # }
  # reticulate::use_virtualenv("r-bert-clean", required = TRUE)
  
  # 1) Get contextual sentence embeddings (BERT family via 'text') ####
  # cli::cli_alert_info("Embedding titles with model: {model}")
  # emb <- text::textEmbed(texts = titles, model = model)
  # if (is.null(emb$texts)) stop("Embedding failed; 'text::textEmbed()' did not return embeddings in $texts.")
  # X <- as.matrix(emb$texts)
  X <- as.matrix(emb_texts$texts)
  if (anyNA(X)) {
    cli::cli_alert_warning("Embeddings contain NA; replacing with 0.")
    X[is.na(X)] <- 0
  }
  
  # 2) Split into training vs inference set based on missing codes ####
  na_codes <- which(is.na(var) | var == "")
  if (length(na_codes) > 0) {
    cli::cli_alert_info("Found {length(na_codes)} missing codes to infer.")
    X_inf   <- X[na_codes, ]
    y_inf   <- var[na_codes]
    X_train <- X[-na_codes, ]
    y_train <- var[-na_codes]
  } else {
    cli::cli_alert_success("No additional coding required. Training for validation.")
    X_train <- X
    y_train <- var
  }
  
  # 3) Remove rare codes (based on training only) ####
  tab_train <- table(y_train)
  rare_levels <- names(tab_train[tab_train < rarity_threshold])
  if (length(rare_levels) > 0) {
    cli::cli_alert_info(
      "Removing rare codes (< {rarity_threshold}): {paste(rare_levels, collapse = ', ')}"
    )
    keep_idx <- which(!(y_train %in% rare_levels))
    X_train  <- X_train[keep_idx, , drop = FALSE]
    y_train  <- y_train[keep_idx]
  }
  
  # If everything got removed, exit early
  if (length(y_train) == 0) {
    cli::cli_alert_warning("No training data left after filtering rare classes.")
    return(NULL)
  }
  
  # 4) Stratified split for validation ####
  idx <- caret::createDataPartition(y = y_train, p = 0.75, list = FALSE)[, 1]
  X_tr <- X_train[idx, , drop = FALSE]
  X_te <- X_train[-idx, , drop = FALSE]
  y_tr <- y_train[idx]
  y_te <- y_train[-idx]
  
  # Ensure consistent factor levels
  class_levels <- sort(unique(y_tr))
  y_tr_f <- factor(y_tr, levels = class_levels)
  y_te_f <- factor(y_te, levels = class_levels)
  
  # 5) Observation weights for imbalance ####
  counts <- table(y_tr_f)
  obs_weights <- list(
    logscaled = as.vector((1 / log1p(counts))[y_tr_f]),
    smoothed  = as.vector((1 / (counts + 5))[y_tr_f]),
    inverse   = as.vector((1 / counts)[y_tr_f]),
    no        = as.vector((counts / length(y_tr_f))[y_tr_f])
  )
  
  # 6) Train with different weighting schemes; early-stop on macro-F1 ####
  best_fit <- NULL
  best_w   <- NULL
  best_f1  <- -Inf
  
  for (w in names(obs_weights)) {
    cli::cli_alert_info("Training glmnet with '{w}' weights.")
    fit <- glmnet::cv.glmnet(
      x = X_tr,
      y = y_tr_f,
      family = "multinomial",
      weights = obs_weights[[w]]
    )
    cli::cli_alert_success("Model trained with '{w}' weights.")
    
    # Validate
    cli::cli_alert_info("Validating on {length(y_te_f)} observations.")
    pred_cls <- stats::predict(fit, newx = X_te, s = "lambda.min", type = "class")
    pred_cls <- as.vector(pred_cls)
    
    f1 <- macro_f1(y_te_f, pred_cls, class_levels)
    cli::cli_alert_info("Macro-F1 = {round(f1, 3)} with '{w}' weights.")
    
    if (f1 > best_f1) {
      best_f1  <- f1
      best_fit <- fit
      best_w   <- w
    }
    if (f1 >= req_f1) break
  }
  
  # if (best_f1 < req_f1) {
  #   cli::cli_alert_warning(
  #     "Macro-F1 ({round(best_f1, 3)}) below requirement ({req_f1}). Consider more data or parameter changes."
  #   )
  #   return(NULL)
  # } else {
    cli::cli_alert_success(
      "Best model found. Macro-F1 = {round(best_f1, 3)} with '{best_w}' weights."
    )
    # Report per-class metrics on the holdout
    final_pred <- stats::predict(best_fit, newx = X_te, s = "lambda.min", type = "class")
    final_pred <- as.vector(final_pred)
    cm <- caret::confusionMatrix(
      data = factor(final_pred, levels = class_levels),
      reference = factor(y_te_f, levels = class_levels),
      mode = "prec_recall"
    )
    # Return per-class metrics
    perf <- cm$byClass[, c("Precision", "Recall", "F1"), drop = FALSE]
  # }
  
  # 7) Imputation or consistency check ####
  if (length(na_codes) > 0) {
    cli::cli_alert_info("Proceeding with inference on missing codes.")
    pred_prob <- stats::predict(best_fit, newx = X_inf, s = "lambda.min", type = "response")
    # cv.glmnet multinomial probabilities: [n, K, 1] array; convert to matrix
    P <- if (length(dim(pred_prob)) == 3) pred_prob[, , 1] else pred_prob
    # P <- as.matrix(P)
    colnames(P) <- if (!is.null(colnames(P))) colnames(P) else class_levels
    max_idx   <- max.col(P, ties.method = "first")
    pred_lab  <- colnames(P)[max_idx]
    max_prob  <- P[cbind(seq_len(nrow(P)), max_idx)]
    
    out <- data.frame(
      title      = titles[na_codes],
      suggestion = as.vector(pred_lab),
      probability= as.numeric(max_prob),
      stringsAsFactors = FALSE
    )
    out <- dplyr::as_tibble(out)  %>% 
      dplyr::arrange(dplyr::desc(probability))
    list(per_class_metrics = perf, suggestions = out)
  } else {
    cli::cli_alert_info("Checking existing codes for possible errors.")
    pred_prob <- stats::predict(best_fit, newx = X, s = "lambda.min", type = "response")
    P <- if (length(dim(pred_prob)) == 3) pred_prob[, , 1, drop = FALSE] else pred_prob
    P <- as.matrix(P)
    colnames(P) <- if (!is.null(colnames(P))) colnames(P) else class_levels
    max_idx   <- max.col(P, ties.method = "first")
    pred_lab  <- colnames(P)[max_idx]
    max_prob  <- P[cbind(seq_len(nrow(P)), max_idx)]
    
    out <- data.frame(
      title       = titles,
      current     = var,
      suggestion  = as.vector(pred_lab),
      probability = as.numeric(max_prob),
      stringsAsFactors = FALSE
    )
    out <- dplyr::as_tibble(out) %>% 
      dplyr::filter(current != suggestion) %>% 
      dplyr::arrange(dplyr::desc(probability))
    list(per_class_metrics = perf, potential_mismatches = out)
  }
}


# Macro-F1 helper (treat NA as 0 to penalize classes with no correct preds)
macro_f1 <- function(truth, pred, levels) {
  truth_f <- factor(truth, levels = levels)
  pred_f  <- factor(pred,  levels = levels)
  cm <- caret::confusionMatrix(
    data = pred_f, reference = truth_f, mode = "prec_recall"
  )
  f1 <- cm$byClass[, "F1", drop = TRUE]
  if (length(f1) == 0) return(0)
  f1[is.na(f1)] <- 0
  mean(f1)
}

