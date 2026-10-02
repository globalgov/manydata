#' Extending codes
#' @description
#'   These functions use text embeddings and multinomial logistic regression
#'   to suggest missing codes or flag potentially incorrect codes based on text data.
#'   They require a vector of text (e.g., titles or descriptions)
#'   and a corresponding vector of categorical codes, with `NA` or empty strings
#'   indicating missing codes to be inferred.
#'   The functions train a multinomial logistic regression model
#'   using `glmnet` on the text embeddings of the entries with known codes,
#'   and then predict codes for the entries with missing codes.
#'   The functions also validate the model's performance
#'   on a holdout set and report per-class precision, recall, and F1-score.
#'   If no missing codes are present, the functions instead
#'   check existing codes for potential mismatches and report them.
#'
#'   `code_extend()` is the main function,
#'   and the `embedding` argument chooses how the text is embedded.
#'   `code_extend_dfm()` and `code_extend_bert()` can also be called directly.
#' @section DFM:
#'   `embedding = "dfm"` (the default) or `code_extend_dfm()`
#'   represents each text by its row in a document-feature matrix
#'   with tf-idf weights, constructed using the `{quanteda}` package.
#'   The matrix is built from the input text only,
#'   so this option is quick and requires no other input.
#'   It works well for short texts such as titles,
#'   where the presence of particular words is informative about the code.
#' @section BERT:
#'   `embedding = "bert"` or `code_extend_bert()`
#'   uses pre-trained sentence embeddings (from the BERT family of models),
#'   as returned by `text::textEmbed()` and passed to the `emb_texts` argument.
#'   These embeddings draw on knowledge from outside the input text,
#'   and so can recognise similar meanings expressed in different words,
#'   but they must be computed by the user beforehand,
#'   which requires the `{text}` package and a Python installation.
#' @section GloVe:
#'   `embedding = "glove"` or `code_extend_glove()`
#'   trained GloVe word embeddings on the input text
#'   using the `{text2vec}` package.
#'   This option is no longer available since v1.2.0,
#'   because `{text2vec}` depends on packages scheduled for archival from CRAN.
#'   GloVe embeddings also need a large corpus to train on,
#'   and on short texts such as titles
#'   they performed less well than the DFM embedding in our tests
#'   (a mean macro-F1 of 0.68 against 0.79 on the `emperors` example).
#'   Please use `embedding = "dfm"` instead.
#' @name code_extend
#' @importFrom caret confusionMatrix createDataPartition
#' @importFrom glmnet cv.glmnet
#' @param titles A character vector of text entries (e.g., titles or descriptions).
#' @param var A character vector of (categorical) codes that might be coded
#'   from the titles or texts.
#'   Entries with missing codes should be `NA_character_` or empty strings.
#'   The function will suggest codes for these entries.
#'   If no missing codes are present, the function will check existing codes
#'   for potential mismatches.
#' @param embedding How the text should be embedded before training the model.
#'   One of "dfm" (default), "bert", or "glove" (no longer available).
#'   See the sections below for more details.
#' @param req_f1 The required macro-F1 score on the validation set
#'   before proceeding with inference.
#'   Default is 0.80.
#' @param rarity_threshold Minimum number of occurrences for a code
#'   to be included in training.
#'   Codes with fewer occurrences are excluded from training
#'   to ensure sufficient data for learning.
#'   Default is 8.
#' @param emb_texts For `embedding = "bert"` or `code_extend_bert()`,
#'   pre-computed embeddings from `text::textEmbed()`.
#'   This avoids re-computing embeddings if they have already been computed.
#'   Any model can be used to compute them,
#'   e.g. "sentence-transformers/all-MiniLM-L6-v2",
#'   but it should produce sentence-level embeddings.
#' @return A list with the per-class precision, recall, and F1-score
#'   on the validation set (`per_class_metrics`),
#'   and a tibble of either the suggested codes for entries with missing codes
#'   (`suggestions`) or, if there are no missing codes,
#'   the entries where the current code differs from the model's suggestion
#'   (`potential_mismatches`).
#'   With the "dfm" embedding, `NULL` is returned if no model reaches
#'   the required macro-F1 score.
#' @examplesIf requireNamespace("quanteda", quietly = TRUE)
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
#' code_extend(titles, var, embedding = "dfm", req_f1 = 0.6)
#' @export
code_extend <- function(titles, var,
                        embedding = c("dfm", "bert", "glove"),
                        req_f1 = 0.80,
                        rarity_threshold = 8,
                        emb_texts){
  embedding <- match.arg(embedding)
  switch(embedding,
         dfm = code_extend_dfm(titles, var, req_f1 = req_f1,
                               rarity_threshold = rarity_threshold),
         bert = code_extend_bert(titles, var, req_f1 = req_f1,
                                 rarity_threshold = rarity_threshold,
                                 emb_texts = emb_texts),
         glove = code_extend_glove(titles, var, req_f1 = req_f1,
                                   rarity_threshold = rarity_threshold))
}

#' @rdname code_extend
#' @export
code_extend_dfm <- function(titles, var,
                            req_f1 = 0.80,
                            rarity_threshold = 8){
  if (length(titles) != length(var)) stop("titles and var must be the same length.")
  if (!is.character(titles)) stop("titles must be a character vector.")
  thisRequires("quanteda")

  # Document-feature matrix of the full corpus, weighted by tf-idf
  toks <- quanteda::tokens(titles, remove_punct = TRUE)
  X <- quanteda::dfm_tfidf(quanteda::dfm(toks))
  X <- methods::as(X, "dgCMatrix")

  code_extend_fit(X, titles, var, req_f1 = req_f1,
                  rarity_threshold = rarity_threshold, enforce_f1 = TRUE)
}

#' @rdname code_extend
#' @export
code_extend_bert <- function(
    titles,
    var,
    req_f1 = 0.80,
    rarity_threshold = 8,
    emb_texts) {
  if (length(titles) != length(var)) stop("titles and var must be the same length.")
  if (!is.character(titles)) stop("titles must be a character vector.")
  if(missing(emb_texts)){
    stop("Please provide pre-computed embeddings from text::textEmbed() via the emb_texts argument.")
  }

  # Contextual sentence embeddings (BERT family via 'text')
  X <- as.matrix(emb_texts$texts)
  if (anyNA(X)) {
    cli::cli_alert_warning("Embeddings contain NA; replacing with 0.")
    X[is.na(X)] <- 0
  }

  code_extend_fit(X, titles, var, req_f1 = req_f1,
                  rarity_threshold = rarity_threshold, enforce_f1 = FALSE)
}

# Trains and validates a multinomial model on the embedded texts (X),
# and then uses it to infer missing codes or check existing codes
code_extend_fit <- function(X, titles, var, req_f1, rarity_threshold,
                            enforce_f1) {

  # 1) Split into training vs inference set based on missing codes ####
  na_codes <- which(is.na(var) | var == "")
  if (length(na_codes) > 0) {
    cli::cli_alert_info("Found {length(na_codes)} missing codes to infer.")
    X_inf   <- X[na_codes, , drop = FALSE]
    X_train <- X[-na_codes, , drop = FALSE]
    y_train <- var[-na_codes]
  } else {
    cli::cli_alert_success("No additional coding required. Training for validation.")
    X_train <- X
    y_train <- var
  }

  # 2) Remove rare codes (based on training only) ####
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

  # 3) Stratified split for validation ####
  idx <- caret::createDataPartition(y = y_train, p = 0.75, list = FALSE)[, 1]
  X_tr <- X_train[idx, , drop = FALSE]
  X_te <- X_train[-idx, , drop = FALSE]
  y_tr <- y_train[idx]
  y_te <- y_train[-idx]

  # Ensure consistent factor levels
  class_levels <- sort(unique(y_tr))
  y_tr_f <- factor(y_tr, levels = class_levels)
  y_te_f <- factor(y_te, levels = class_levels)

  # 4) Observation weights for imbalance ####
  counts <- table(y_tr_f)
  obs_weights <- list(
    logscaled = as.vector((1 / log1p(counts))[y_tr_f]),
    smoothed  = as.vector((1 / (counts + 5))[y_tr_f]),
    inverse   = as.vector((1 / counts)[y_tr_f]),
    no        = as.vector((counts / length(y_tr_f))[y_tr_f])
  )

  # 5) Train with different weighting schemes; early-stop on macro-F1 ####
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

  if (enforce_f1 && best_f1 < req_f1) {
    cli::cli_alert_warning(
      "Macro-F1 ({round(best_f1, 3)}) below requirement ({req_f1}). Consider more data or parameter changes."
    )
    return(NULL)
  }
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
  perf <- cm$byClass[, c("Precision", "Recall", "F1"), drop = FALSE]

  # 6) Imputation or consistency check ####
  if (length(na_codes) > 0) {
    cli::cli_alert_info("Proceeding with inference on missing codes.")
    P <- prob_matrix(stats::predict(best_fit, newx = X_inf, s = "lambda.min",
                                    type = "response"), class_levels)
    max_idx <- max.col(P, ties.method = "first")

    out <- data.frame(
      title       = titles[na_codes],
      suggestion  = colnames(P)[max_idx],
      probability = as.numeric(P[cbind(seq_len(nrow(P)), max_idx)]),
      stringsAsFactors = FALSE
    )
    out <- dplyr::as_tibble(out) |>
      dplyr::arrange(dplyr::desc(probability))
    cli::cli_alert_success("Predicted {length(na_codes)} missing codes.")
    list(per_class_metrics = perf, suggestions = out)
  } else {
    cli::cli_alert_info("Checking existing codes for possible errors.")
    P <- prob_matrix(stats::predict(best_fit, newx = X, s = "lambda.min",
                                    type = "response"), class_levels)
    max_idx <- max.col(P, ties.method = "first")

    out <- data.frame(
      title       = titles,
      current     = var,
      suggestion  = colnames(P)[max_idx],
      probability = as.numeric(P[cbind(seq_len(nrow(P)), max_idx)]),
      stringsAsFactors = FALSE
    )
    out <- dplyr::as_tibble(out) |>
      dplyr::filter(current != suggestion) |>
      dplyr::arrange(dplyr::desc(probability))
    cli::cli_alert_success("Found {nrow(out)} unlikely codes.")
    list(per_class_metrics = perf, potential_mismatches = out)
  }
}

# cv.glmnet multinomial probabilities are an [n, K, 1] array;
# this converts them to an [n, K] matrix, also where n is 1
prob_matrix <- function(pred_prob, class_levels) {
  if (length(dim(pred_prob)) == 3) {
    P <- matrix(pred_prob, nrow = dim(pred_prob)[1], ncol = dim(pred_prob)[2],
                dimnames = dimnames(pred_prob)[1:2])
  } else {
    P <- as.matrix(pred_prob)
  }
  if (is.null(colnames(P))) colnames(P) <- class_levels
  P
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
