titles <- paste(emperors$Wikipedia$CityBirth,
                emperors$Wikipedia$ProvinceBirth,
                emperors$Wikipedia$Rise,
                emperors$Wikipedia$Dynasty,
                emperors$Wikipedia$Cause)
var <- emperors$Wikipedia$Killer
var[var %in% c("Senate", "Court Officials", "Opposing Army")] <- "Enemies"
var[var %in% c("Fire", "Lightning", "Aneurism", "Heart Failure")] <- "God"
var[var %in% c("Wife", "Usurper", "Praetorian Guard", "Own Army")] <- "Friends"
var[is.na(var)] <- "Unknown"
var_full <- var
var[var == "Unknown"] <- NA

# Embeddings in the format returned by text::textEmbed()
set.seed(123)
codes <- rep(c("A", "B", "C"), each = 40)
emb <- matrix(rnorm(120 * 20), 120, 20)
signal <- cbind(seq_along(codes), match(codes, c("A", "B", "C")))
emb[signal] <- emb[signal] + 3
emb <- list(texts = as.data.frame(emb))
texts <- paste("text", seq_along(codes))
codes_na <- codes
codes_na[c(5, 50, 100, 110)] <- NA

quietly <- function(x) suppressWarnings(suppressMessages(x))

test_that("code_extend_dfm() suggests missing codes", {
  skip_if_not_installed("quanteda")
  set.seed(123)
  out <- quietly(code_extend_dfm(titles, var, req_f1 = 0))
  expect_named(out, c("per_class_metrics", "suggestions"))
  expect_equal(colnames(out$per_class_metrics), c("Precision", "Recall", "F1"))
  expect_s3_class(out$suggestions, "tbl_df")
  expect_named(out$suggestions, c("title", "suggestion", "probability"))
  expect_equal(nrow(out$suggestions), sum(is.na(var)))
  expect_true(all(out$suggestions$suggestion %in% var))
  expect_true(all(out$suggestions$probability >= 0 &
                    out$suggestions$probability <= 1))
})

test_that("code_extend_dfm() checks existing codes", {
  skip_if_not_installed("quanteda")
  set.seed(123)
  out <- quietly(code_extend_dfm(titles, var_full, req_f1 = 0))
  expect_named(out, c("per_class_metrics", "potential_mismatches"))
  expect_named(out$potential_mismatches,
               c("title", "current", "suggestion", "probability"))
  expect_true(all(out$potential_mismatches$current !=
                    out$potential_mismatches$suggestion))
})

test_that("code_extend_dfm() requires a sufficient model", {
  skip_if_not_installed("quanteda")
  expect_null(quietly(code_extend_dfm(titles, var, req_f1 = 1.01)))
  expect_error(code_extend_dfm(titles, var[-1]), "same length")
})

test_that("code_extend_bert() suggests missing codes and checks existing codes", {
  expect_error(code_extend_bert(texts, codes_na), "emb_texts")
  set.seed(123)
  out <- quietly(code_extend_bert(texts, codes_na, emb_texts = emb))
  expect_named(out, c("per_class_metrics", "suggestions"))
  expect_equal(nrow(out$suggestions), 4)
  expect_true(all(out$suggestions$suggestion %in% codes))
  set.seed(123)
  out <- quietly(code_extend_bert(texts, codes, emb_texts = emb))
  expect_named(out, c("per_class_metrics", "potential_mismatches"))
  expect_true(all(out$potential_mismatches$suggestion %in% codes))
})

test_that("code_extend() dispatches on embedding", {
  set.seed(123)
  out1 <- quietly(code_extend(texts, codes_na, embedding = "bert",
                              emb_texts = emb))
  set.seed(123)
  out2 <- quietly(code_extend_bert(texts, codes_na, emb_texts = emb))
  expect_equal(out1, out2)
  expect_error(code_extend(texts, codes_na, embedding = "other"))
  skip_if_not_installed("quanteda")
  set.seed(123)
  out1 <- quietly(code_extend(titles, var, req_f1 = 0))
  set.seed(123)
  out2 <- quietly(code_extend_dfm(titles, var, req_f1 = 0))
  expect_equal(out1, out2)
})

test_that("code_extend_glove() is defunct", {
  expect_error(code_extend_glove(titles, var), class = "defunctError")
  expect_error(code_extend(titles, var, embedding = "glove"),
               class = "defunctError")
})
