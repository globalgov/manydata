# Extending codes

These functions use text embeddings and multinomial logistic regression
to suggest missing codes or flag potentially incorrect codes based on
text data. Two approaches are provided: one using GloVe embeddings
trained on the input text, and another using pre-trained BERT embeddings
via the `{text}` package. Both functions require a vector of text (e.g.,
titles or descriptions) and a corresponding vector of categorical codes,
with `NA` or empty strings indicating missing codes to be inferred. The
functions train a multinomial logistic regression model using `glmnet`
on the text embeddings of the entries with known codes, and then predict
codes for the entries with missing codes. The functions also validate
the model's performance on a holdout set and report per-class precision,
recall, and F1-score. If no missing codes are present, the functions
instead check existing codes for potential mismatches and report them.
Note that `code_extend_glove()` requires the `{text2vec}` package to be
installed.

## Usage

``` r
code_extend_glove(titles, var, req_f1 = 0.8, rarity_threshold = 8)

code_extend_bert(titles, var, req_f1 = 0.8, rarity_threshold = 8, emb_texts)
```

## Arguments

- titles:

  A character vector of text entries (e.g., titles or descriptions).

- var:

  A character vector of (categorical) codes that might be coded from the
  titles or texts. Entries with missing codes should be `NA_character_`
  or empty strings. The function will suggest codes for these entries.
  If no missing codes are present, the function will check existing
  codes for potential mismatches.

- req_f1:

  The required macro-F1 score on the validation set before proceeding
  with inference. Default is 0.80.

- rarity_threshold:

  Minimum number of occurrences for a code to be included in training.
  Codes with fewer occurrences are excluded from training to ensure
  sufficient data for learning. Default is 8.

- emb_texts:

  For `code_extend_bert()`, pre-computed embeddings from
  `text::textEmbed()`. This avoids re-computing embeddings if they have
  already been computed. A Hugging Face model can be specified via the
  `model` argument. Default is "sentence-transformers/all-MiniLM-L6-v2".
  Other models can be used, but they should produce sentence-level
  embeddings.

## Examples

``` r
titles <- paste(emperors$Wikipedia$CityBirth,
                emperors$Wikipedia$ProvinceBirth,
                emperors$Wikipedia$Rise,
                emperors$Wikipedia$Dynasty,
                emperors$Wikipedia$Cause)
var <- emperors$Wikipedia$Killer
var[var=="Unknown"] <- NA
var[var %in% c("Senate","Court Officials","Opposing Army")] <- "Enemies"
var[var %in% c("Fire","Lightning","Aneurism","Heart Failure")] <- "God"
var[var %in% c("Wife","Usurper","Praetorian Guard","Own Army")] <- "Friends"
glo <- code_extend_glove(titles, 
           var)
#> ℹ Training GloVe model.
#> INFO  [08:45:52.157] epoch 1, loss 0.3591
#> INFO  [08:45:52.175] epoch 2, loss 0.1879
#> INFO  [08:45:52.179] epoch 3, loss 0.0690
#> INFO  [08:45:52.179] epoch 4, loss 0.0318
#> INFO  [08:45:52.180] epoch 5, loss 0.0213
#> INFO  [08:45:52.181] epoch 6, loss 0.0149
#> INFO  [08:45:52.182] epoch 7, loss 0.0108
#> INFO  [08:45:52.182] epoch 8, loss 0.0078
#> INFO  [08:45:52.183] epoch 9, loss 0.0058
#> INFO  [08:45:52.184] epoch 10, loss 0.0044
#> INFO  [08:45:52.184] epoch 11, loss 0.0033
#> INFO  [08:45:52.185] epoch 12, loss 0.0026
#> INFO  [08:45:52.186] epoch 13, loss 0.0020
#> INFO  [08:45:52.186] epoch 14, loss 0.0016
#> INFO  [08:45:52.187] epoch 15, loss 0.0013
#> INFO  [08:45:52.188] epoch 16, loss 0.0010
#> INFO  [08:45:52.188] epoch 17, loss 0.0008
#> INFO  [08:45:52.189] epoch 18, loss 0.0006
#> INFO  [08:45:52.190] epoch 19, loss 0.0005
#> INFO  [08:45:52.191] epoch 20, loss 0.0004
#> ℹ Found 6 missing codes to infer.
#> ℹ Removing God codes from training data as they have fewer than 8 occurrences.
#> ℹ Training glmnet with 'logscaled' weights.
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> ✔ Model trained with 'logscaled' weights.
#> ℹ Validating on 13 observations.
#> ℹ Macro-F1 = 0.525 with 'logscaled' weights.
#> ℹ Training glmnet with 'smoothed' weights.
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> ✔ Model trained with 'smoothed' weights.
#> ℹ Validating on 13 observations.
#> ℹ Macro-F1 = 0.525 with 'smoothed' weights.
#> ℹ Training glmnet with 'inverse' weights.
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> ✔ Model trained with 'inverse' weights.
#> ℹ Validating on 13 observations.
#> ℹ Macro-F1 = 0.525 with 'inverse' weights.
#> ℹ Training glmnet with 'no' weights.
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
#> ✔ Model trained with 'no' weights.
#> ℹ Validating on 13 observations.
#> ℹ Macro-F1 = 0.561 with 'no' weights.
#> ! Macro-F1 (0.561) below requirement (0.8). Consider more data or parameter changes.
```
