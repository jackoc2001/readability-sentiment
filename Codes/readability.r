#===============================================================================
# Load packages
#===============================================================================

setwd("~/Desktop/Project")
causal_claims <- read_csv("~/Desktop/Project/causal_claims_beta.csv")
names(causal_claims)

#===============================================================================
# READABILITY MEASURES (requires quanteda.textstats)
#===============================================================================
text_column <- "paper_abstract_meta"
corp <- corpus(causal_claims[[text_column]])
causal_claims$flesch <- textstat_readability(corp, measure = "Flesch")$Flesch
causal_claims$flesch_kincaid <- textstat_readability(corp, measure = "Flesch.Kincaid")$Flesch.Kincaid
causal_claims$dale_chall <- textstat_readability(corp, measure = "Dale.Chall")$Dale.Chall

#===============================================================================
# CHECK FOR ECONOMETRIC WORDS
#===============================================================================

data(DictionaryGI)


text_column <- "paper_abstract_meta"

if (!text_column %in% names(causal_claims)) {
  print("Available columns in your dataset:")
  print(names(causal_claims))
  stop(paste("Column", text_column, "not found. Please update 'text_column' variable with the correct column name."))
}

toks_clean <- tokens_tolower(toks)
toks_clean <- tokens_remove(toks_clean, stopwords('en'))

print(paste("Corpus created with", ndoc(corp), "documents"))
print(paste("Average tokens per document after preprocessing:", mean(ntoken(toks_clean))))

head(positive_words, 20)
head(negative_words, 20)

# Check if specific words are in the dictionary
words_to_check <- c("average", "limit", "regression", "subtract", "ordinary", 
                    "aggregate", "natural", "validity", "append", "value")

print("Words found in positive dictionary:")
words_to_check[words_to_check %in% positive_words]

print("Words found in negative dictionary:")
words_to_check[words_to_check %in% negative_words]

# Or check both at once
print("Words found in either dictionary:")
words_to_check[words_to_check %in% c(positive_words, negative_words)]

# Remove econometrics words
words_to_exclude <- c("limit", "regression", "subtract", "aggregate", 
                      "natural", "validity", "append", "value")

# Remove excluded words from positive and negative word lists
positive_words <- positive_words[!positive_words %in% words_to_exclude]
negative_words <- negative_words[!negative_words %in% words_to_exclude]

# Print how many words were removed
print(paste("Words removed from positive dictionary:", 
            sum(words_to_exclude %in% positive_words)))
print(paste("Words removed from negative dictionary:", 
            sum(words_to_exclude %in% negative_words)))




#===============================================================================
# CALCULATE POSITIVE/NEGATIVE SENTIMENT
#===============================================================================
data(DictionaryGI)
# Extract positive and negative words from DictionaryGI
# Handle different possible structures of the dictionary
if (is.data.frame(DictionaryGI)) {
  if ("word" %in% names(DictionaryGI) && "positive" %in% names(DictionaryGI)) {
    positive_words <- DictionaryGI[DictionaryGI$positive != 0, "word"]
    negative_words <- DictionaryGI[DictionaryGI$negative != 0, "word"]
  } else if (any(grepl("pos", names(DictionaryGI), ignore.case = TRUE))) {
    pos_col <- names(DictionaryGI)[grepl("pos", names(DictionaryGI), ignore.case = TRUE)][1]
    neg_col <- names(DictionaryGI)[grepl("neg", names(DictionaryGI), ignore.case = TRUE)][1]
    word_col <- names(DictionaryGI)[1]
    
    positive_words <- DictionaryGI[DictionaryGI[[pos_col]] != 0, word_col]
    negative_words <- DictionaryGI[DictionaryGI[[neg_col]] != 0, word_col]
  }
} else if (is.list(DictionaryGI)) {
  positive_words <- DictionaryGI$positive
  negative_words <- DictionaryGI$negative
} else {
  # Fallback: use SentimentAnalysis built-in dictionaries
  print("Using SentimentAnalysis built-in dictionaries...")
  dict_gi <- loadDictionaryGI()
  positive_words <- dict_gi$positive
  negative_words <- dict_gi$negative
}


positive_words <- positive_words[!is.na(positive_words)]
negative_words <- negative_words[!is.na(negative_words)]

# Words to exclude from sentiment analysis
words_to_exclude <- c("limit", "regression", "subtract", "aggregate", 
                      "natural", "validity", "append", "value")

# Remove excluded words from positive and negative word lists
positive_words <- positive_words[!positive_words %in% words_to_exclude]
negative_words <- negative_words[!negative_words %in% words_to_exclude]
# Create the Harvard IV dictionary for quanteda
harvard_iv_dict <- dictionary(list(
  positive = positive_words,
  negative = negative_words
))

# Apply Harvard IV dictionary
harvard_scores <- dfm(tokens_lookup(toks_clean, dictionary = harvard_iv_dict))

# Extract Harvard IV positive/negative counts
causal_claims$pos_words <- as.numeric(harvard_scores[, "positive"])
causal_claims$neg_words <- as.numeric(harvard_scores[, "negative"])

# Calculate Harvard IV sentiment ratio
causal_claims$pos_sentiment <- ifelse(
  (causal_claims$pos_words + causal_claims$neg_words) > 0,
  (causal_claims$pos_words - causal_claims$neg_words) / 
    (causal_claims$pos_words + causal_claims$neg_words),
  0
)
