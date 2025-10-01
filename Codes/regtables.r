asinh_ols_citations_1 <- feols(citations_asinh ~ flesch + classification_of_paper
                               | year + jel_codes,
                               data = causal_claims_with_citations)

asinh_ols_citations_2 <- feols(citations_asinh ~ flesch_kincaid + classification_of_paper
                               | year + jel_codes, 
                               data = causal_claims_with_citations)

asinh_ols_citations_3 <- feols(citations_asinh ~ dale_chall + classification_of_paper
                               | year + jel_codes,
                               data = causal_claims_with_citations)

asinh_ols_citations_4 <- feols(citations_asinh ~ is_tentative + classification_of_paper
                               | year + jel_codes,
                               data = causal_claims_with_citations)

asinh_ols_citations_5 <- feols(citations_asinh ~ is_certain + classification_of_paper
                               | year + jel_codes, 
                               data = causal_claims_with_citations)

# Create a list of models for easy handling
models <- list(
  "Flesch" = asinh_ols_citations_1,
  "Flesch-Kincaid" = asinh_ols_citations_2,
  "Dale-Chall" = asinh_ols_citations_3,
  "Tentative" = asinh_ols_citations_4,
  "Certain" = asinh_ols_citations_5
)


# Basic side-by-side table
modelsummary(models,
             title = "Readability and Language Effects on Citations (asinh)",
             stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE")

# More customized table
modelsummary(models,
             title = "Effects of Readability and Language Certainty on Citations",
             coef_rename = c("flesch" = "Flesch Score",
                             "flesch_kincaid" = "Flesch-Kincaid Score", 
                             "dale_chall" = "Dale-Chall Score",
                             "is_tentative" = "Tentative Language",
                             "is_certain" = "Certain Language",
                             "classification_of_paper" = "Paper Classification"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = "Fixed effects: Year and JEL codes. Standard errors in parentheses.",
             gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Std.Errors")
