#===============================================================================
# READABILILTY, SENTIMENT VS. CITATIONS 
#===============================================================================
# Create individual plots
p1 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = flesch, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Flesch Score", x = "Flesch Score", y = "Citations") +
  theme_minimal()

p2 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = flesch_kincaid, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Flesch-Kincaid", x = "Flesch-Kincaid Score", y = "Citations") +
  theme_minimal()

p3 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = dale_chall, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Dale-Chall", x = "Dale-Chall Score", y = "Citations") +
  theme_minimal()

p4 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = pos_sentiment, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Positive Sentiment", x = "Positive Sentiment", y = "Citations") +
  theme_minimal()

p5 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = is_tentative, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Tentative Language", x = "Is Tentative", y = "Citations") +
  theme_minimal()

p6 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = is_certain, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Certain Language", x = "Is Certain", y = "Citations") +
  theme_minimal()

# Arrange all plots in a grid (3x2 layout)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)



#===============================================================================
# PAPER-METHODS VS. CITATIONS GRID ARRANGEMENT
#===============================================================================

# Create individual plots for each paper method
p1 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_RDD, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs RDD Method", x = "RDD Method", y = "Citations") +
  theme_minimal()

p2 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_DID, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs DID Method", x = "DID Method", y = "Citations") +
  theme_minimal()

p3 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_RCT, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs RCT Method", x = "RCT Method", y = "Citations") +
  theme_minimal()

p4 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_IV, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs IV Method", x = "IV Method", y = "Citations") +
  theme_minimal()

p5 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_Structural, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Structural Method", x = "Structural Method", y = "Citations") +
  theme_minimal()

p6 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_TWFE, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs TWFE Method", x = "TWFE Method", y = "Citations") +
  theme_minimal()

p7 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_Event, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Event Study", x = "Event Study Method", y = "Citations") +
  theme_minimal()

p8 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_Simulations, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Simulations", x = "Simulations Method", y = "Citations") +
  theme_minimal()

p9 <- ggplot(data = causal_claims_with_citations) + 
  geom_point(mapping = aes(x = paper_method_Theoretical, y = citations_asinh, color = authors_institutions)) +
  labs(title = "Citations vs Theoretical", x = "Theoretical Method", y = "Citations") +
  theme_minimal()

# Arrange all plots in a 3x3 grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3)

#===============================================================================
# DESCRIPTIVE STATISTICS
#===============================================================================


# Select and rename variables based on your screenshot
selected_data <- causal_claims_with_citations %>%
  dplyr::select(
    year, 
    citations_asinh,
    citations,
    flesch,
    flesch_kincaid,
    dale_chall,
    pos_words,
    neg_words,
    pos_sentiment,
    is_tentative,
    is_certain,
    classification_of_paper,
    paper_repo,
    is_evidence_provided_in_paper,
    level_of_tentativeness,
    # Economic field indicators
    is_finance,
    is_development,
    is_labour,
    is_public_economics,
    is_urban_economics,
    is_macroeconomics,
    is_behavioral_economics,
    is_economic_history,
    is_econometric_theory,
    is_industrial_organization,
    is_environmental_economics,
    is_health_economics,
    # Methods variables
    paper_method_RDD,
    paper_method_DID,
    paper_method_RCT,
    paper_method_IV,
    paper_method_Structural,
    paper_method_TWFE,
    paper_method_Event,
    paper_method_Simulations,
    paper_method_Theoretical,
    # Other paper characteristics
    paper_year_meta,
    edge_number
  ) %>%
  na.omit() %>%  # Remove rows with any NA values
  dplyr::rename(
    "Publication Year" = year,
    "Citations (asinh)" = citations_asinh,
    "Citations" = citations,
    "Flesch Score" = flesch,
    "Flesch-Kincaid Score" = flesch_kincaid,
    "Dale-Chall Score" = dale_chall,
    "Positive Words" = pos_words,
    "Negative Words" = neg_words,
    "Positive Sentiment" = pos_sentiment,
    "Tentative Language" = is_tentative,
    "Certain Language" = is_certain,
    "Paper Classification" = classification_of_paper,
    "Paper Repository" = paper_repo,
    "Evidence Provided" = is_evidence_provided_in_paper,
    "Level of Tentativeness" = level_of_tentativeness,
    # Economic fields
    "Finance" = is_finance,
    "Development" = is_development,
    "Labour" = is_labour,
    "Public Economics" = is_public_economics,
    "Urban Economics" = is_urban_economics,
    "Macroeconomics" = is_macroeconomics,
    "Behavioral Economics" = is_behavioral_economics,
    "Economic History" = is_economic_history,
    "Econometric Theory" = is_econometric_theory,
    "Industrial Organization" = is_industrial_organization,
    "Environmental Economics" = is_environmental_economics,
    "Health Economics" = is_health_economics,
    # Methods
    "RDD Method" = paper_method_RDD,
    "DID Method" = paper_method_DID,
    "RCT Method" = paper_method_RCT,
    "IV Method" = paper_method_IV,
    "Structural Method" = paper_method_Structural,
    "TWFE Method" = paper_method_TWFE,
    "Event Study Method" = paper_method_Event,
    "Simulations Method" = paper_method_Simulations,
    "Theoretical Method" = paper_method_Theoretical,
    # Other characteristics
    "Paper Year (Meta)" = paper_year_meta,
    "Edge Number" = edge_number
  )

# Create descriptive statistics table
datasummary(All(selected_data) ~ (`Observations` = N) + Mean + SD + Min + Median + Max,
            data = selected_data,
            title = "Descriptive Statistics of Main Variables",
            fmt = 2,
            notes = "This table shows the descriptive statistics for all variables used in the citation analysis. Binary variables represent proportions.")

# Alternative: Create separate tables for different variable types
# Readability and Sentiment Variables
readability_sentiment <- selected_data %>%
  select(`Flesch Score`, `Flesch-Kincaid Score`, `Dale-Chall Score`, 
         `Positive Words`, `Negative Words`, `Positive Sentiment`,
         `Tentative Language`, `Certain Language`)

datasummary(All(readability_sentiment) ~ (`Observations` = N) + Mean + SD + Min + Median + Max,
            data = readability_sentiment,
            title = "Descriptive Statistics: Readability and Sentiment Variables",
            fmt = 2)

# Citation and Paper Characteristics
citation_characteristics <- selected_data %>%
  select(`Citations`, `Citations (asinh)`, `Publication Year`, 
         `Paper Year (Meta)`, `Edge Number`)

datasummary(All(citation_characteristics) ~ (`Observations` = N) + Mean + SD + Min + Median + Max,
            data = citation_characteristics,
            title = "Descriptive Statistics: Citation and Paper Characteristics",
            fmt = 2)

# Economic Field Indicators (Binary Variables)
economic_fields <- selected_data %>%
  select(Finance, Development, Labour, `Public Economics`, `Urban Economics`,
         Macroeconomics, `Behavioral Economics`, `Economic History`,
         `Econometric Theory`, `Industrial Organization`, 
         `Environmental Economics`, `Health Economics`)

datasummary(All(economic_fields) ~ (`Observations` = N) + Mean + SD + Min + Max,
            data = economic_fields,
            title = "Descriptive Statistics: Economic Field Indicators",
            fmt = 3,
            notes = "Values represent proportions for binary field indicators.")

# Econometric Methods (Binary Variables)
methods <- selected_data %>%
  select(`RDD Method`, `DID Method`, `RCT Method`, `IV Method`,
         `Structural Method`, `TWFE Method`, `Event Study Method`,
         `Simulations Method`, `Theoretical Method`)

datasummary(All(methods) ~ (`Observations` = N) + Mean + SD + Min + Max,
            data = methods,
            title = "Descriptive Statistics: Econometric Methods",
            fmt = 3,
            notes = "Values represent proportions for binary method indicators.")

# Alternatively
datasummary_skim(causal_claims_with_citations, output = "default", histogram=T)
