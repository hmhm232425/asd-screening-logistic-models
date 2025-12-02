# Research


# Abstract

This study examined whether parental ASD family history, sex, ethnicity,
age, and neonatal jaundice are associated with ASD-like behavioral
traits in 1,054 toddlers from the Q-Chat-10 dataset. Logistic regression
models were used to evaluate four questions: demographic moderation of
family history effects, item-level associations, the interaction between
family history and age, and the interaction between family history and
jaundice. Sex showed a consistent association, with boys displaying
higher odds of ASD-like traits, and ethnicity showed a modest effect,
with children in the “Other” category having lower odds. Jaundice was
also modestly associated with increased ASD-like traits. In contrast,
parental ASD family history showed weak or non-significant associations
in most models, likely due to measurement limitations in the dataset,
including a screening-based outcome and a coarse family history
variable. Overall, demographic and developmental factors demonstrated
clearer associations than parental history, underscoring the need for
more precise familial and clinical information in future research.

# 1. Introduction

## 1.1. Background and Motivation

Autism Spectrum Disorder (ASD) is a neurodevelopmental condition
characterized by a range of difficulties in social communication,
behavioral flexibility, and sensory processing, and it is understood to
arise from a combination of genetic and environmental factors (Lord et
al., 2020). Recent studies report that the heritability of ASD is
approximately 70 to 80 percent (Bai et al., 2019), and that it occurs
more frequently in boys (Le Couteur and Szatmari, 2015). One of the
strongest pieces of evidence for the genetic basis of ASD comes from
twin studies. Monozygotic twins show concordance rates of about 76
percent, whereas dizygotic twins show concordance rates that are close
to zero (Tick et al., 2016). This pronounced difference indicates that
genetic factors contribute substantially to ASD liability. However, even
with high heritability, the extent to which this genetic risk manifests
as ASD-related traits may differ depending on developmental and
environmental contexts, including sex, race, and age. In fact, ASD shows
variation in diagnostic likelihood not only by sex but also across
racial and ethnic groups (Mandell et al., 2009; Zaroff and Uhm, 2012),
and minority group children have been reported to experience delayed or
under-diagnosis even when early behavioral signs of ASD are present (Jo
et al., 2015). These findings highlight the importance of understanding
how parental family history of ASD as a genetic risk factor interacts
with demographic characteristics.

These prior studies show that ASD has a complex set of characteristics
that cannot be explained by a single cause, and that genetic risk exists
but may not manifest uniformly across all children. Various factors such
as sex, race or ethnicity, developmental stage, and birth-related
characteristics may influence the expression and interpretation of early
risk indicators. The present study builds on this perspective and uses
Q-Chat-10 data to explore how parental family history of ASD, as a proxy
for genetic risk, is reflected in ASD-related traits together with the
child’s sex, race, age, and history of neonatal jaundice. Specifically,
the study examines whether children with a family history of ASD show
certain behavioral patterns more frequently, whether this influence
differs across sex or racial groups, and whether the expression of these
traits varies by developmental stage. Through this analysis, the study
aims to deepen understanding of the complex structure of ASD risk.

## 1.2. Dataset Description

In the present study, we used the Q-Chat-10 dataset from the [*Autism
Dataset for
Toddlers*](https://www.kaggle.com/datasets/vaishnavisirigiri/autism-dataset-for-toddlers)
published on Kaggle (provided by Vaishnavi Sirigiri). This dataset
includes behavioral ASD risk screening data for a total of 1,054
toddlers and was developed to support the creation of simple and
accessible behavior-based early ASD risk detection models that address
delayed diagnosis and the high cost of clinical assessment. Because this
dataset contains both behavioral ASD risk indicators and diverse
background information, including sex, race, family history, and
birth-related factors, it is well suited for exploratory analyses of how
behavioral markers interact with demographic and environmental
characteristics to shape ASD-like traits. The variables are as follows:

- Behavioral items (A1–A10): Binary variables representing the ten items
  of the Q-Chat-10.

  - For A1–A9, responses of Sometimes / Rarely / Never were coded as 1

  - For A10, responses of Always / Usually / Sometimes were coded as 1

- Total score (Qchat.10.Score): Calculated as the sum of A1–A10 and
  generally interpreted such that a score above 3 indicates a potential
  presence of ASD traits.

- Binary classification (Class.ASD.Traits): An indicator of ASD-like
  behavioral traits coded as “Yes” when Qchat.10.Score ≥ 4. This
  variable reflects behavioral risk screening rather than clinical
  diagnosis.

- Demographic information: Sex, Ethnicity, and Age_Mons (age in months).

- Family history (Family_mem_with_ASD): A variable indicating whether a
  parent or family member has a history of ASD, serving as a simple
  proxy for genetic risk.

- Birth-related factor (Jaundice): An indicator of neonatal jaundice,
  which can be used as a nonshared environmental or perinatal factor.

- Respondent information (who.completed.the.test): Identifies who
  completed the questionnaire (e.g., parent or caregiver).

The code below provides a brief overview of the structure of the
dataset.

``` r
library(tidyverse)
library(broom)
library(ggplot2)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(pheatmap)

nrow(df)
```

    [1] 1054

``` r
head(df)
```

      Case_No A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 Age_Mons Qchat.10.Score Sex
    1       1  0  0  0  0  0  0  1  1  0   1       28              3   f
    2       2  1  1  0  0  0  1  1  0  0   0       36              4   m
    3       3  1  0  0  0  0  0  1  1  0   1       36              4   m
    4       4  1  1  1  1  1  1  1  1  1   1       24             10   m
    5       5  1  1  0  1  1  1  1  1  1   1       20              9   f
    6       6  1  1  0  0  1  1  1  1  1   1       21              8   m
           Ethnicity Jaundice Family_mem_with_ASD Who.completed.the.test
    1 middle eastern      yes                  no          family member
    2 White European      yes                  no          family member
    3 middle eastern      yes                  no          family member
    4       Hispanic       no                  no          family member
    5 White European       no                 yes          family member
    6          black       no                  no          family member
      Class.ASD.Traits.
    1                No
    2               Yes
    3               Yes
    4               Yes
    5               Yes
    6               Yes

``` r
plot_pie <- function(data, var, title) {
  
  df_plot <- data %>%
    count({{ var }}) %>%
    mutate(
      pct = n / sum(n) * 100,
      legend_label = paste0({{ var }}, " (", round(pct, 1), "%)")
    )
  
  ggplot(df_plot, aes(x = "", y = pct, fill = legend_label)) +
    geom_col(width = 1) +
    coord_polar("y", start = 0) +
    labs(title = title, x = NULL, y = NULL, fill = as_label(enquo(var))) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
}
```

``` r
plot_pie(df, Sex, "Sex Distribution")
```

![](index_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
plot_pie(df, Ethnicity, "Ethnicity Distribution")
```

![](index_files/figure-commonmark/unnamed-chunk-4-2.png)

``` r
plot_pie(df, Jaundice, "Jaundice Distribution")
```

![](index_files/figure-commonmark/unnamed-chunk-4-3.png)

``` r
plot_pie(df, Who.completed.the.test, "Who Completed the Test Distribution")
```

![](index_files/figure-commonmark/unnamed-chunk-4-4.png)

``` r
plot_pie(df, Class.ASD.Traits., "Class ASD Traits Distribution")
```

![](index_files/figure-commonmark/unnamed-chunk-4-5.png)

## 1.3. Research Questions

In this study, we address the following four research questions.

1.  Does the level of ASD risk differ by sex or ethnicity among children
    with a parental family history of ASD?

    - Children with a parental history of ASD are known to be more
      likely to show ASD traits (autistic tendency scores). However,
      there is limited research on whether this genetic influence
      operates uniformly across all children, or whether it differs by
      sex (male/female) or ethnicity group. Therefore, by examining
      interactions such as family history × sex and family history ×
      ethnicity, we aim to explore whether genetic effects are stronger
      in specific subgroups.

2.  Do children with a parental family history of ASD show higher
    problem scores on specific behavioral items (A1–A10)?

    - ASD risk is not a single score but is composed of multiple
      behavioral characteristics (A1–A10). When there is a parental
      history of ASD, we analyze whether children exhibit stronger risk
      patterns on particular items. The final results will be visualized
      using tools such as heatmaps and PCA to explore differences in
      behavioral patterns.

3.  Does the association between parental ASD family history and ASD
    traits vary according to the child’s age in months (Age_Mons)?

    - The expression of ASD traits may differ across developmental
      stages in toddlers. Therefore, even when the same family history
      is present, we explore whether the association with ASD tendency
      scores varies between younger and older children. Specifically, we
      analyze age-related differences in effects using the interaction
      between family history and Age_Mons, treating Age_Mons either as a
      continuous variable or as a low-age versus high-age grouping
      variable.

4.  What patterns are observed for simple nonshared environmental
    effects, including jaundice?

    - Although causal relationships cannot be inferred, we explore
      whether jaundice is independently associated with ASD traits, or
      whether it further amplifies risk among children with a parental
      family history of ASD, when considering family history and
      jaundice status simultaneously.

# 2. Methodology

## 2.1. Logistic Regression Models

For all research questions, we use logistic regression models in which
the dependent variable is binary. The general form of the model is:

$$logit{P(ASD traits = 1)} = \beta_0 + \beta X + Interaction terms$$

This framework allows us to examine how parental ASD family history
relates to ASD traits, both directly and through interactions with
demographic or environmental factors.

### Model 1A - Family ASD x Sex

This model evaluates whether the association between parental ASD
history and ASD traits differs by the child’s sex.

$logit\,P(\text{ASD traits}=1) = \beta_0 + \beta_1 FamilyASD + \beta_2 Sex + \beta_3(FamilyASD \times Sex) + \beta_4 Ethnicity + \beta_5 AgeMons + \beta_6 Jaundice$

The interaction term tests whether the effect of family history is
stronger for one sex than the other.

### Model 1B - Family ASD x Ethnicity

This model examines whether the effect of parental ASD family history
varies across racial and ethnic groups.

$logit\,P(\text{ASD traits}=1) = \beta_0 + \beta_1 FamilyASD + \beta_2 Ethnicity + \beta_3(FamilyASD \times Ethnicity) + \beta_4 Sex + \beta_5 AgeMons + \beta_6 Jaundice$

This allows us to assess potential demographic disparities in how
genetic risk manifests behaviorally.

### Model 2 - Item-Level Logistic Regressions

To understand how parental ASD history relates to specific behavioral
domains, we fit separate logistic regression models for each Q-Chat-10
item.

For all Items $A_i (i = 1, 2, ..., 10)$

$logit\,P(A_i=1) = \beta_0 + \beta_1 FamilyASD + \beta_2 Sex + \beta_3 Ethnicity + \beta_4 AgeMons + \beta_5 Jaundice$

Interaction terms are not used here because the goal is to identify
global behavioral tendencies associated with family history rather than
to model effect modification.

### Model 3 - Family ASD x Age_Mons

This model tests whether age moderates the relationship between family
history and ASD traits.

$logit\,P(\text{ASD traits}=1) = \beta_0 + \beta_1 FamilyASD + \beta_2 AgeMons + \beta_3(FamilyASD \times AgeMons) + \beta_4 Sex + \beta_5 Ethnicity + \beta_6 Jaundice$

This approach captures potential developmental differences in the
expression of ASD-related behaviors.

### Model 4 - Family ASD x Jaundice

This model evaluates whether neonatal jaundice modifies the association
between family history and ASD traits.

$logit\,P(\text{ASD traits}=1) = \beta_0 + \beta_1 FamilyASD + \beta_2 Jaundice + \beta_3(FamilyASD \times Jaundice) + \beta_4 Sex + \beta_5 Ethnicity + \beta_6 AgeMons$

This enables exploratory assessment of a possible nonshared
environmental factor and its interaction with genetic risk.

## 2.2. Principal Component Analysis (PCA)

Principal Component Analysis (PCA) is a dimensionality reduction method
that converts correlated variables into a smaller set of uncorrelated
components that capture the main axes of variation. Because the ten
behavioral items are expected to co-occur, PCA provides a concise way to
characterize their joint structure and identify underlying behavioral
dimensions. By mapping individuals onto the first two principal
components, we can visualize broad patterns in these behaviors and
assess whether children with a parental ASD family history show
discernible differences compared to those without such history. This
approach allows us to detect group-level distinctions in overall
behavioral profiles that may not be apparent from item-by-item analyses.

## 2.3. Data Cleansing

Before conducting the regression analyses, we performed basic data
cleansing to ensure that all variables were coded in formats appropriate
for statistical modeling. The following code converts key categorical
variables into factors with explicitly defined levels, standardizes the
coding of binary variables, and inspects the distribution of each
category. In preliminary models, some ethnicity categories had very
small cell counts and contributed to quasi-complete separation in
logistic regression, particularly when interaction terms with family ASD
history were included. To mitigate this issue and obtain stable
estimates, we further collapsed ethnicity into two groups (White vs
Other) for the regression analyses.

``` r
df$Class.ASD.Traits      <- factor(df$Class.ASD.Traits, levels = c("No", "Yes"))
df$Family_mem_with_ASD   <- factor(df$Family_mem_with_ASD, levels = c("no", "yes"))
df$Jaundice              <- factor(df$Jaundice, levels = c("no", "yes"))
df$Sex                   <- factor(df$Sex)
df$Ethnicity             <- factor(df$Ethnicity)
df$Age_Mons              <- as.numeric(df$Age_Mons)

lapply(
  df[, c("Class.ASD.Traits", "Family_mem_with_ASD", "Jaundice",
         "Sex", "Ethnicity")],
  table,
  useNA = "ifany"
)
```

    $Class.ASD.Traits

     No Yes 
    326 728 

    $Family_mem_with_ASD

     no yes 
    884 170 

    $Jaundice

     no yes 
    766 288 

    $Sex

      f   m 
    319 735 

    $Ethnicity

             asian          black       Hispanic         Latino middle eastern 
               299             53             40             26            188 
             mixed  Native Indian         Others       Pacifica    south asian 
                 8              3             35              8             60 
    White European 
               334 

``` r
df_rq1 <- df %>%
  mutate(
    Class_ASD  = factor(Class.ASD.Traits, levels = c("No", "Yes")),
    Family_ASD = factor(Family_mem_with_ASD, levels = c("no", "yes")),
    Sex        = factor(Sex),
    Ethnicity  = factor(Ethnicity),
    Age_Mons   = as.numeric(Age_Mons)
  ) %>%
  filter(
    !is.na(Class_ASD),
    !is.na(Family_ASD),
    !is.na(Sex),
    !is.na(Ethnicity),
    !is.na(Age_Mons)
  ) %>%
  mutate(
    Ethnicity_collapsed = ifelse(grepl("White", Ethnicity), "White", "Other"),
    Ethnicity_collapsed = factor(Ethnicity_collapsed, levels = c("White", "Other"))
  )
```

# 3. Results

## 3.1. Results for RQ1

``` r
library(dplyr)
library(forcats)
library(logistf)
library(knitr)   

model_sex <- glm(
  Class_ASD ~ Family_ASD * Sex + Age_Mons + Ethnicity_collapsed,
  data   = df_rq1,
  family = binomial
)
```

``` r
tbl_sex <- tidy(
  model_sex,
  exponentiate = TRUE,  
  conf.int    = TRUE     
) %>%

  mutate(term = case_when(
    term == "(Intercept)"               ~ "Intercept",
    term == "Family_ASDyes"            ~ "Family ASD history (yes vs no)",
    term == "Sexmale"                  ~ "Sex (male vs female)", 
    grepl("^Ethnicity_collapsed", term)~ gsub("Ethnicity_collapsed", "Ethnicity: ", term),
    term == "Age_Mons"                 ~ "Age (months)",
    term == "Family_ASDyes:Sexmale"    ~ "Family ASD × Sex interaction",
    TRUE                               ~ term
  )) %>%
  rename(
    OR        = estimate,
    `CI low`  = conf.low,
    `CI high` = conf.high,
    `p-value` = p.value
  )
```

``` r
kable(
  tbl_sex,
  digits  = 3,
  caption = "Logistic regression of ASD traits on family ASD history, sex, age, and ethnicity"
)
```

| term | OR | std.error | statistic | p-value | CI low | CI high |
|:---|---:|---:|---:|---:|---:|---:|
| Intercept | 1.496 | 0.271 | 1.486 | 0.137 | 0.882 | 2.554 |
| Family ASD history (yes vs no) | 0.610 | 0.343 | -1.441 | 0.150 | 0.311 | 1.203 |
| Sexm | 1.556 | 0.156 | 2.834 | 0.005 | 1.145 | 2.110 |
| Age (months) | 1.016 | 0.008 | 1.889 | 0.059 | 0.999 | 1.033 |
| Ethnicity: Other | 0.644 | 0.152 | -2.899 | 0.004 | 0.476 | 0.864 |
| Family_ASDyes:Sexm | 1.520 | 0.405 | 1.034 | 0.301 | 0.686 | 3.371 |

Logistic regression of ASD traits on family ASD history, sex, age, and
ethnicity

In Model 1A, male sex emerged as a significant predictor of ASD traits,
with males exhibiting approximately 1.56 times higher odds of being
classified as displaying ASD-like behaviors than females. Parental ASD
family history did not show a strong independent main effect, and the
interaction between family history and sex was not statistically
significant, indicating that the influence of family history does not
differ substantially between males and females. Age showed a marginal
trend toward higher odds of ASD traits with increasing months, although
this association did not reach conventional significance. Children
categorized in the Other ethnicity group had significantly lower odds of
ASD traits compared to White children. Overall, sex and ethnicity were
the strongest predictors in the model, whereas the modifying role of
family history was not supported.

``` r
model_eth <- glm(
  Class_ASD ~ Family_ASD * Ethnicity_collapsed + Sex + Age_Mons + Jaundice,
  data   = df_rq1,
  family = binomial
)

tbl_eth <- tidy(
  model_eth,
  exponentiate = TRUE,   # OR
  conf.int    = TRUE     # 95% CI
) %>%
  mutate(term = case_when(
    term == "(Intercept)"                        ~ "Intercept",
    term == "Family_ASDyes"                     ~ "Family ASD history (yes vs no)",
    term == "Sexmale"                           ~ "Sex (male vs female)",  
    grepl("^Ethnicity_collapsed", term)         ~ gsub("Ethnicity_collapsed", "Ethnicity: ", term),
    grepl("Family_ASDyes:Ethnicity_collapsed", term) ~ gsub("Family_ASDyes:Ethnicity_collapsed", 
                                                            "Interaction: Family ASD × Ethnicity: ", term),
    term == "Age_Mons"                          ~ "Age (months)",
    term == "Jaundiceyes"                       ~ "Jaundice (yes vs no)",
    TRUE                                        ~ term
  )) %>%
  rename(
    OR        = estimate,
    `CI low`  = conf.low,
    `CI high` = conf.high,
    `p-value` = p.value
  )

kable(
  tbl_eth,
  digits  = 3,
  caption = "Logistic regression of ASD traits on family ASD history, ethnicity, sex, age, and jaundice"
)
```

| term | OR | std.error | statistic | p-value | CI low | CI high |
|:---|---:|---:|---:|---:|---:|---:|
| Intercept | 1.310 | 0.281 | 0.962 | 0.336 | 0.758 | 2.281 |
| Family ASD history (yes vs no) | 0.716 | 0.297 | -1.128 | 0.259 | 0.403 | 1.297 |
| Ethnicity: Other | 0.652 | 0.171 | -2.500 | 0.012 | 0.463 | 0.908 |
| Sexm | 1.633 | 0.144 | 3.394 | 0.001 | 1.229 | 2.166 |
| Age (months) | 1.017 | 0.008 | 1.940 | 0.052 | 1.000 | 1.034 |
| Jaundice (yes vs no) | 1.389 | 0.159 | 2.061 | 0.039 | 1.020 | 1.905 |
| Interaction: Family ASD × Ethnicity: Other | 1.208 | 0.378 | 0.501 | 0.616 | 0.573 | 2.528 |

Logistic regression of ASD traits on family ASD history, ethnicity, sex,
age, and jaundice

In Model 1B, ethnicity and sex emerged as significant predictors of ASD
trait classification. Children in the Other ethnicity group showed
significantly lower odds of ASD traits compared to White children, and
males exhibited higher odds than females. Age showed a marginal positive
association with ASD traits, and jaundice was associated with higher
odds, although the interpretation remains exploratory. Parental ASD
family history did not have a significant main effect, and the
interaction between family history and ethnicity was not significant,
indicating that the influence of family history does not differ
meaningfully across ethnic groups.

## 3.2. Results for RQ2

``` r
item_vars <- paste0("A", 1:10)

models_items <- lapply(item_vars, function(v) {
  glm(
    as.formula(
      paste(v, "~ Family_ASD + Sex + Ethnicity_collapsed + Age_Mons + Jaundice")
    ),
    data   = df_rq1,
    family = binomial
  )
})

names(models_items) <- item_vars
```

``` r
tbl_items <- lapply(names(models_items), function(v) {
  tidy(models_items[[v]],
       exponentiate = TRUE,
       conf.int     = TRUE) %>%
    mutate(item = v)
}) %>%
  bind_rows() %>%
  relocate(item, .before = term)

tbl_items_family <- tbl_items %>%
  filter(term == "Family_ASDyes") %>%
  rename(
    OR        = estimate,
    `CI low`  = conf.low,
    `CI high` = conf.high,
    `p-value` = p.value
  )

kable(
  tbl_items_family,
  digits  = 3,
  caption = "Association between parental ASD history and each Q-Chat-10 item (adjusted for sex, ethnicity, age, and jaundice)"
)
```

| item | term          |    OR | std.error | statistic | p-value | CI low | CI high |
|:-----|:--------------|------:|----------:|----------:|--------:|-------:|--------:|
| A1   | Family_ASDyes | 1.228 |     0.175 |     1.171 |   0.242 |  0.873 |   1.738 |
| A2   | Family_ASDyes | 1.107 |     0.170 |     0.598 |   0.550 |  0.792 |   1.546 |
| A3   | Family_ASDyes | 1.240 |     0.172 |     1.249 |   0.212 |  0.883 |   1.737 |
| A4   | Family_ASDyes | 0.759 |     0.172 |    -1.606 |   0.108 |  0.541 |   1.062 |
| A5   | Family_ASDyes | 0.814 |     0.171 |    -1.204 |   0.229 |  0.582 |   1.138 |
| A6   | Family_ASDyes | 1.214 |     0.176 |     1.105 |   0.269 |  0.863 |   1.719 |
| A7   | Family_ASDyes | 0.800 |     0.176 |    -1.263 |   0.206 |  0.568 |   1.134 |
| A8   | Family_ASDyes | 0.776 |     0.172 |    -1.469 |   0.142 |  0.553 |   1.086 |
| A9   | Family_ASDyes | 0.885 |     0.171 |    -0.719 |   0.472 |  0.632 |   1.236 |
| A10  | Family_ASDyes | 1.015 |     0.173 |     0.086 |   0.931 |  0.725 |   1.428 |

Association between parental ASD history and each Q-Chat-10 item
(adjusted for sex, ethnicity, age, and jaundice)

Across the ten behavioral items (A1–A10), none of the odds ratios
associated with parental ASD history reached statistical significance
after adjusting for sex, ethnicity, age, and jaundice. The odds ratios
ranged roughly from 0.76 to 1.24, indicating small and inconsistent
differences between children with and without an ASD family history.

- For some items (e.g., A1, A3, A6), the OR was slightly above 1,
  suggesting a weak trend toward higher endorsement among children with
  a family history.

- For other items (e.g., A4, A5, A7, A8), the OR was below 1, indicating
  a weak trend in the opposite direction.

- However, all p-values were greater than 0.10, and all confidence
  intervals crossed 1.0, meaning none of these differences provide
  statistically reliable evidence of item-specific effects.

Although item-level logistic regressions did not reveal significant
associations with parental ASD history, we additionally examined the
multivariate item structure using PCA and heatmaps. These exploratory
visualizations likewise showed minimal separation between groups,
reinforcing the conclusion that family history does not selectively
influence specific behavioral indicators.

``` r
items <- df_rq1[, paste0("A", 1:10)]

pca_res <- prcomp(items, scale. = TRUE)
summary(pca_res)
```

    Importance of components:
                              PC1    PC2     PC3     PC4     PC5     PC6     PC7
    Standard deviation     1.8994 1.0944 0.93415 0.91420 0.87753 0.79994 0.75919
    Proportion of Variance 0.3608 0.1198 0.08726 0.08358 0.07701 0.06399 0.05764
    Cumulative Proportion  0.3608 0.4806 0.56782 0.65140 0.72840 0.79239 0.85003
                               PC8     PC9    PC10
    Standard deviation     0.74182 0.72779 0.64787
    Proportion of Variance 0.05503 0.05297 0.04197
    Cumulative Proportion  0.90506 0.95803 1.00000

``` r
pca_df <- data.frame(
  PC1        = pca_res$x[, 1],
  PC2        = pca_res$x[, 2],
  Family_ASD = df_rq1$Family_ASD
)

ggplot(pca_df, aes(PC1, PC2, color = Family_ASD)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "PCA of Q-Chat-10 Items (A1–A10) by Parental ASD History",
    x     = "PC1",
    y     = "PC2",
    color = "Family ASD history"
  )
```

![](index_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
loadings <- data.frame(
  item = rownames(pca_res$rotation),
  PC1  = pca_res$rotation[, 1],
  PC2  = pca_res$rotation[, 2]
)

ggplot(loadings, aes(PC1, PC2, label = item)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "PCA Loadings for Q-Chat-10 Items (A1–A10)",
    x     = "Loading on PC1",
    y     = "Loading on PC2"
  )
```

![](index_files/figure-commonmark/unnamed-chunk-14-1.png)

``` r
library(pheatmap)
item_means <- aggregate(
  items,
  by   = list(Family_ASD = df_rq1$Family_ASD),
  FUN  = mean
)

rownames(item_means) <- item_means$Family_ASD
item_means$Family_ASD <- NULL

pheatmap(
  item_means,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  main = "Mean Q-Chat-10 Item Scores by Parental ASD History"
)
```

![](index_files/figure-commonmark/unnamed-chunk-15-1.png)

Across all analyses including item-level logistic regressions, PCA, and
heatmap visualizations, there was no evidence that parental ASD family
history was associated with specific behavioral patterns on the
Q-Chat-10. Children with and without a family history showed nearly
identical distributions across individual items, similar positions in
the PCA space, and comparable mean item scores. These findings suggest
that family history does not selectively influence particular early
behavioral indicators measured by the Q-Chat-10.

## 3.3. Results for RQ3

``` r
model3_age <- glm(
  Class_ASD ~ Family_ASD * Age_Mons +
    Sex + Ethnicity_collapsed + Jaundice,
  data   = df_rq1,
  family = binomial
)
summary(model3_age)
```


    Call:
    glm(formula = Class_ASD ~ Family_ASD * Age_Mons + Sex + Ethnicity_collapsed + 
        Jaundice, family = binomial, data = df_rq1)

    Coefficients:
                              Estimate Std. Error z value Pr(>|z|)    
    (Intercept)               0.035053   0.288428   0.122 0.903270    
    Family_ASDyes             1.289145   0.716939   1.798 0.072157 .  
    Age_Mons                  0.024395   0.009189   2.655 0.007931 ** 
    Sexm                      0.484727   0.144592   3.352 0.000801 ***
    Ethnicity_collapsedOther -0.396509   0.153057  -2.591 0.009581 ** 
    Jaundiceyes               0.314834   0.159552   1.973 0.048469 *  
    Family_ASDyes:Age_Mons   -0.052923   0.023994  -2.206 0.027410 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1303.9  on 1053  degrees of freedom
    Residual deviance: 1268.6  on 1047  degrees of freedom
    AIC: 1282.6

    Number of Fisher Scoring iterations: 4

``` r
tbl_model3 <- tidy(
  model3_age,
  exponentiate = TRUE,   # OR
  conf.int     = TRUE    # CI
) %>%
  mutate(term = case_when(
    term == "(Intercept)"            ~ "Intercept",
    term == "Family_ASDyes"         ~ "Family ASD history (yes vs no)",
    term == "Age_Mons"              ~ "Age (months)",
    term == "Family_ASDyes:Age_Mons" ~ "Interaction: Family ASD × Age",
    term == "Sexmale"               ~ "Sex (male vs female)",
    grepl("^Ethnicity_collapsed", term) ~ gsub("Ethnicity_collapsed", "Ethnicity: ", term),
    term == "Jaundiceyes"           ~ "Jaundice (yes vs no)",
    TRUE                             ~ term
  )) %>%
  rename(
    OR        = estimate,
    `CI low`  = conf.low,
    `CI high` = conf.high,
    `p-value` = p.value
  )

kable(
  tbl_model3,
  digits  = 3,
  caption = "Model 3: Interaction between family ASD history and age (months)"
)
```

| term | OR | std.error | statistic | p-value | CI low | CI high |
|:---|---:|---:|---:|---:|---:|---:|
| Intercept | 1.036 | 0.288 | 0.122 | 0.903 | 0.590 | 1.829 |
| Family ASD history (yes vs no) | 3.630 | 0.717 | 1.798 | 0.072 | 0.924 | 15.582 |
| Age (months) | 1.025 | 0.009 | 2.655 | 0.008 | 1.006 | 1.043 |
| Sexm | 1.624 | 0.145 | 3.352 | 0.001 | 1.222 | 2.155 |
| Ethnicity: Other | 0.673 | 0.153 | -2.591 | 0.010 | 0.497 | 0.905 |
| Jaundice (yes vs no) | 1.370 | 0.160 | 1.973 | 0.048 | 1.006 | 1.881 |
| Interaction: Family ASD × Age | 0.948 | 0.024 | -2.206 | 0.027 | 0.904 | 0.993 |

Model 3: Interaction between family ASD history and age (months)

Model 3 assessed whether the association between parental ASD history
and ASD traits varied across age by including a Family ASD × Age
interaction term. The interaction was statistically significant (OR =
0.948, p = 0.027), indicating that the age-related increase in ASD trait
endorsement was weaker among children with a parental ASD family
history. In other words, although older toddlers generally showed higher
odds of ASD-like behaviors, this developmental increase was less
pronounced in the family-history group. This suggests that the influence
of parental ASD history may be stronger at younger ages and gradually
attenuate as children grow. Age, sex, and jaundice also showed
independent associations with ASD traits.

## 3.4. Results for RQ4

``` r
model4_jaundice <- glm(
  Class_ASD ~ Family_ASD * Jaundice +
    Sex + Ethnicity_collapsed + Age_Mons,
  data   = df_rq1,
  family = binomial
)

summary(model4_jaundice)
```


    Call:
    glm(formula = Class_ASD ~ Family_ASD * Jaundice + Sex + Ethnicity_collapsed + 
        Age_Mons, family = binomial, data = df_rq1)

    Coefficients:
                               Estimate Std. Error z value Pr(>|z|)    
    (Intercept)                0.252496   0.274841   0.919 0.358253    
    Family_ASDyes             -0.314556   0.218003  -1.443 0.149050    
    Jaundiceyes                0.265634   0.175855   1.511 0.130910    
    Sexm                       0.487976   0.144376   3.380 0.000725 ***
    Ethnicity_collapsedOther  -0.391826   0.152738  -2.565 0.010307 *  
    Age_Mons                   0.016635   0.008465   1.965 0.049409 *  
    Family_ASDyes:Jaundiceyes  0.332451   0.407144   0.817 0.414189    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 1303.9  on 1053  degrees of freedom
    Residual deviance: 1272.9  on 1047  degrees of freedom
    AIC: 1286.9

    Number of Fisher Scoring iterations: 4

``` r
tbl_model4 <- tidy(
  model4_jaundice,
  exponentiate = TRUE,
  conf.int     = TRUE
) %>%
  mutate(term = case_when(
    term == "(Intercept)"              ~ "Intercept",
    term == "Family_ASDyes"           ~ "Family ASD history (yes vs no)",
    term == "Jaundiceyes"             ~ "Jaundice (yes vs no)",
    term == "Family_ASDyes:Jaundiceyes" ~ "Interaction: Family ASD × Jaundice",
    term == "Sexmale"                 ~ "Sex (male vs female)",
    grepl("^Ethnicity_collapsed", term) ~ gsub("Ethnicity_collapsed", "Ethnicity: ", term),
    term == "Age_Mons"                ~ "Age (months)",
    TRUE                              ~ term
  )) %>%
  rename(
    OR        = estimate,
    `CI low`  = conf.low,
    `CI high` = conf.high,
    `p-value` = p.value
  )

kable(
  tbl_model4,
  digits  = 3,
  caption = "Model 4: Interaction between family ASD history and jaundice"
)
```

| term | OR | std.error | statistic | p-value | CI low | CI high |
|:---|---:|---:|---:|---:|---:|---:|
| Intercept | 1.287 | 0.275 | 0.919 | 0.358 | 0.753 | 2.214 |
| Family ASD history (yes vs no) | 0.730 | 0.218 | -1.443 | 0.149 | 0.478 | 1.126 |
| Jaundice (yes vs no) | 1.304 | 0.176 | 1.511 | 0.131 | 0.928 | 1.851 |
| Sexm | 1.629 | 0.144 | 3.380 | 0.001 | 1.227 | 2.161 |
| Ethnicity: Other | 0.676 | 0.153 | -2.565 | 0.010 | 0.499 | 0.909 |
| Age (months) | 1.017 | 0.008 | 1.965 | 0.049 | 1.000 | 1.034 |
| Interaction: Family ASD × Jaundice | 1.394 | 0.407 | 0.817 | 0.414 | 0.636 | 3.156 |

Model 4: Interaction between family ASD history and jaundice

Model 4 examined whether neonatal jaundice moderated the association
between parental ASD family history and ASD traits. The interaction term
was not statistically significant (OR = 1.39, p = 0.41), indicating that
jaundice did not alter the effect of family ASD history on the
likelihood of ASD-like behaviors. Although jaundice and family history
showed small, non-significant associations in opposite directions, their
effects were independent rather than interactive. Sex and age remained
significant predictors, consistent with established developmental
patterns.

# 4. Discussion

This study examined how early ASD related traits are associated with
family history, sex, ethnicity, age, and perinatal factors. Across the
four research questions, the overall pattern suggests that family ASD
history showed limited influence on early ASD like behaviors with the
exception of its interaction with age. The interaction between family
history and sex was not significant. Although boys typically show higher
ASD prevalence in population studies (Le Couteur and Szatmari, 2015),
many studies suggest that these sex differences are shaped partly by
diagnostic practices and sociocultural expectations rather than stronger
genetic effects in boys (Mandy et al., 2012; Loomses et al., 2017). Our
finding is consistent with this perspective by showing that genetic risk
did not manifest differently between sexes during infancy. The
interaction between family ASD history and ethnicity was also not
significant. This pattern agrees with the argument presented by Zaroff
and Uhm (2012), who emphasized that global variation in ASD prevalence
is driven primarily by methodological differences such as diagnostic
criteria, sampling strategies, and assessment procedures rather than
biological differences across ethnic groups. Furthermore, Jo et
al. (2015) showed that minority groups, particularly non Hispanic Black
and Hispanic children who speak languages other than English at home,
are often under identified or diagnosed later due to barriers in access,
awareness, and reporting. Because the current study relies on a parent
completed screening instrument rather than a clinical diagnostic
pathway, our dataset is less vulnerable to these disparities. This may
explain why ethnicity did not significantly modify the association
between family history and ASD traits.

The item level analyses showed that family ASD history did not
significantly predict any individual Q Chat item after adjusting for
covariates. PCA and heatmap analyses also revealed that children with
and without a family history did not form distinct behavioral clusters.
These results suggest that early genetic liability may not be strongly
expressed through specific discrete behaviors measured by binary items
but may instead influence early developmental tendencies that are
difficult to isolate at the item level.

Age was the only significant moderator. In line with prior evidence on
genetic influences in early neurodevelopment, children with a family ASD
history showed elevated ASD like traits at younger ages, and the effect
diminished with age. This pattern may reflect the idea that genetic
vulnerabilities are more detectable during very early developmental
windows before environmental interactions and maturation broaden
individual variability.

Jaundice did not significantly interact with family history, and its
main effect was small. This is consistent with recent neonatal risk
research showing mixed or weak associations between neonatal jaundice
and ASD, suggesting that perinatal bilirubin related processes are
unlikely to be major contributors to ASD traits.

This study has several limitations. The dataset is cross sectional,
preventing the assessment of developmental trajectories or causal
relationships. The ASD outcome is based on a screening threshold rather
than clinical diagnosis, so findings reflect ASD like traits rather than
confirmed ASD. Family history is self reported and does not capture
detailed genetic information. The dataset lacks key prenatal and
socioeconomic variables such as birth weight, parental education, or
healthcare access, which are known to influence ASD identification and
reporting. The sample is a convenience sample from an online platform
and may not be representative. Finally, some interactions may suffer
from limited cell counts, reducing statistical power.

# 5. Conclusion

Using the Q-Chat-10 dataset, this study examined how parental ASD family
history relates to ASD-like behavioral traits in toddlers. The findings
indicate that parental ASD history shows only a modest association with
overall risk and does not interact with sex or ethnicity. Item-level
analyses revealed no specific behavioral indicators that were
selectively elevated among children with a family history, and PCA and
heatmap visualizations further confirmed the absence of meaningful
structural differences in behavioral profiles between groups. Age was
independently associated with higher ASD-like behaviors, and the
influence of family history gradually diminished with increasing age.
Neonatal jaundice did not moderate the effect of family history.
Overall, parental ASD family history appears to exert broad but limited
influence on early behavioral traits, with effects that change gradually
with developmental age rather than being concentrated within specific
behaviors or demographic subgroups.

# 6. References

1.  Bai, D., Yip, B. H. K., Windham, G. C., Sourander, A., Francis, R.,
    Yoffe, R., … & Sandin, S. (2019). Association of genetic and
    environmental factors with autism in a 5-country cohort. JAMA
    psychiatry, 76(10), 1035-1043.

2.  Jo, H., Schieve, L. A., Rice, C. E., Yeargin-Allsopp, M., Tian, L.
    H., Blumberg, S. J., … & Boyle, C. A. (2015). Age at autism spectrum
    disorder (ASD) diagnosis by race, ethnicity, and primary household
    language among children with special health care needs, United
    States, 2009–2010. Maternal and child health journal, 19(8),
    1687-1697.

3.  Le Couteur, A., & Szatmari, P. (2015). Autism spectrum disorder.
    Rutter’s child and adolescent psychiatry, 661-682.

4.  Loomes, R., Hull, L., & Mandy, W. P. L. (2017). What is the
    male-to-female ratio in autism spectrum disorder? A systematic
    review and meta-analysis. Journal of the American Academy of Child &
    Adolescent Psychiatry, 56(6), 466-474.

5.  Lord, C., Brugha, T. S., Charman, T., Cusack, J., Dumas, G.,
    Frazier, T., … & Veenstra-VanderWeele, J. (2020). Autism spectrum
    disorder. Nature reviews Disease primers, 6(1), 5.

6.  Mandell, D. S., Wiggins, L. D., Carpenter, L. A., Daniels, J.,
    DiGuiseppi, C., Durkin, M. S., … & Kirby, R. S. (2009).
    Racial/ethnic disparities in the identification of children with
    autism spectrum disorders. American journal of public health, 99(3),
    493-498.

7.  Mandy, W., Chilvers, R., Chowdhury, U., Salter, G., Seigal, A., &
    Skuse, D. (2012). Sex differences in autism spectrum disorder:
    evidence from a large sample of children and adolescents. Journal of
    autism and developmental disorders, 42(7), 1304-1313.

8.  Tick, B., Bolton, P., Happé, F., Rutter, M., & Rijsdijk, F. (2016).
    Heritability of autism spectrum disorders: a meta‐analysis of twin
    studies. Journal of Child Psychology and Psychiatry, 57(5), 585-595.

9.  Zaroff, C. M., & Uhm, S. Y. (2012). Prevalence of autism spectrum
    disorders and influence of country of measurement and ethnicity.
    Social psychiatry and psychiatric epidemiology, 47(3), 395-398.
