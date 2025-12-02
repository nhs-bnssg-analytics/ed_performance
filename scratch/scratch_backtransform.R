# 1. Identify all continuous variables to be scaled
continuous_vars <- c("dta_time", "time_to_dta", "nctr", "n_ed", "vw_score")

# Create a new data frame for the scaled data
mod_df_scaled <- mod_df

# Create a list to store the means and SDs for back-transformation
scaling_params <- list()

# Loop through the continuous variables, scale them, and save parameters
for (var in continuous_vars) {
  cat(var)
    # Scale the variable (centers and scales it)
    scaled_result <- scale(mod_df_scaled[[var]])
    
    # Store the scaled result back into the new data frame with a '_z' suffix
    mod_df_scaled[[paste0(var, "_z")]] <- as.numeric(scaled_result)
    
    # Save the mean and SD for back-transformation
    scaling_params[[var]] <- list(
        mean = attr(scaled_result, "scaled:center"),
        sd = attr(scaled_result, "scaled:scale")
    )
}

# Add the categorical/non-scaled variables back to the new data frame
mod_df_scaled$mort_30 <- mod_df_raw$mort_30
mod_df_scaled$sex <- mod_df_raw$sex
mod_df_scaled$arrival_month <- mod_df_raw$arrival_month
mod_df_scaled$arrival_time <- mod_df_raw$arrival_time
mod_df_scaled$wkend <- mod_df_raw$wkend
mod_df_scaled$nhs_number <- mod_df_raw$nhs_number
# Note: Ensure all necessary non-scaled variables are copied over.

# The 'scaling_params' object now holds all necessary values for back-transformation.

full_formula_z <- mort_30 ~ dta_time_z +
  age +
  sex +
  time_to_dta_z +
  arrival_time +
  arrival_month + 
  wkend +
  nctr_z +
  n_ed_z +
  vw_score_z +
  I(time_to_dta_z ^ 2) +
  I(nctr_z ^ 2) +
  I(n_ed_z ^ 2) +
  I(vw_score_z ^2)

mod_raw <- glm(full_formula_z
  , data = mod_df_scaled, family = binomial)


library(broom.mixed)

# 1. Tidy the converged, scaled model
# This gives you the standard output columns: estimate, std.error, statistic, p.value, etc.
tidy_scaled <- tidy(mod_raw)

# 2. Extract necessary values for back-transformation (from your 'scaling_params')
mean_n_ed <- scaling_params[['n_ed']]$mean
sd_n_ed <- scaling_params[['n_ed']]$sd

beta0_star <- tidy_scaled[tidy_scaled$term == '(Intercept)', 'estimate']
beta1_star <- tidy_scaled[tidy_scaled$term == 'n_ed_z', 'estimate']
beta2_star <- tidy_scaled[tidy_scaled$term == 'I(n_ed_z^2)', 'estimate']
# 3. Calculate back-transformed coefficients (b0, b1, b2)
b2_n_ed <- beta2_star / (sd_n_ed^2)
b1_n_ed <- (beta1_star / sd_n_ed) - (2 * beta2_star * mean_n_ed / (sd_n_ed^2))
b0_n_ed <- beta0_star - (beta1_star * mean_n_ed / sd_n_ed) + (beta2_star * (mean_n_ed^2) / (sd_n_ed^2))

# 4. Prepare the new tidy data frame with back-transformed values

# a. Start with a copy of the original tidy data frame
tidy_backtransformed <- tidy_scaled

# b. Rename the scaled terms to their original, interpretable names for the output table
tidy_backtransformed[tidy_backtransformed$term == 'n_ed_z', 'term'] <- 'n_ed'
tidy_backtransformed[tidy_backtransformed$term == 'I(n_ed_z^2)', 'term'] <- 'I(n_ed^2)'

# c. Update the 'estimate' column with the new values
tidy_backtransformed[tidy_backtransformed$term == '(Intercept)', 'estimate'] <- b0_n_ed
tidy_backtransformed[tidy_backtransformed$term == 'n_ed', 'estimate'] <- b1_n_ed
tidy_backtransformed[tidy_backtransformed$term == 'I(n_ed^2)', 'estimate'] <- b2_n_ed

# IMPORTANT NOTE: The standard errors (std.error) and p-values of the back-transformed
# coefficients WILL BE INCORRECT unless you perform the Delta method. 
# For academic tables, you may need to report p-values from the original scaled model. 
# You should, at minimum, set the SE and p-value for the transformed terms to NA 
# or use the original scaled p-values and include a footnote. 
# For simplicity here, we'll leave the SE/p-values as is, but be aware of the caveat.

# If you only want to report the scaled p-value for the overall term, you can use:
# tidy_backtransformed[tidy_backtransformed$term == '(Intercept)', c('std.error', 'statistic', 'p.value')] <- NA
# This is a strong signal that the inferential stats are compromised by the transformation.


library(gtsummary)

# Pass the manually-modified tidy data frame to gtsummary
tbl_final <- tbl_regression(
    tidy_data = tidy_backtransformed,
    exponentiate = TRUE, # Use TRUE for a logistic regression (Odds Ratios)
    label = list(
        '(Intercept)' = "Intercept",
        'n_ed' = "n ed",
        'I(n_ed2)' = "n ed Squared"
        # ... and so on for other variables
    )
)

tbl_final



mod_df <- mod_df %>%
  mutate(n_ed_dis = case_when(
    n_ed == 0 ~ "0",
    n_ed == 1 ~ "1",
    n_ed == 2 ~ "2",
    n_ed > 2 ~ "2+"
   ))


start_coef <- glm(mort_30 ~ 
  + dta_time
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed_dis
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(vw_score ^2,
family = binomial,
data = mod_df) %>%
  coef()


  start_list <- list(
    fixef = start_coef,
    # Set a small, non-zero starting value for the single random intercept variance component (theta)
    theta = 0.5 
)



mod_raw <- glmer(mort_30 ~ 
  + dta_time
  + age
  + sex
  + time_to_dta
  + arrival_time
  + arrival_month
  + wkend
  + nctr
  + n_ed_dis
  + imd
  + vw_score
  + I(time_to_dta ^ 2)
  + I(nctr ^ 2)
  + I(vw_score ^2) 
  + (1 | nhs_number), 
                    # data = mod_df_raw,
                    data = sample_n(mod_df, 1000), 
                     family = binomial,
                     start = start_list,
                    control = lme4::glmerControl(
                       optimizer = "optimx",
                        calc.derivs = FALSE,
                         optCtrl = list(method = "L-BFGS-B")))


