## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(data.table) # Extension of `data.frame`
#library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
library(haven) # Open .xpt and Stata (.do) files
library(foreign) # Read and Write Data from Other Statistical Systems
library(Hmisc) # Harrell Miscellaneous. Used to extract .dta file label easily
library(htmltools) # Tools for HTML
library(lintr) # Trying to removing the annoying lint
library(gt) # Easily Create Presentation-Ready Display Tables
library(gtExtras) # Additional Functions to Enhance 'gt'
library(broom) # Convert Statistical Analysis Objects into Tidy Tibbles
library(boot) # Bootstrap Functions for R
library(lfe) # Linear Group Fixed Effects
library(sandwich) # For robust standard errors
library(lmtest) # For coefficient testing with robust standard errors
library(plm) # Panel Data Models - to perform OLS with FE
library(tidyr) # Easily Tidy Data with 'spread()' and 'gather()' Functions
library(tidycat) # Tidy Categorical Data with 'tidycat'
library(fixest) # Fast and User-Friendly Fixed-Effects Estimations
library(here)

# linter:disable
lint(
    text = strrep("x", 400L),
    linters = line_length_linter(length = 400L)
)

options(max.print = 100)

## 2. Load and Clean Data


bananas1.R <- read_dta("OriginalData/bananas1.dta") # Main dataset used on the papaer
bananas2.R <- read_dta("OriginalData/bananas2.dta") # Dataset used to generate the tables with maternal fixed effects

bananas1 <- bananas1.R %>% clean_names() # Clean the column names
bananas2 <- bananas2.R %>% clean_names() # Clean the column names

GetBananasVariableDescription <- function(dataset) {
    dataset_length <- dim(dataset)[2] # Get the number of rows
    dataset_labels <- sapply(dataset, function(column) attr(column, "label")) # Get the labels of the columns (its a messy list)
    dataset_types <- sapply(dataset, function(column) attr(column, "format.stata"))
    dataset_variables_description <- tibble(column_name = NA, label = NA, format = NA) # Create a tibble to store the variables description
    for (i in c(1:dataset_length)) {
        dataset_variables_description[i, 1] <- names(dataset[i])
        dataset_variables_description[i, 2] <- dataset_labels[[i]]
        dataset_variables_description[i, 3] <- dataset_types[[i]]
    } # complete the tibble with the variables description. First column: each name, second column: each label
    return(dataset_variables_description)
}

bananas1_variables_description <- GetBananasVariableDescription(bananas1)
bananas2_variables_description <- GetBananasVariableDescription(bananas2)

# Make relevant discrete variables as factors
bananas1$marital <- as.factor(bananas1$marital)
bananas1$ethnic <- as.factor(bananas1$ethnic)
bananas1$grid <- as.factor(bananas1$grid)
bananas1$cohort <- as.factor(bananas1$cohort)

bananas2$marital <- as.factor(bananas2$marital)
bananas2$ethnic <- as.factor(bananas2$ethnic)
bananas2$grid <- as.factor(bananas2$grid)
bananas2$cohort <- as.factor(bananas2$cohort)
bananas2$mfeid <- as.factor(bananas2$mfeid)
no_bootstraps = 100

## 3. Set up functions

### 3.1. Given a dataset with cluster specified, this returns a list with a sample of replacement the size of the unique number of clusters
cluster_sample <- function(data, cluster_var) {
    cluster_list <- unique(data[[cluster_var]]) # List of unique clusters
    sample_size <- length(cluster_list) # Sample size = number of unique clusters (but the sample will have repetitions and non-apperences)

    cluster_sample <- sample(cluster_list, sample_size, replace = TRUE) # Sample of the clusters with
    return(cluster_sample)
}

example1 <- cluster_sample(data = bananas1, cluster_var = "grid")
example1 # Return a list with lenght = 172 of a sample with replacement from the 172 possible clusters

### 3.2. Given a dataset with cluster specified, this returns a shuffled dataset according to the sample of clusters. Notice that since a cluster may appear multiple times, entire chunks of rows from the original dataset will appear multiple times
boot_dataset <- function(data, cluster_var) {
    cluster_sample <- cluster_sample(data, cluster_var) # sample with repetitions of clusters

    boot_dataset <- data.frame() # Empty dataset
    for (i in 1:length(cluster_sample)) {
        temp_dataset <- data %>% filter(.[[cluster_var]] == cluster_sample[i]) # temp_dataset = dataset with rows for cluster_sample[i], there will be repetitions and non-apperences
        boot_dataset <- bind_rows(boot_dataset, temp_dataset) # Add the rows of the sample to the new dataset
    }
    return(boot_dataset) # It will be a dataset with different size from data. Since some clusters appear more than once, this will happern
}

example2 <- boot_dataset(bananas1, "grid")
tibble(example2) %>% head() # This looks like bananas1
dim(example2) # This has the same number os cols, but different number of rows

### 3.3. Given data and model specification, this produces a model object that will be used on the bootsrapping itself
set_model <- function(data, dependent_var, independent_var, fixed_effects, remove_var, model_type) {
    # Common specifications for all regressions
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital")

    if (!is.null(remove_var) && length(remove_var) > 0) {
        mom_controls <- setdiff(mom_controls, remove_var)
    } # Remove the variable if it is in the mom_controls list, if not, remains the same

    mom_controls_str = paste(mom_controls, collapse = " + ")
    
    formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls_str, " | ", fixed_effects_str)
    formula <- as.formula(formula_str)
    print("MODEL FORMULA BELOW:")
    print(formula)

    if (model_type == "ols") {
        model <- felm(formula, data = data) # Fixed effects linear model
    } else if (model_type == "logit") {
        model <- feglm(formula, data = data, family = "logit") # Fixed effects logit model, returns estimates for the coefficients, not the odds ratio
    }
    return(model)
}

example3_1 <- set_model(
    data = bananas1,
    dependent_var = "weight", # weight = birth weight of the children (continuous)
    independent_var = c("bx", "ethnic"), # bx = banana exposure (0 or 1), ethnic = ethnic if the mom (need to include here to have a generic formula to be used to table 5)
    fixed_effects = c("grid", "cohort"), # grid = geographical location (172 unique), cohort = month of birth (36? unique)
    remove_var = c("labors", "visits", "private"),
    model_type = "ols"
)
tidy(example3_1) # Returns the summary, not accounthing for heteroskedasticity (clusted bootstrapping will solve it)

example3_2 <- set_model(
    data = bananas1,
    dependent_var = "weight", # weight = birth weight of the children (continuous)
    independent_var = c("bx", "ethnic"), # bx = banana exposure (0 or 1), ethnic = ethnic if the mom (need to include here to have a generic formula to be used to table 5)
    fixed_effects = c("grid", "cohort"), # grid = geographical location (172 unique), cohort = month of birth (36? unique)
    remove_var = NA,
    model_type = "ols"
)
tidy(example3_2) 

### 3.4. Given original data and model specifications, this returns a 1 X n_coeficients dataframe with the estimates of the coefficients based on a clustered resample with repetition dataset
estimate_coefs <- function(data, dependent_var, independent_var, fixed_effects, remove_var, cluster_var, model_type) {
    boot_data <- boot_dataset(data, cluster_var) %>% tibble() # Make the boot dataset that will be used for one estiamtion
    boot_model <- set_model(boot_data, dependent_var, independent_var, fixed_effects, remove_var, model_type) # Fit the model

    estimates <- tidy(boot_model)
    estimates <- estimates %>% subset(select = c(term, estimate)) # Clean results
    estimates <- as.data.frame(t(estimates)) # Janitoring
    colnames(estimates) <- as.character(unlist(estimates[1, ])) # More janitoring
    estimates <- estimates[-1, ] # Remove the first row
    estimates <- estimates %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df

    estimates[] <- estimates[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    return(estimates)
}

example4 <- estimate_coefs(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols"
)
example4 # Returns a 1 x n_coeficients dataframe. Doing this multiple times we will be able to get the distribution of the coefficients and it's robust standard errors

### 3.5. Given a dataset, model specifications and the number of bootstraps, this performs the previous algorithm n_bootstraps times and returns a dataframe with the estimates of the coefficients for each bootstrap
run_bootstrap <- function(data, dependent_var, independent_var, fixed_effects, remove_var, cluster_var, model_type, n_bootstraps) {
    boot_estimates <- tibble()

    for (i in 1:n_bootstraps) {
        coefs_boot_i <- estimate_coefs(data, dependent_var, independent_var, fixed_effects, remove_var, cluster_var, model_type)
        coefs_boot_i$no_boot <- i
        boot_estimates <- bind_rows(boot_estimates, coefs_boot_i)
        print(paste('Bootstrap no.',i))
    }
    return(boot_estimates)
}

example5_1 <- run_bootstrap(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("grid", "cohort"),
    cluster_var = "grid",
    remove_var = NA,
    model_type = "ols",
    n_bootstraps = 2 # Just to test, in the paper the authors used 1000 - Notice this is VERY computationally expensive
)
example5_1 # (n_bootstraps x n_coeficients) dataframe with the estimates of the coefficients for each bootstrap. From this we get the mean and std. deviaton of the coefficients

example5_2 <- run_bootstrap(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("grid", "cohort"),
    cluster_var = "grid",
    remove_var = c("labors", "visits", "private"),
    model_type = "ols",
    n_bootstraps = 2 # Just to test, in the paper the authors used 1000 - Notice this is VERY computationally expensive
)

example5_3 = run_bootstrap(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxt1", 'ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = 10)

    view(example5_3)

    mean(example5_3$bx_pxt1) # Notice that the standard deviation of the estimates is very high, this is because the sample is very small

# Notice that the mfeid_grid variable is the interaction between the maternal fixed effect and the grid
### 3.6. Given the dataframe with the estimates of the coefficients for each bootstrap, this returns a dataframe with the mean and standard deviation of the coefficients
get_results <- function(data, dependent_var, independent_var, fixed_effects, remove_var, cluster_var, model_type, n_bootstraps, regression_number) {
    boot_estimates <- run_bootstrap(data, dependent_var, independent_var, fixed_effects, remove_var, cluster_var, model_type, n_bootstraps) # Get the dataframe with multiple coefs estimates

    boot_estimates <- boot_estimates %>% subset(select = -c(no_boot)) # Remove the no_boot (in 1, 2, ...., n_bootstraps) column

    if (model_type == "logit") {
        boot_estimates[] <- exp(boot_estimates[]) # Get the odds ratio:
    } else if (model_type == "ols") {
        boot_estimates <- boot_estimates
    }

    coefs_estimates <- set_model(data, dependent_var, independent_var, fixed_effects, remove_var, model_type)
    coefs_estimates <- tidy(coefs_estimates)
    coefs_estimates <- coefs_estimates %>% subset(select = c(1, 2))
    coefs_estimates <- coefs_estimates %>% setnames(c("coef", "estimate"))
    coefs_estimates$coef <- gsub("bx:px", "bx_px", coefs_estimates$coef)

    if(model_type == "logit") {
        coefs_estimates$estimate <- exp(coefs_estimates$estimate)
    }
    else if (model_type == "ols") {
       coefs_estimates <- coefs_estimates
    }

    boot_mean <- sapply(boot_estimates, mean, na.rm = TRUE) # Get the mean for every column
    boot_mean <- as.data.frame(t(boot_mean)) # Janitor it a little bit
    boot_mean <- boot_mean %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_mean[] <- boot_mean[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_mean <- boot_mean %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "mean") # Make it a n_coeficients x 2 dataframe (col1 = coef_name, col2 = mean of bootstraps)
    boot_mean$coef <- gsub("bx:px", "bx_px", boot_mean$coef)

    boot_se <- sapply(boot_estimates, sd, na.rm = TRUE) # Do the same for the std. deviation of bootstrap estiamates = robust std. error
    boot_se <- as.data.frame(t(boot_se))
    boot_se <- boot_se %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_se[] <- boot_se[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_se <- boot_se %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "robust_se")

    results <- full_join(boot_mean, boot_se, by = "coef") # Join both columns
    results <- full_join(coefs_estimates, results, by = "coef")

    results <- results %>% mutate(
        ci_low = estimate - 1.96 * robust_se,
        ci_high = estimate + 1.96 * robust_se
    ) # find 95% conf. interval

    if (model_type == "logit") {
    results <- results %>% mutate(significant = ifelse((ci_low < 1 & ci_high < 1) | (ci_low > 1 & ci_high > 1), "yes", "no"))
    } 
    else if (model_type == "ols") {
    results <- results %>% mutate(significant = ifelse((ci_low < 0 & ci_high < 0) | (ci_low > 0 & ci_high > 0), "yes", "no"))
    }


    results <- results %>% mutate(ci = paste0("[", round(ci_low, 2), ", ", round(ci_high, 2), "]")) # Show the CI
    results <- results %>% select(coef, estimate, mean, robust_se, ci, significant) # Final results for some regression
    colnames(results) <- c(
        "coef",
        paste0("estimate_", regression_number),
        paste0("mean_", regression_number),
        paste0("se_", regression_number),
        paste0("ci_", regression_number),
        paste0("significant_", regression_number)
    )
    return(results)
}

example6 <- get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c('bx','ethnic'),
    fixed_effects = c('grid','cohort'),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = 2,
    regression_number = 1
)
example6 # Returns a dataframe with the mean and robust standard error of the coefficients for the first regression

#example7 <- get_results( #20min to run in my computer
    data = bananas1,
    dependent_var = "weight",
    independent_var = c('bx','ethnic'),
    fixed_effects = c('grid','cohort'),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = 200,
    regression_number = 1
)
example7 # Notice how estimate bootstrap means for the coefficients come closer - thanks LLN!


## 4. Table 3
table3_reg1 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = 5,
    regression_number = 1
)
write.csv(x = table3_reg1, file = 'ReplicatedResults/table3_reg1.csv')

table3_reg2 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxp", "ethnic"),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 2
)
write.csv(x = table3_reg2, file = 'ReplicatedResults/table3_reg2.csv')

table3_reg3 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxt1", 'ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 3)
write.csv(x = table3_reg3, file = 'ReplicatedResults/table3_reg3.csv')

table3_reg4 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxt2", 'ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 4)
write.csv(x = table3_reg4, file = 'ReplicatedResults/table3_reg4.csv')

table3_reg5 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxt3",'ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 5)
write.csv(x = table3_reg5, file = 'ReplicatedResults/table3_reg5.csv')

table3_reg6 = get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx*pxt1", 'bx*pxt2', 'bx*pxt3', 'ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 6)
write.csv(x = table3_reg6, file = 'ReplicatedResults/table3_reg6.csv')

## 5. Table 4
table4_reg1 = get_results(
    data = bananas1,
    dependent_var = "gweeks",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 1)
write.csv(x = table4_reg1, file = 'ReplicatedResults/table4_reg1.csv')

table4_reg2 = get_results(
    data = bananas1,
    dependent_var = "gweeks",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 2)
write.csv(x = table4_reg2, file = 'ReplicatedResults/table4_reg2.csv')

table4_reg3 = get_results(
    data = bananas1,
    dependent_var = "preterm",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 3)
write.csv(x = table4_reg3, file = 'ReplicatedResults/table4_reg3.csv')

table4_reg4 = get_results(
    data = bananas1,
    dependent_var = "preterm",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 4)
write.csv(x = table4_reg4, file = 'ReplicatedResults/table4_reg4.csv')

table4_reg5 = get_results(
    data = bananas1,
    dependent_var = "lbw",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 5)
write.csv(x = table4_reg5, file = 'ReplicatedResults/table4_reg5.csv')

table4_reg6 = get_results(
    data = bananas1,
    dependent_var = "lbw",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 6)
write.csv(x = table4_reg6, file = 'ReplicatedResults/table4_reg6.csv')

## 6. Table 5
birth_interval = c("birth_interval1","birth_interval2","birth_interval3")

table5_reg1 = get_results(
    data = bananas2,
    dependent_var = "weight",
    independent_var = c('bx',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 1)
    table5_reg1
write.csv(x = table5_reg1, file = 'ReplicatedResults/table5_reg1.csv')

table5_reg2 = get_results(
    data = bananas2,
    dependent_var = "weight",
    independent_var = c('bx*pxp',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 2)
write.csv(x = table5_reg2, file = 'ReplicatedResults/table5_reg2.csv')

table5_reg3 = get_results(
    data = bananas2,
    dependent_var = "weight",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 3)
write.csv(x = table5_reg3, file = 'ReplicatedResults/table5_reg3.csv')

bananas2_staymom = bananas2 %>% filter(stay_mom > 0)
table5_reg4 = get_results(
    data = bananas2_staymom,
    dependent_var = "weight",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 4)
write.csv(x = table5_reg4, file = 'ReplicatedResults/table5_reg4.csv')

table5_reg5 = get_results(
    data = bananas2,
    dependent_var = "gweeks",
    independent_var = c('bx',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 5)
write.csv(x = table5_reg5, file = 'ReplicatedResults/table5_reg5.csv')

table5_reg6 = get_results(
    data = bananas2,
    dependent_var = "gweeks",
    independent_var = c('bx*pxp',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 6)
write.csv(x = table5_reg6, file = 'ReplicatedResults/table5_reg6.csv')

table5_reg7 = get_results(
    data = bananas2,
    dependent_var = "gweeks",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 7)
write.csv(x = table5_reg7, file = 'ReplicatedResults/table5_reg7.csv')


table5_reg8 = get_results(
    data = bananas2_staymom,
    dependent_var = "gweeks",
    independent_var = c('bx*pxt1','bx*pxt2','bx*pxt3',birth_interval),
    fixed_effects = c('cohort',"mfeid*grid"),
    remove_var = NA,
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 8)
write.csv(x = table5_reg8, file = 'ReplicatedResults/table5_reg8.csv')

## 7. Table 6
table6_reg1 = get_results(
    data = bananas1,
    dependent_var = "labors",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('labors'),
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 1)
write.csv(x = table6_reg1, file = 'ReplicatedResults/table6_reg1.csv')


table6_reg2 = get_results(
    data = bananas1,
    dependent_var = "nchild",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('nchild'),
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 2)
write.csv(x = table6_reg2, file = 'ReplicatedResults/table6_reg2.csv')

table6_reg3 = get_results(
    data = bananas1,
    dependent_var = "nbl",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('nbl'),
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 3)
write.csv(x = table6_reg3, file = 'ReplicatedResults/table6_reg3.csv')

table6_reg4 = get_results(
    data = bananas1,
    dependent_var = "visits",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('visits'),
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = no_bootstraps,
    regression_number = 4)
write.csv(x = table6_reg4, file = 'ReplicatedResults/table6_reg4.csv')

table6_reg5 = get_results(
    data = bananas1,
    dependent_var = "educ",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('educ'),
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 5)
write.csv(x = table6_reg5, file = 'ReplicatedResults/table6_reg5.csv')

table6_reg6 = get_results(
    data = bananas1,
    dependent_var = "private",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('private'),
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 6)
write.csv(x = table6_reg6, file = 'ReplicatedResults/table6_reg6.csv')

table6_reg7 = get_results(
    data = bananas1,
    dependent_var = "tlabor",
    independent_var = c('bx*pxp','ethnic'),
    fixed_effects = c("grid", "cohort"),
    remove_var = c('tlabor'),
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = no_bootstraps,
    regression_number = 7) 
write.csv(x = table6_reg7, file = 'ReplicatedResults/table6_reg7.csv')

