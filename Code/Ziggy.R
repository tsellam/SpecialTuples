library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(compiler)

MIN_SPARSITY <- 0.25

data <- read.arff("~/Data/Files/crime/communities.arff")
data_check <- data
#data <- rbind(data, data, data, data, data)

for (j in 20:50)
    data[[j]] <- as.character(cut(data[[j]], 3))
selection <- data$ViolentCrimesPerPop > 0.7

check_group <- function(col_set, dataset = data){

   types <- sapply(col_set, function(col) class(dataset[,col] ))
   cat("Ploting for types:")
   print(types)

   if (length(col_set) == 1){

      if (types == "numeric"){
         g <- ggplot(dataset, aes_string(x = col_set,
                                 fill='selection', color = 'selection')) +
              geom_histogram(position = "dodge", aes(y = ..density..))
         print(g)

         cat("Baseline mean equality test:\n")
         print(t.test(dataset[selection, col_set], dataset[!selection, col_set]))

         cat("Baseline variance ratio test:\n")
         print(var.test(dataset[selection, col_set], dataset[!selection, col_set]))
      }

      if (types == "factor"){

         g <- ggplot(dataset, aes_string(x = col_set,
                                      fill='selection', color = 'selection')) +
            geom_histogram(position = "dodge", aes(y = ..count../sum(..count..)))
         print(g)

         tab <- table(dataset[selection, col_set])
         ps <- table(dataset[!selection, col_set]) / sum(table(dataset[!selection, col_set]))
         print(chisq.test(tab, p =  ps))
      }

   }

   if (length(col_set) == 2){

      if (all(types == "numeric")){

         g <- ggplot(dataset, aes_string(x = col_set[1],
                                      y = col_set[2],
                                      fill = 'selection',
                                      color = 'selection')) +
            geom_point() +
            geom_smooth(method = lm)

         print(g)

      }

      if (all(types == "factor")){

         dataset$selection <- factor(selection)
         g <- ggplot(dataset, aes_string(x = col_set[1], y = col_set[2])) +
            geom_bin2d() +
            facet_wrap( ~ selection)

         print(g)

      }

   }

}

#############################
# Reading and preprocessing #
#############################
preprocess <- function(data, nbins=4){
    cat("Preprocessing in progress...")
    types <- sapply(data, class)

    # Trivial case
    if (all(types == "numeric")){
        cat("Only numeric variables\n")
        return(data)
    }

    # Doing replacements
    char_col <- names(types[types %in% c("character")])
    for (col in char_col)
        data[[col]] <- factor(data[[col]])

   # Doing additions
   num_col  <-  names(types[types %in% c("numeric")])
   num_data <- data[,num_col]
   for (col in num_col)
      num_data[[col]] <- cut(data[[col]], nbins)
   names(num_data) <- paste0("!!", names(num_data))
   data <- cbind(data, num_data)

   cat("Done!\n\n")
   return(data)
}

##############
# Statistics #
##############
# ------------------------- #
# One population statistics #
# ------------------------- #
compute_uni_stats <- function(data, exclude_cols = c()){
   # Simple sanity checks
   if (!is.data.frame(data))
      stop("Error - input is not a DF")
   if (nrow(data) < 1) stop("Empty data frame")

   # Column exclusion
   data <- data[,!names(data) %in% exclude_cols]

   # Gets and checks types types
   data_types <- sapply(data, class)
   if (!all(data_types %in% c("numeric", "factor")))
      stop("Data frame contains wrong types")

   num_cols <- names(data_types[data_types == "numeric"])
   cat_cols <- names(data_types[data_types == "factor"])

   # First numeric values
   cat("Computing univariate stats for numeric values\n")
   counts    <- sapply(data[,num_cols], function(col) length(col[!is.na(col)]))
   means     <- sapply(data[,num_cols], function(col) mean(col, na.rm = T))
   variances <- sapply(data[,num_cols], function(col) var(col, na.rm = T))
   stats_num_uni <- data_frame(column = as.character(num_cols),
                                  count  = counts,
                                  mean   = means,
                                  variance = variances)

   # Then categorical values
   cat("Computing univariate stats for categorical values\n")
   tru_cat_cols <- cat_cols[!grepl('!!', cat_cols)]
   histograms   <- lapply(data[,tru_cat_cols], function(col) table(col))
   #histograms   <- lapply(tru_cat_cols, function(col) bigtable(data, col))
   stats_cat_uni <- data_frame(column = tru_cat_cols,
                                  hist   = histograms)

   return(list(
      stats_num_uni = stats_num_uni,
      stats_cat_uni = stats_cat_uni
   ))
}

# Updates univariate statics for numeric data
rollback_num_uni <- function(all_num_uni, sel_num_uni){
   cat("Un-updates preprocessed univariate numeric statistics\n")
   exc_num_uni <- select(sel_num_uni,
                            column,
                            sel_count = count,
                            sel_mean  = mean,
                            sel_variance = variance) %>%
      inner_join(all_num_uni, by = 'column') %>%
      mutate(exc_count = count - sel_count,
             exc_mean  = (count * mean - sel_count * sel_mean)
             / exc_count) %>%
      mutate(M = variance * (count-1),
             sel_M = sel_variance * (sel_count-1),
             exc_M = M - sel_M - (sel_mean - exc_mean)^2 *
                sel_count * exc_count / count,
             exc_variance = exc_M / (exc_count - 1)) %>%
      select(column,
             count    = exc_count,
             mean     = exc_mean,
             variance = exc_variance)

   return(exc_num_uni)
}

# Updates univariate statistics for categorical data
rollback_cat_uni <- function(all_cat_uni, sel_cat_uni){
   cat("Un-updates preprocessed univariate categorical statistics\n")
   exc_cat_uni <- select(sel_cat_uni,
                            column,
                            sel_hist = hist) %>%
                     inner_join(all_cat_uni, by='column') %>%
                     rowwise() %>%
                     summarise(column = column, hist = list(hist - sel_hist))

   return(exc_cat_uni)
}


# -------------------------- #
# Two populations statistics #
# -------------------------- #
# WARNING: the populations to compare may have different number of columns!

# Computes dissimilarities for univariate numerical values
mean_magnitude <- function(m1, m2, v1, v2, n1, n2){
   (m1 - m2) / sqrt(v2)
}

mean_test <- function(mean1, mean2, var1, var2, n1, n2){
   W <- (mean1 - mean2) / sqrt(var1/n1 + var2/n2)
   p <- 2*pnorm(-abs(W))
   return(p)
}

variance_magnitude <- function(var1, var2)
   sqrt(var2/var1) - 1

variance_test <- function(var1, var2, n1, n2){
   # Making sure than var1 <= var2
   to_switch <- var1 > var2

   tmp <- var1[to_switch]
   var1[to_switch] <- var2[to_switch]
   var2[to_switch] <- tmp

   tmp <- n1[to_switch]
   n1[to_switch] <- n2[to_switch]
   n2[to_switch] <- tmp

   # Running the test
   ratios <- var2 / var1
   p <- 2*(1 - pf(ratios, (n2-1), (n1-1)))
}

scores_num_uni <- function(sel_num_uni, exc_num_uni){
   cat("Computing two-population univariate tests, numerical data\n")
   side_by_side <- select(sel_num_uni,
                    column,
                    sel_count = count,
                    sel_mean = mean,
                    sel_variance = variance) %>%
               inner_join(exc_num_uni, by='column') %>%
               rename(exc_count = count,
                      exc_mean = mean,
                      exc_variance = variance)

   stats <- side_by_side %>%
            mutate(p_mean_diff = mean_test(sel_mean, exc_mean,
                                           sel_variance, exc_variance,
                                           sel_count, exc_count),
                   m_mean_diff = mean_magnitude(sel_mean, exc_mean,
                                                sel_variance, exc_variance,
                                                sel_count, exc_count))

   stats <- stats %>%
             mutate(p_variance_ratio = variance_test(sel_variance,
                                                     exc_variance,
                                                     sel_count,
                                                     exc_count),
                   m_variance_ratio = variance_magnitude(sel_variance,
                                                         exc_variance))
   stats

}


# Computes dissimilarities for uni categorical data
cat_single_test_magn <- function(hists1, hists2){

   # In this case, hists2 describe the expected distributions
   out <- sapply(1:length(hists2), function(i, hists1, hists2){
      hist1 <- hists1[[i]]
      hist2 <- hists2[[i]]

      if(length(hist1) <= 1) return(NA)

      E <- hist2  * sum(hist1) / sum(hist2)
      X2 <- sum((hist1 - E)^2 / E)

      # Pearson's test
      P <- 1 - pchisq(X2, length(hist2) - 1)
      # Cramer's V... or at least I hope :)
      M <- sqrt( X2 / ((length(hist2) - 1) *  sum(hist1)))

      c('p_chi2_test' = P, 'm_chi2_test' = M, 'X2'=X2)

   }, hists1, hists2)

#    checks <- sapply(1:length(hists2), function(i, hists1, hists2){
#       hist1 <- hists1[[i]]
#       hist2 <- hists2[[i]]
#
#       P <- hist2 / sum(hist2)
#       out <- chisq.test(hist1, p=P)
#       c('p_check' = out$p.value, 'X_check' = out$statistic)
#    }, hists1, hists2)
#    browser()

   out
}

scores_cat_uni <- function(sel_cat_uni, exc_cat_uni){
   cat("Computing two-population univariate tests, categorical data\n")

   side_by_side <- sel_cat_uni %>%
      rename(sel_hist = hist) %>%
      inner_join(exc_cat_uni, by='column') %>%
      rename(exc_hist = hist)

   stats <- cat_single_test_magn(side_by_side$sel_hist, side_by_side$exc_hist)
   stats <- cbind(side_by_side, t(stats))

   stats
}

########################
# Bivariate statistics #
########################
#------------------------------#
# Single Population Statistics #
#------------------------------#
o_calc_cramer_V <- function(cross_tabs){
   sapply(cross_tabs, function(tab){

      # First, filter our empty rows/columns
      null_rows <- apply(tab, 1, sum) == 0
      null_cols <- apply(tab, 2, sum) == 0
      tab <- tab[!null_rows, !null_cols, drop=FALSE]

      # Checks trivial cases
      V_norm <- min(nrow(tab) - 1, ncol(tab) - 1)
      total  <- sum(tab)
      if (V_norm < 1 | total < 10)
         return(
            c('p_chi2'    = NA,
              'Chi_chi2'  = NA,
              'm_CramersV'= NA,
              'df'        = NA,
              'V_norm'    = NA,
              'count'     = NA
            )
         )

      # Computes the actual stats
      res <- chisq.test(tab)
      V   <- sqrt(res$statistic / (total * V_norm))

      # Checks Chi-squared assumptions
       p_chi2 <- if (sum(tab < 5) > 0.3*length(tab)){
          NA
       } else {
          res$p.value
       }

      c('p_chi2'    = p_chi2,
        'Chi_chi2'  = res$statistic,
        'm_CramersV'= V,
        'df'        = res$parameter,
        'V_norm'    = V_norm,
        'count'     = total
        )
   })
}
#calc_cramer_V <- cmpfun(o_calc_cramer_V, c('optimize'=3))
calc_cramer_V <- o_calc_cramer_V

compute_bi_stats <- function(data, uni_stats, exclude_cols = NULL){
   # Simple sanity checks
   if (!is.data.frame(data))
      stop("Error - input is not a DF")
   if (nrow(data) < 1) stop("Empty data frame")

   # Column exclusion
   data <- data[,!names(data) %in% exclude_cols]

   # Gets and checks types
   data_types <- sapply(data, class)
   if (!all(data_types %in% c("numeric", "factor")))
      stop("Data frame contains wrong types")

   num_cols <- names(data_types[data_types == "numeric"])
   cat_cols <- names(data_types[data_types == "factor"])

   # FIRST, NUMERIC VALUES
   cat("Computing correlations for numeric values\n")

   # Checking sparse columns
   sparse_cols <- sapply(data[,num_cols], function(c)
      sum(is.na(c)) / nrow(data) > MIN_SPARSITY
   )
   if (any(sparse_cols))
      warning("Lots of missing values, pairwise correlations may be off\n")

   # Computes
   cov_mat <- cov(data[num_cols], use = "pairwise.complete.obs")

   # Formats
   stats_num_bi <- data.frame(cov_mat) %>%
                   cbind(
                      data.frame(column1 = row.names(cov_mat),
                                 stringsAsFactors = F)) %>%
                   gather(column2, covariance, -column1) %>%
                   mutate(column2 = as.character(column2)) %>%
                   select(column1, column2, covariance) %>%
                   filter(column1 < column2)

   # Augments with single stats
   stats_num_bi <- stats_num_bi %>%
                  inner_join(uni_stats$stats_num_uni, c("column1"="column")) %>%
                  rename(col1_count = count,
                         col1_mean  = mean,
                         col1_variance = variance) %>%
                  inner_join(uni_stats$stats_num_uni, c("column2"="column")) %>%
                  rename(col2_count    = count,
                         col2_mean     = mean,
                         col2_variance = variance) %>%
                  mutate(count = pmin(col1_count, col2_count)) %>%
                  select(-col1_count, -col2_count)


   # AND THEN, CATEGORICAL VALUES
   cat("Computing contingency tables for categorical values\n")
   cat_cols <- sort(cat_cols, decreasing = T)

   # Big calculations
   count_tables <- lapply(1:(length(cat_cols) - 1), function(j1){
      col1 <- cat_cols[j1]
      if (grepl('!!*', col1)) return(NULL)
      lapply((j1+1):length(cat_cols), function(j2){
         table(data[,cat_cols[c(j1,j2)]])
      })
   })

   # Formatting
   count_tables <- count_tables[!sapply(count_tables, is.null)]
   count_tables <- unlist(count_tables, recursive = F)
   col_names <- sapply(count_tables, function(tab){
      names(attributes(tab)$dimnames)
   })
   stats_cat_bi <- data.frame(t(col_names), stringsAsFactors = F)
   names(stats_cat_bi) <- c('column2', 'column1')
   stats_cat_bi[['cross_tabs']] <- count_tables

   # Augments with Chi-2 tests
   add_stats <- t(calc_cramer_V(count_tables))
   colnames(add_stats) <- c('p_Chi2', 'Chi2', 'CramersV', 'df', 'V_norm_fact', 'count')
   stats_cat_bi <- cbind(stats_cat_bi, add_stats)

   return(list(
       stats_num_bi = stats_num_bi,
       stats_cat_bi = stats_cat_bi
    ))
}

rollback_num_bi  <- function(all_num_bi, sel_num_bi,
                             sel_num_uni, exc_num_uni){

   cat("Un-updates preprocessed bivariate numeric statistics\n")

   exc_num_bi <- sel_num_bi %>%
                  rename(sel_covariance = covariance,
                         sel_count = count,
                         col1_sel_mean = col1_mean,
                         col2_sel_mean = col2_mean) %>%
                  select(-col1_variance, -col2_variance) %>%
                  inner_join(all_num_bi, c("column1", "column2")) %>%
                  rename(all_covariance = covariance) %>%
                  select(-col1_mean, -col1_variance,
                         -col2_mean, -col2_variance,
                         -count)

    exc_num_bi <- exc_num_bi %>%
                   inner_join(exc_num_uni, c("column1"="column")) %>%
                   rename(col1_exc_count    = count,
                          col1_exc_mean     = mean,
                          col1_exc_variance = variance) %>%
                   inner_join(exc_num_uni, c("column2"="column")) %>%
                   rename(col2_exc_count    = count,
                          col2_exc_mean     = mean,
                          col2_exc_variance = variance) %>%
                   mutate(exc_count = pmin(col1_exc_count, col2_exc_count)) %>%
                   select(-col1_exc_count, -col2_exc_count)

    exc_num_bi <- exc_num_bi %>%
                   mutate(C_all = (sel_count + exc_count - 1) * all_covariance,
                          C_sel = (sel_count - 1) * sel_covariance,
                          diffs = (col1_exc_mean - col1_sel_mean) *
                                  (col2_exc_mean - col2_sel_mean) *
                                  (exc_count*sel_count) / (exc_count+sel_count),
                          covariance = (C_all-C_sel-diffs) / (exc_count-1)) %>%
                   select(column1, column2,
                          col1_mean = col1_exc_mean,
                          col2_mean = col2_exc_mean,
                          covariance,
                          col1_variance = col1_exc_variance,
                          col2_variance = col2_exc_variance,
                          count = exc_count)
   return(exc_num_bi)
}

rollback_cat_bi <- function(all_cat_bi, sel_cat_bi){
   cat("Un-updates preprocessed bivariate categorical statistics\n")

   exc_cat_bi <- sel_cat_bi %>%
      inner_join(all_cat_bi, c("column1", "column2")) %>%
      rename(sel_cross_tabs = cross_tabs.x,
             all_cross_tabs = cross_tabs.y)  %>%
             rowwise() %>%
             summarise(column1, column2,
                       cross_tabs = list(all_cross_tabs - sel_cross_tabs))

   # Augments with Chi-2 tests
   add_stats <- t(calc_cramer_V(exc_cat_bi$cross_tabs))
   colnames(add_stats) <- c('p_Chi2', 'Chi2', 'CramersV', 'df',
                            'V_norm_fact', 'count')
   exc_cat_bi <- cbind(exc_cat_bi, add_stats)

   return(exc_cat_bi)
}

scores_num_bi <- function(sel_num_bi, exc_num_bi){

   cat("Computing two-population bivariate tests, numerical data\n")
   stats <- sel_num_bi %>%
                   inner_join(exc_num_bi, c('column1', 'column2'))

   # Effect size
   stats <- stats %>%
      mutate(sel_corr = covariance.x /
                (sqrt(col1_variance.x * col2_variance.x)),
             exc_corr = covariance.y /
                (sqrt(col1_variance.y * col2_variance.y)),
             m_corr_diff = sel_corr - exc_corr)

   # Testing
   stats <- stats %>%
            mutate(sel_z = atanh(sel_corr),
                   exc_z = atanh(exc_corr),
                   sel_z_var = ifelse(count.x > 10, 1 / (count.x - 3), NA),
                   exc_z_var = ifelse(count.y > 10, 1 / (count.y - 3), NA)) %>%
            mutate(t_corr_sel = 2 * pnorm(-abs(sel_z / sqrt(sel_z_var))),
                   t_corr_exc = 2 * pnorm(-abs(exc_z / sqrt(exc_z_var))),
                   test_z_diff = (sel_z - exc_z) /
                              sqrt( sel_z_var + exc_z_var ),
                   t_corr_diff = 2 * pnorm(-abs(test_z_diff)))

   stats <- select(stats, column1, column2,
                   sel_corr, exc_corr, t_corr_sel, t_corr_exc,
                   m_corr_diff, t_corr_diff)

}


test_Chi2_ratio <- function(V_sel, V_exc, df_sel, df_exc,
                            fact_sel, fact_exc,
                            count_sel, count_exc){

   # Fisher / Wilson & Hilferty transform
   mean_z_sel <- sqrt((df_sel - 0.5)/(fact_sel * count_sel))
   mean_z_exc <- sqrt((df_exc - 0.5)/(fact_exc * count_exc))
   var_z_sel <- 1 / (2 * fact_sel * count_sel)
   var_z_exc <- 1 / (2 * fact_exc * count_exc)

   Z <- ((V_sel - V_exc) - (mean_z_sel - mean_z_exc)) /
      sqrt(var_z_sel + var_z_exc)

   P <- 2*pnorm(-abs(Z))
}

scores_cat_bi <- function(sel_cat_bi, exc_cat_bi){

   cat("Computing two-population bivariate tests, categorical data\n")
   stats <- sel_cat_bi %>%
            inner_join(exc_cat_bi, c('column1', 'column2'))

   stats <- mutate(stats, m_CramersV_diff = CramersV.x - CramersV.y,
                          t_CramersV_diff = test_Chi2_ratio(
                                                CramersV.x, CramersV.y,
                                                df.x, df.y,
                                                V_norm_fact.x, V_norm_fact.y,
                                                count.x, count.y))
}

#########################
# Main comment function #
#########################
comment <- function(selection, data, offline_uni_stats, offline_bi_stats){

   # Sanity checks
   if(!is.logical(selection) || !is.data.frame(data) ||
      !is.list(offline_uni_stats) || !is.list(offline_bi_stats))
      stop("Wrong incoming data to function comment")


   # Materializes selection and compute statistics
   cat("\nMaterializes selection... ")
   sel_data  <- data[selection,]
   t_sparse_columns <- sapply(sel_data, function(col)
      sum(is.na(col)) > MIN_SPARSITY
   )
   if (any(t_sparse_columns))
      cat("Ignoring", sum(t_sparse_columns), "sparse columns.\n")
   sparse_columns <- names(sel_data)[t_sparse_columns]


   # Univariate statistics
   all_num_uni <- offline_uni_stats$stats_num_uni
   all_cat_uni <- offline_uni_stats$stats_cat_uni

   cat("* Univariate statistics....\n")
   sel_uni_stats <- compute_uni_stats(sel_data, exclude_cols = sparse_columns)
   sel_num_uni <- sel_uni_stats$stats_num_uni
   sel_cat_uni <- sel_uni_stats$stats_cat_uni

   # Un-unpdate the full table statistics
   exc_num_uni <- rollback_num_uni(all_num_uni, sel_num_uni)
   exc_cat_uni <- rollback_cat_uni(all_cat_uni, sel_cat_uni)

   # Computes zig-components
   zig_num_uni <- scores_num_uni(sel_num_uni, exc_num_uni)
   zig_cat_uni <- scores_cat_uni(sel_cat_uni, exc_cat_uni)


   # Bivariate statistics
   all_num_bi  <- offline_bi_stats$stats_num_bi
   all_cat_bi  <- offline_bi_stats$stats_cat_bi %>%
                  filter(is.finite(p_Chi2))

   cat("* Bivariate statistics....\n")
   sel_bi_stats <- compute_bi_stats(sel_data,
                                    sel_uni_stats,
                                    exclude_cols = sparse_columns)
   sel_num_bi <- sel_bi_stats$stats_num_bi
   sel_cat_bi <- sel_bi_stats$stats_cat_bi
   # Removes useless entries
   sel_cat_bi <- filter(sel_cat_bi, is.finite(Chi2), is.finite(p_Chi2))

   # Un-updates the full table statistics
   exc_num_bi <- rollback_num_bi(all_num_bi, sel_num_bi,
                                 sel_num_uni, exc_num_uni)
   exc_cat_bi <- rollback_cat_bi(all_cat_bi, sel_cat_bi)

   # Computes zig-components
   zig_num_bi <- scores_num_bi(sel_num_bi, exc_num_bi)
   zig_cat_bi <- scores_cat_bi(sel_cat_bi, exc_cat_bi)

}

# Workflow
data  <- preprocess(data)
CLOCK1 <- proc.time()['elapsed']
offline_uni_stats   <- compute_uni_stats(data)
offline_bi_stats <- compute_bi_stats(data, offline_uni_stats)
comments <- comment(selection, data, offline_uni_stats, offline_bi_stats)
CLOCK2 <- proc.time()['elapsed']
print(CLOCK2 - CLOCK1)
