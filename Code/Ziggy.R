library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)

MIN_SPARSITY <- 0.25

zig_coefficients <- c(
   'm_mean_diff' = 1,
   'm_variance_ratio' = .5,
   'm_CramersV' = 1.5,
   'm_corr_diff' = 1,
   'm_CramersV_diff' = 1.5
)


#####################################################
# Functions for quick inspection of Ziggy's results #
#####################################################
check_group <- function(col_set, dataset = data){

   types <- sapply(col_set, function(col) class(dataset[,col] ))
   cat("Ploting for types:")
   print(types)

   if (length(col_set) == 1){

      if (types %in% c("numeric", "integer")){
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

      if (all(types %in% c("numeric", "integer"))){

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
preprocess <- function(data,  nbins=4){
    cat("Preprocessing in progress...")


   sparse <- sapply(data, function(col)
      sum(is.na(col)) > MIN_SPARSITY * length(col)
   )
   if (any(sparse)){
      cat("Removing", sum(sparse), "sparse columns\n")
      data <- data[!sparse]
   }
   flat <- sapply(data, function(col)
      (is.factor(col) | is.character(col)) &
         length(unique(col)) > 1024
   )
   if (any(flat)){
      cat("Removing", sum(flat), "flat columns\n")
      data <- data[!flat]
   }

    types <- sapply(data, class)

    # Trivial case
    if (all(types %in% c("numeric", "integer"))){
        cat("Only numeric variables\n")
        return(data)
    }

    # Doing replacements
    char_col <- names(types[types %in% c("character")])
    for (col in char_col)
        data[[col]] <- factor(data[[col]])

   # Doing additions
   if (any(types %in% c('numeric', 'integer'))){
      num_col  <-  names(types[types %in% c("numeric", "integer")])
      num_data <- data[,num_col]
      for (col in num_col)
         num_data[[col]] <- cut(data[[col]], nbins)
      names(num_data) <- paste0(names(num_data), '!!NUM')
      data <- cbind(data, num_data)
   }
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
   if (!all(data_types %in% c("numeric", "integer", "factor")))
      stop("Data frame contains wrong types")

   num_cols <- names(data_types[data_types %in% c("numeric", "integer")])
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
   tru_cat_cols <- cat_cols[!grepl('!!NUM$', cat_cols)]
   histograms   <- lapply(data[,tru_cat_cols, drop=FALSE], function(col) table(col))
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

   # Trivial cases
   if (nrow(all_num_uni) < 1 | nrow(sel_num_uni) < 1) return(all_num_uni)

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

   # Trivial cases
   if (nrow(all_cat_uni) < 1 | nrow(sel_cat_uni) < 1) return(all_cat_uni)

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
   sqrt(var1/var2) - 1

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

   # Trivial cases
   if (nrow(sel_num_uni) < 1 | nrow(exc_num_uni) < 1)
      return(data.frame(
         'column' = character(0),
         'sel_count' = numeric(0),
         'sel_mean' = numeric(0),
         'sel_variance' = numeric(0),
         'exc_count' = numeric(0),
         'exc_mean' = numeric(0),
         'exc_variance' = numeric(0),
         'p_mean_diff' = numeric(0),
         'm_mean_diff' = numeric(0),
         'p_variance_ratio' = numeric(0),
         'm_variance_ratio' = numeric(0)
   ))

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

      # Cramer's V, inspired by the lsr package
      min_E <- which.min(E)
      dev <- E
      dev[min_E] <- sum(hist1) - dev[min_E]
      max_X2 <- sum(dev^2/E)
      M <- sqrt(X2/max_X2)

     #M <- sqrt(X2/(sum(hist1) * (length(hist1) - 1)))

      c('p_chi2_test' = P, 'm_CramersV' = M, 'X2'=X2)

   }, hists1, hists2)

   out
}

scores_cat_uni <- function(sel_cat_uni, exc_cat_uni){

   # Trivial cases
   if (nrow(sel_cat_uni) < 1 | nrow(exc_cat_uni) < 1)
      return(data.frame(
         'column' = character(0),
         'sel_hist' = list(),
         'exc_hist' = list(),
         'p_chi2_test' = numeric(0),
         'm_CramersV' = numeric(0),
         'X2' = numeric(0)
      ))

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
compute_bi_stats_num <- function(data, single_stats){
   cat("Computing correlations for numeric values\n")

   # Checking sparse columns
   sparse_cols <- sapply(data, function(c)
      sum(is.na(c)) / nrow(data) > MIN_SPARSITY
   )
   if (any(sparse_cols))
      warning("Lots of missing values, pairwise correlations may be off\n")

   # Computes
   cov_mat <- cov(data, use = "pairwise.complete.obs")

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
      inner_join(single_stats, c("column1"="column")) %>%
      rename(col1_count = count,
             col1_mean  = mean,
             col1_variance = variance) %>%
      inner_join(single_stats, c("column2"="column")) %>%
      rename(col2_count    = count,
             col2_mean     = mean,
             col2_variance = variance) %>%
      mutate(count = pmin(col1_count, col2_count)) %>%
      select(-col1_count, -col2_count)

}





calc_cramer_V <- function(cross_tabs){
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
      res <- suppressWarnings(chisq.test(tab))
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

compute_bi_stats_cat <- function(data){
   cat("Computing contingency tables for categorical values\n")

   # Prepares data structure
   cat_cols <- sort(names(data))
   stats_cat_bi <- combn(cat_cols, 2)
   stats_cat_bi <- as.data.frame(t(stats_cat_bi), stringsAsFactors = F)
   names(stats_cat_bi) <- c('column1', 'column2')
   stats_cat_bi <- stats_cat_bi %>% filter(!(grepl('!!NUM$', column1) &
                                             grepl('!!NUM$', column2)))


   tables <- lapply(1:nrow(stats_cat_bi), function(r){
      cols <- as.character(stats_cat_bi[r,])
      table(data[,cols])
   })
   stats_cat_bi[['cross_tabs']] <- tables

#
#    cat_cols <- sort(names(data))
#    pure_cat_cols <- cat_cols[!grepl("!!", cat_cols)]
#
#    # Big calculations
#    count_tables <- lapply(cat_cols, function(col1){
#       lapply(pure_cat_cols[pure_cat_cols > col1], function(col2){
#          table(data[,c(col1, col2)])
#       })
#    })
#
#    # Formatting
#    count_tables <- count_tables[!sapply(count_tables, is.null)]
#    count_tables <- unlist(count_tables, recursive = F)
#    col_names <- sapply(count_tables, function(tab){
#       names(attributes(tab)$dimnames)
#    })
#    stats_cat_bi <- data.frame(t(col_names), stringsAsFactors = F)
#    names(stats_cat_bi) <- c('column1', 'column2')
#    stats_cat_bi[['cross_tabs']] <- count_tables

   # Augments with Chi-2 tests
   add_stats <- t(calc_cramer_V(stats_cat_bi$cross_tabs))
   colnames(add_stats) <- c('p_Chi2', 'Chi2', 'CramersV', 'df', 'V_norm_fact', 'count')
   stats_cat_bi <- cbind(stats_cat_bi, add_stats)
}





compute_bi_stats <- function(data, uni_stats,
                             exclude_cols = NULL, empty = FALSE){
   # Simple sanity checks
   if (!is.data.frame(data))
      stop("Error - input is not a DF")
   if (nrow(data) < 1) stop("Empty data frame")

   # Column exclusion
   data <- data[,!names(data) %in% exclude_cols]

   # Gets and checks types
   data_types <- sapply(data, class)
   if (!all(data_types %in% c("numeric", "integer", "factor")))
      stop("Data frame contains wrong types")

   num_cols <- names(data_types[data_types %in% c("numeric", "integer")])
   cat_cols <- names(data_types[data_types == "factor"])

   # First, numeric values
   stats_num_bi <- if(length(num_cols) > 0 & !empty)
      compute_bi_stats_num(data[num_cols], uni_stats$stats_num_uni)
   else
      data.frame(
         'column1' = character(0),
         'column2' = character(0),
         'covariance' = numeric(0),
         'col1_mean' = numeric(0),
         'col1_variance' = numeric(0),
         'col2_mean' = numeric(0),
         'col2_variance' = numeric(0),
         'count' = numeric(0)
      )

   # Then, categorical data
   stats_cat_bi <- if (length(cat_cols) > 0 & !empty)
      compute_bi_stats_cat(data[,cat_cols])
   else data.frame('column1' = character(0),
                 'column2' = character(0),
                 'cross_tabs' = list(),
                 'p_Chi2' = numeric(0),
                 'Chi2' = numeric(0),
                 'CramersV'= numeric(0),
                 'df'= numeric(0),
                 'V_norm_fact'= numeric(0),
                 'count'= numeric(0))

   return(list(
       stats_num_bi = stats_num_bi,
       stats_cat_bi = stats_cat_bi
    ))
}









rollback_num_bi  <- function(all_num_bi, sel_num_bi,
                             sel_num_uni, exc_num_uni){

   # Trivial case
   if (nrow(all_num_bi) < 1 | nrow(all_num_bi) < 1)
      return(all_num_bi)

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

   # Trivial case
   if (nrow(all_cat_bi) < 1 | nrow(all_cat_bi) < 1)
      return(all_cat_bi)

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







#----------------------------#
# Two Populations Statistics #
#----------------------------#
scores_num_bi <- function(sel_num_bi, exc_num_bi){

   # Trivial case
   if (nrow(sel_num_bi) < 1 | nrow(exc_num_bi) < 1)
      return(data.frame(
         'column1' = character(0),
         'column2' = character(0),
         'sel_corr' = numeric(0),
         'exc_corr' = numeric(0),
         't_corr_sel' = numeric(0),
         't_corr_exc' = numeric(0),
         'm_corr_diff' = numeric(0),
         't_corr_diff' = numeric(0)
      ))


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

   stats
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

   # Trivial case
   if (nrow(sel_cat_bi) < 1 | nrow(exc_cat_bi) < 1)
      data.frame(
         'column1' = character(0),
         'column2' = character(0),
         'sel_CramersV' = numeric(0),
         'sel_p_Chi2' = numeric(0),
         'exc_CramersV' = numeric(0),
         'exc_p_Chi2' = numeric(0),
         'm_CramersV_diff' = numeric(0),
         't_CramersV_diff' = numeric(0)
      )

   cat("Computing two-population bivariate tests, categorical data\n")
   stats <- sel_cat_bi %>%
            inner_join(exc_cat_bi, c('column1', 'column2'))

   stats <- mutate(stats, m_CramersV_diff = CramersV.x - CramersV.y,
                          t_CramersV_diff = test_Chi2_ratio(
                                                CramersV.x, CramersV.y,
                                                df.x, df.y,
                                                V_norm_fact.x, V_norm_fact.y,
                                                count.x, count.y))

   select(stats,
          column1, column2,
          sel_CramersV = CramersV.x,
          sel_p_Chi2 = p_Chi2.x,
          exc_CramersV = CramersV.y,
          exc_p_Chi2 = p_Chi2.y,
          m_CramersV_diff,
          t_CramersV_diff)

}





##########################
# Wrapper for statistics #
##########################
zig_score <- function(selection, data, offline_uni_stats, offline_bi_stats){

   # Sanity checks
   if(!is.logical(selection) || !is.data.frame(data) ||
      !is.list(offline_uni_stats) || !is.list(offline_bi_stats))
      stop("Wrong incoming data to function comment")


   # Materializes selection and compute statistics
   cat("Materializes selection...\n")
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

   return(list(
      zig_num_uni = zig_num_uni,
      zig_cat_uni = zig_cat_uni,
      zig_num_bi  = zig_num_bi,
      zig_cat_bi  = zig_cat_bi
   ))
}

########################
# Aggregation function #
########################
zig_aggregate <- function(zig_components, zig_coef){

    if (!length(zig_components) == 4 | !is.numeric(zig_coef))
       stop("Wrong arguments to aggregation function")

      cat("* Aggregates all zig-components\n")

      #Single, numerical zig components
      is <- names(zig_coef) %in% names(zig_components$zig_num_uni)
      coef <- zig_coef[is]

      zig_mat <- zig_components$zig_num_uni[,names(coef), drop = F]
      zig_mat <- abs(scale(zig_mat))
      zig <- zig_mat %*% coef
      zig_components$zig_num_uni <- mutate(zig_components$zig_num_uni,
                                           zig = as.numeric(zig))


      # Single, categorical components
      is <- names(zig_coef) %in% names(zig_components$zig_cat_uni)
      coef <- zig_coef[is]

      zig_mat <- zig_components$zig_cat_uni[,names(coef), drop = F]
      zig_mat <- abs(scale(zig_mat))
      zig <- zig_mat %*% coef
      zig_components$zig_cat_uni <- mutate(zig_components$zig_cat_uni,
                                           zig = as.numeric(zig))


      # Bivariate, numerical zig components
      # Finds relevant coefficients
      is <- names(zig_coef) %in% names(zig_components$zig_num_bi)
      coef <- zig_coef[is]

      # Computes dot-product
      zig_mat <- zig_components$zig_num_bi[,names(coef), drop = F]
      zig_mat <- abs(scale(zig_mat))
      zig <- zig_mat %*% coef
      zig_components$zig_num_bi <- mutate(zig_components$zig_num_bi,
                                          zig = as.numeric(zig))



      # Bivariate, categorical zig components
      is <- names(zig_coef) %in% names(zig_components$zig_cat_bi)
      coef <- zig_coef[is]

      zig_mat <- zig_components$zig_cat_bi[,names(coef), drop = F]
      zig_mat <- abs(scale(zig_mat))
      zig <- zig_mat %*% coef
      zig_components$zig_cat_bi <- mutate(zig_components$zig_cat_bi,
                                          zig = as.numeric(zig))


      zig_components
}

########################
# View search function #
########################
change_point <- function(o_series){

   # Trivial case
   if (length(o_series) < 4) return(length(o_series))

   # Prep data
   series <- cumsum(o_series)

   # Statistics
   means <- sapply(2:length(series)-1, function(i){
      c(
         m1 = mean(series[1:i]),
         m2 = mean(series[(i+1):length(series)])
      )
   })
   pooled_sd <- sd(series)

   # Actual test
   likelihoods <- sapply(2:length(series)-1, function(i){

      l1 <- dnorm(series[1:i],
                  means['m1', i],
                  pooled_sd,
                  log = T)

      l2 <- dnorm(series[(i+1):length(series)],
                  means['m2', i],
                  pooled_sd,
                  log = T)

      sum(l1, l2)
   })

   return(which.max(likelihoods))

}




search_views <- function(K, D, zig_scores,
                         offline_depdendencies,
                         hard_dep_threshold = NULL,
                         soft_dep_threshold = NULL,
                         fill_NAs = TRUE,
                         auto_stop = FALSE){
   if (!length(zig_scores) == 4 |
       !is.numeric(K)  | K < 1 |
       !is.numeric(D)  | D < 1 )
      stop("Wrong arguments to view search function")


   cat("* View search\n")

   # Prepares Zig-Scores
   uni_zigs <- rbind(
      select(zig_scores$zig_num_uni, column, zig),
      select(zig_scores$zig_cat_uni, column, zig)
   )
   bi_zigs <- rbind(
      select(zig_scores$zig_num_bi, column1, column2, zig),
      select(zig_scores$zig_cat_bi, column1, column2, zig) %>%
         mutate(column1 = sub('!!NUM$', '', column1),
                column2 = sub('!!NUM$', '', column2))
   )

   # Prepares dependencies
   num_dependencies <- offline_depdendencies$stats_num_bi %>%
         mutate(dependency = abs(covariance/
                                 sqrt(col1_variance * col2_variance))) %>%
         select(column1, column2, dependency)

   cat_dependencies <- offline_depdendencies$stats_cat_bi %>%
         select(column1, column2, dependency = abs(CramersV)) %>%
      mutate(column1 = sub('!!NUM$', '', column1),
             column2 = sub('!!NUM$', '', column2))

   self_depencies <- data_frame(
      column1 = uni_zigs$column,
      column2 =  uni_zigs$column,
      dependency = 1
   )

   dependencies <- rbind(num_dependencies, cat_dependencies, self_depencies)


   if (fill_NAs){
      # Fills missing values
      all_combis <- combn(sort(uni_zigs$column), 2)
      all_combis <- as.data.frame(t(all_combis), stringsAsFactors = F)
      colnames(all_combis) <- c('column1', 'column2')
      bi_zigs <- all_combis %>%
                  left_join(bi_zigs, by=c('column1', 'column2'))
      bi_zigs$zig[is.na(bi_zigs$zig)] <- 0
   }

   # Generates list of candidate edges
   candidates <- bi_zigs %>%
                  inner_join(uni_zigs, by = c('column1' = 'column')) %>%
                  rename(bi_zig = zig.x, zig1 = zig.y) %>%
                  inner_join(uni_zigs, by = c('column2' = 'column')) %>%
                  rename(zig2 = zig)


   # Here comes the MEAT!
   zigs  <- list()
   views <- list(character(0))

   for (k in 1:K){

      col_zigs <- c()
      view     <- data_frame('column' =  character())
      old_columns  <- data_frame('column' = unlist(views))

      # Removes redundant candidates - hard threshold
      if (!is.null(hard_dep_threshold)){
         left_exclude <- old_columns %>%
                        inner_join(dependencies, by = c('column' = 'column1')) %>%
                        filter(dependency > hard_dep_threshold) %>%
                        select(column = column2)

         right_exclude <- old_columns %>%
                        inner_join(dependencies, by = c('column' = 'column2')) %>%
                        filter(dependency > hard_dep_threshold) %>%
                        select(column = column1)

         all_exclude <- rbind_list(left_exclude, right_exclude)

         candidates <- candidates %>%
            anti_join(all_exclude, by=c('column1'='column')) %>%
            anti_join(all_exclude, by=c('column2'='column'))

         if (nrow(candidates) < D){
            cat("Not enough columns to explore, interrupting for k =", k, "\n")
            break
         }
      }

      # Trivial case !! to update
      if (D == 1){

         view_cols <- rbind_list(
                        select(candidates, column = column1),
                        select(candidates, column = column2)) %>%
                     inner_join(uni_zigs, by = c('column')) %>%
                     arrange(desc(zig)) %>%
                     slice(1)

         views[[k]] <- select(view_cols, column)
         zigs[[k]]  <- sum(view_cols$zig)

         next
      }

      # Computes redundancy with old columns - in preparation for soft threshold
      if (!is.null(soft_dep_threshold)){

         left_dependencies <- dependencies %>%
            inner_join(old_columns, by = c('column1' = 'column')) %>%
            group_by(column2) %>%
            summarise(sum_dependency = sum(dependency),
                      n_dependencies = length(dependency)) %>%
            select(column = column2, sum_dependency, n_dependencies)

         right_dependencies <- dependencies %>%
            filter(column1 != column2) %>%
            inner_join(old_columns, by = c('column2' = 'column')) %>%
            group_by(column1) %>%
            summarise(sum_dependency = sum(dependency),
                      n_dependencies = length(dependency)) %>%
            select(column = column1, sum_dependency, n_dependencies)

         all_dep <- rbind_list(left_dependencies, right_dependencies) %>%
            group_by(column) %>%
            summarise(sum_dependency = sum(sum_dependency),
                      n_dependency   = sum(n_dependencies))
      }


      # Body of the search
      for (d in 1:(D-1)){

         # Initializes candidate list
         filtered_candidates <- candidates

         # Excludes the redundant ones - soft threshold
         if (!is.null(soft_dep_threshold)){

            prev_dependency <- all_dep %>%
                              inner_join(view, by='column') %>%
                              summarise(sum_dependency = sum(sum_dependency),
                                        n_dependency   = sum(n_dependency))

            current_sum <- prev_dependency$sum_dependency[1]
            current_n   <- prev_dependency$n_dependency[1]

            to_exclude <- all_dep %>%
                           mutate(new_avg_dep = (sum_dependency + current_sum)/
                                     (n_dependency + current_n)) %>%
               filter(new_avg_dep > soft_dep_threshold)

            filtered_candidates <- candidates %>%
                           anti_join(to_exclude, by = c('column1'='column')) %>%
                           anti_join(to_exclude, by = c('column2'='column'))

         }

         # Initializes the view
         if (nrow(view) == 0){

            selection <- filtered_candidates %>%
                        mutate(zig = bi_zig + zig1 + zig2 ) %>%
                        filter(zig == max(zig, na.rm = T)) %>%
                        mutate(zig_min = min(zig1, zig2), zig_max = max(zig1, zig2),
                               delta_zig = zig_min + bi_zig) %>%
                        select(column1, column2, zig_max, delta_zig)

            view_cols <- as.character(selection[1, c('column1', 'column2')])
            view <- data.frame('column'  = view_cols, stringsAsFactors = F)
            col_zigs  <- c(as.numeric(selection[1,'zig_max']),
                           as.numeric(selection[1,'delta_zig']))
            next
         }


         # Finds the strongest candidate
         left_edges <- filtered_candidates %>%
                           semi_join(view, by = c('column1' = 'column')) %>%
                           anti_join(view, by = c('column2' = 'column')) %>%
                           group_by(column = column2) %>%
                           summarize(delta_bi_zig  = sum(bi_zig),
                                     delta_uni_zig = first(zig2))

         right_edges <- filtered_candidates %>%
                           semi_join(view, by = c('column2' = 'column')) %>%
                           anti_join(view, by = c('column1' = 'column')) %>%
                           group_by(column = column1) %>%
                           summarize(delta_bi_zig  = sum(bi_zig),
                                     delta_uni_zig = first(zig1))

         if (nrow(left_edges) < 1 & nrow(right_edges) < 1){
            break
         }


         best_col <- rbind_list(left_edges, right_edges) %>%
                           group_by(column) %>%
                           summarize(delta_zig =
                                  sum(delta_bi_zig) + first(delta_uni_zig)) %>%
                           filter(delta_zig == max(delta_zig, na.rm = T)) %>%
                           filter(row_number(delta_zig) == 1)

         if (nrow(best_col) < 1){
            break
         }


         # And appends it!
         view <- rbind_list(view, select(best_col, column))
         col_zigs <- c(col_zigs, best_col$delta_zig[[1]])

      }

   if (auto_stop){
      cp <- change_point(col_zigs)
      col_zigs <- col_zigs[1:cp]
      view <- view %>% slice(1:cp)
    }


   zigs[[k]]  <- sum(col_zigs)
   views[[k]] <- view

   }

   return(list(
      views  = views,
      scores = zigs
   ))


}

#################
# FULL FUNCTION #
#################
ziggy_comment <- function(data, selection, K, D,
                          soft_dep_threshold=NULL, hard_dep_threshold=NULL,
                          logfun = NULL, outfun = NULL){


   cat("Running the ZIGGY!\n")
   TIME0 <- proc.time()["elapsed"]


   # Prepares "offline" statistics
   offline_uni_stats <- compute_uni_stats(data)
   offline_bi_stats <- compute_bi_stats(data, offline_uni_stats)
   TIME1 <- proc.time()["elapsed"]


   # Computes Zig-dissimilarities
   zig_components <- zig_score(selection, data, offline_uni_stats,
                               offline_bi_stats)
   zig_scores <- zig_aggregate(zig_components, zig_coefficients)
   TIME2 <- proc.time()["elapsed"]

   # Detects views
   viewset <- search_views(K, D, zig_scores,
                           offline_bi_stats,
                           soft_dep_threshold=soft_dep_threshold,
                           hard_dep_threshold=hard_dep_threshold)
   TIME3 <- proc.time()["elapsed"]

   # Wrapping up
   views <- lapply(1:length(viewset$views), function(i){
      list(
         columns  = viewset$views[[i]]$column,
         strength = viewset$scores[[i]]
      )
   })

   # Reporting
   if (!is.null(logfun)){
      logfun("Ziggy", "Time-Offline",    TIME1 - TIME0)
      logfun("Ziggy", "Time-Zigscores",  TIME2 - TIME1)
      logfun("Ziggy", "Time-ViewSearch", TIME3 - TIME2)
      logfun("Ziggy", "Time",            TIME3 - TIME0)
   }
   if (!is.null(outfun))
      write_results("Ziggy", views, outfun)

   cat("Done!\n")
   views


}
