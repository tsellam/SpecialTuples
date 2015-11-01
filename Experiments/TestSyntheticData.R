#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0 || exists("R_TEST")) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")

library(R.utils)
library(clusterGeneration)
select <- dplyr::select

source("Baselines/Claude.R", chdir = TRUE)
source("Baselines/Baselines.R", chdir = TRUE)
jar_loc <- paste0(getwd(), "/Baselines/4S/4S.jar")
source("../Code/Ziggy.R", chdir = TRUE)




# --------------------------#
# Function to generate data #
# --------------------------#
generateData <- function(n_subspaces, w_subspaces, n_noise,
                         n_tuples_sel, n_tuples_exc){

   cat("Generating random dataset with", n_subspaces * w_subspaces + n_noise,
       "columns, and",  n_tuples_sel + n_tuples_exc, "rows\n")

   subspaces <- lapply(1:n_subspaces, function(s){


      N <- ceiling(n_tuples_exc / 5)

      mean1 <- runif(n = w_subspaces, min = 40, max = 60)
      cov1  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data1 <- mvrnorm(n_tuples_sel, mu = mean1, Sigma = cov1$Sigma)

      mean2 <- runif(n = w_subspaces, min = 40, max = 60)
      cov2  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data2 <- mvrnorm(N, mu = mean2, Sigma = cov2$Sigma)

      mean3 <- runif(n = w_subspaces, min = 40, max = 60)
      cov3  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data3 <- mvrnorm(N, mu = mean3, Sigma = cov3$Sigma)

      mean4 <- runif(n = w_subspaces, min = 40, max = 60)
      cov4  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data4 <- mvrnorm(N, mu = mean4, Sigma = cov4$Sigma)

      mean5 <- runif(n = w_subspaces, min = 40, max = 60)
      cov5  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data5 <- mvrnorm(N, mu = mean5, Sigma = cov5$Sigma)

      mean6 <- runif(n = w_subspaces, min = 40, max = 60)
      cov6  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data6 <- mvrnorm(N, mu = mean6, Sigma = cov6$Sigma)

      rbind(data1, data2, data3, data4, data5, data6)


   })

   noise_cols <- lapply(1:n_noise, function(j){
      noise_mean <- runif(1, min = 10, max = 90)
      noise_vari <- runif(1, min = 0,  max = 30)
      rnorm(nrow(subspaces[[1]]), noise_mean, noise_vari)
   })


   data <- do.call(cbind,  subspaces)
   noise <- do.call(cbind, noise_cols)

   data <- as.data.frame(cbind(data, noise))
   names(data) <- paste0('col', 1:ncol(data))
   target <- c(rep(TRUE, n_tuples_sel), rep(FALSE, n_tuples_exc))
   data <- cbind(data, target)

   data  <- data[sample(1:nrow(data), nrow(data), replace=FALSE),]

   data
}


# -------------#
# Functionals  #
# -------------#
file_log     <- "synth_results.log"
log_headers <- c("experiment", "n_subspaces", "w_subspaces",
                  "n_noise", "n_tuples_sel" , "n_tuples_exc",
                  "soft_thres", "hard_thres",
                 "algo", "key", "value\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)

file_out <- "synth_results.out"
out_headers <- c("experiment", "n_subspaces", "w_subspaces",
                 "n_noise", "n_tuples_sel" , "n_tuples_exc",
                 "soft_thres", "hard_thres",
                  "algo", "view", "key", "value\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)

wrapper <- function(..., score_function, algo){
   tryCatch({
      out <- evalWithTimeout(... , timeout=600)
      score_function(out, algo)
   },
   error = function(e){
      cat("Error, or TIMEOUT!\n")
      print(e)
   }
   )
}

# Creates alternative score function
score_function <- function(res, algo){
   #get_NB_score(res, clean_data_NB, target, writeout, algo)
   get_kNN_score(res, clean_data_kNN, target, writeout, algo)
}




if (test_mode){
   REF_n_subspaces   <- 5
   REF_w_subspaces   <- 5
   REF_n_noise       <- 5
   REF_n_tuples_sel  <- 50
   REF_n_tuples_exc  <- 300
} else {
   REF_n_subspaces   <- 5
   REF_w_subspaces   <- 5
   REF_n_noise       <- 25
   REF_n_tuples_sel  <- 5000
   REF_n_tuples_exc  <- 30000
}



############################################
# First batch : Test accuracy varying data #
############################################

#-------------------------------------#
# First experiment - vary subspaces W #
#-------------------------------------#
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Vary Subspace Size\n')
grid <- if (test_mode){
   c(2, 4)
} else {
   c(2,4,8,12,16)
}
for (w_subspaces in grid){

   n_noise <- n_subspaces * w_subspaces


   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("VarySubspaceWidth", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VarySubspaceWidth", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                                n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- n_subspaces
      D <- w_subspaces

      soft <- 0.3
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- 0.99
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL


      # Runs the other guys
      #  Baselines

      if (!test_mode){

         fourS_views <-  wrapper(
         FourS(clean_data_kNN, target, jar_loc = jar_loc,
               logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "4S"
      )

      beamed_kNN <-  wrapper(
         search_exact_kNN(clean_data_kNN, target, q = K,
                          size_view = D, size_beam = D*2,
                          logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Wrap_kNN"
      )

      }

      clique_approx <-  wrapper(
         search_cliques(clean_data_claude, target, q = K,
                        size_view = D, size_beam = D*2,
                        logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Clique"
      )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = D*2, dup_factor = D*4,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}

#--------------------------------#
# Second experiment - vary noise #
#--------------------------------#
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Vary Noise\n')
grid <- if (test_mode){
   c(100, 50)
} else {
   c(25, 50, 75, 100)
}
for (n_noise in grid){

   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("vary_noise", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("vary_noise", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                           n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- n_subspaces
      D <- w_subspaces

      soft <- 0.3
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- 0.99
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL


      # Runs the other guys
      #  Baselines

      if (!test_mode){

         fourS_views <-  wrapper(
            FourS(clean_data_kNN, target, jar_loc = jar_loc,
                  logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "4S"
         )

         beamed_kNN <-  wrapper(
            search_exact_kNN(clean_data_kNN, target, q = K,
                             size_view = D, size_beam = D*2,
                             logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_kNN"
         )

      }

      clique_approx <-  wrapper(
         search_cliques(clean_data_claude, target, q = K,
                        size_view = D, size_beam = D*2,
                        logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Clique"
      )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = D*2, dup_factor = D*4,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}

#-------------------------------------#
# Third experiment - vary  size target #
#--------------------------------------#
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Vary Size target\n')
grid <- if (test_mode){
   c(10, 250)
} else {
   c(1000, 5000, 15000, 25000)
}
for (n_tuples_sel in grid){

   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("vary_size_target", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("vary_size_target", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                           n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- n_subspaces
      D <- w_subspaces

      soft <- 0.3
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- 0.99
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL


      # Runs the other guys
      #  Baselines

      if (!test_mode){

         fourS_views <-  wrapper(
            FourS(clean_data_kNN, target, jar_loc = jar_loc,
                  logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "4S"
         )

         beamed_kNN <-  wrapper(
            search_exact_kNN(clean_data_kNN, target, q = K,
                             size_view = D, size_beam = D*2,
                             logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_kNN"
         )

      }

      clique_approx <-  wrapper(
         search_cliques(clean_data_claude, target, q = K,
                        size_view = D, size_beam = D*2,
                        logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Clique"
      )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = D*2, dup_factor = D*4,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}


##########################
# Second batch : Runtime #
##########################

#-------------------------------------#
# First experiment - vary ntuples     #
#-------------------------------------#
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Runtime - nrows \n')
grid <- if (test_mode){
   c(300, 1000)
} else {
   c(25000, 75000, 100000, 125000, 150000)
}
for (n_tuples_exc in grid){

   n_tuples_sel <- ceiling(1/6 * n_tuples_exc)

   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("VaryNumberTuples", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VaryNumberTuples", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                           n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- n_subspaces
      D <- w_subspaces

      soft <- 0.3
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- 0.99
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL


      # Runs the other guys
      #  Baselines

      if (!test_mode){

         fourS_views <-  wrapper(
            FourS(clean_data_kNN, target, jar_loc = jar_loc,
                  logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "4S"
         )

         beamed_kNN <-  wrapper(
            search_exact_kNN(clean_data_kNN, target, q = K,
                             size_view = D, size_beam = D*2,
                             logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_kNN"
         )

      }

      clique_approx <-  wrapper(
         search_cliques(clean_data_claude, target, q = K,
                        size_view = D, size_beam = D*2,
                        logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Clique"
      )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = D*2, dup_factor = D*4,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}


#---------------------------------------#
# Second experiment - vary ncolumns     #
#---------------------------------------#
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Runtime - Number of Subspaces \n')
grid <- if (test_mode){
   c(1, 3)
} else {
   c(5, 10, 15, 20, 25)
}
for (n_subspaces in grid){


   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("VaryNumberSubspaces", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VaryNumberSubspaces", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                           n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- REF_n_subspaces
      D <- REF_w_subspaces

      soft <- 0.3
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- 0.99
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL


      # Runs the other guys
      #  Baselines

      if (!test_mode){

         fourS_views <-  wrapper(
            FourS(clean_data_kNN, target, jar_loc = jar_loc,
                  logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "4S"
         )

         beamed_kNN <-  wrapper(
            search_exact_kNN(clean_data_kNN, target, q = K,
                             size_view = D, size_beam = D*2,
                             logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_kNN"
         )

      }

      clique_approx <-  wrapper(
         search_cliques(clean_data_claude, target, q = K,
                        size_view = D, size_beam = D*2,
                        logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Clique"
      )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = D*2, dup_factor = D*4,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}


###############################
# Third batch : Deduplication #
###############################
n_subspaces   <- REF_n_subspaces
w_subspaces   <- REF_w_subspaces
n_noise       <- REF_n_noise
n_tuples_sel  <- REF_n_tuples_sel
n_tuples_exc  <- REF_n_tuples_exc


cat('\n\n\n\n**** Experiment: Deduplication \n')
grid <- if (test_mode){
   c(0.2, 0.5)
} else {
   1:5*0.2
}
for (dedup in grid){


   tryCatch({

      # Prepares functions to output stuff
      writelog <- function(...){
         line <- paste0(c("VaryDeduplication", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VaryDeduplication", n_subspaces, w_subspaces,
                          n_noise, n_tuples_sel, n_tuples_exc,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...), collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }


      # Prepares data
      file <- generateData(n_subspaces, w_subspaces,
                           n_noise, n_tuples_sel, n_tuples_exc)
      target <- names(file)[[ncol(file)]]

      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Runs Ziggy
      K <- n_subspaces
      D <- w_subspaces

      soft <- dedup
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- dedup
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )

      soft <- NULL
      hard <- NULL

   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })

}