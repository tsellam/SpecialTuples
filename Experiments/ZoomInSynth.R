#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
WORKGROUP <- 'group3'
cat("******* Working group:", WORKGROUP,"********\n")

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

   N_trick <- ceiling(n_tuples_exc / 10)
   N_noise <- N_trick * 5

   subspaces <- lapply(1:n_subspaces, function(s){

      mean1 <- runif(n = w_subspaces, min = 40, max = 60)
      cov1  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data1 <- mvrnorm(n_tuples_sel, mu = mean1, Sigma = cov1$Sigma)

      mean2 <- runif(n = w_subspaces, min = 40, max = 60)
      cov2  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data2 <- mvrnorm(N_trick, mu = mean2, Sigma = cov2$Sigma)

      mean3 <- runif(n = w_subspaces, min = 40, max = 60)
      cov3  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data3 <- mvrnorm(N_trick, mu = mean3, Sigma = cov3$Sigma)

      mean4 <- runif(n = w_subspaces, min = 40, max = 60)
      cov4  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data4 <- mvrnorm(N_trick, mu = mean4, Sigma = cov4$Sigma)

      mean5 <- runif(n = w_subspaces, min = 40, max = 60)
      cov5  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data5 <- mvrnorm(N_trick, mu = mean5, Sigma = cov5$Sigma)

      mean6 <- runif(n = w_subspaces, min = 40, max = 60)
      cov6  <- genPositiveDefMat(w_subspaces, rangeVar = c(1,20))
      data6 <- mvrnorm(N_trick, mu = mean6, Sigma = cov6$Sigma)

      noise <- runif(N_noise * w_subspaces, min = 10, max=90)
      noise <- matrix(noise, nrow = N_noise, ncol = w_subspaces)

      all_extras <- rbind(data2, data3, data4, data5, data6, noise)
      all_extras <- all_extras[sample(1:nrow(all_extras),
                                      nrow(all_extras),
                                      replace=FALSE),]

      rbind(data1, all_extras)

   })

   noise_cols <- lapply(1:n_noise, function(j){

      noise_mean <- runif(1, min = 10, max = 90)
      noise_vari <- runif(1, min = 0,  max = 30)

      gauss <- rnorm(n_tuples_sel + N_noise, noise_mean, noise_vari)
      noise <-  runif(N_noise, min = 10, max=90)

      all_col <- c(gauss, noise)
      all_col <- all_col[sample(1:length(all_col), length(all_col), replace=FALSE)]
      all_col <- matrix(all_col, ncol = 1)

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
      out <- evalWithTimeout(... , timeout=900)
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


   REF_n_subspaces   <- 5
   REF_w_subspaces   <- 4
   REF_n_noise       <- 4
   REF_n_tuples_sel  <- 3000
   REF_n_tuples_exc  <- 30000



if (WORKGROUP == "group1"){
   grid_w_subspaces  <- c(4)
   grid_n_noise      <- c(25)
   grid_n_tuples_sel <- c(25000)
   grid_n_tuples_exc <- c(125000)
   grid_n_subspaces  <- c(15)
   grid_dedup        <- 0.2
} else if (WORKGROUP == "group2"){
   grid_w_subspaces  <- c(8)
   grid_n_noise      <- c(50)
   grid_n_tuples_sel <- c(15000)
   grid_n_tuples_exc <- c(100000)
   grid_n_subspaces  <- c(5)
   grid_dedup        <- 0.4
} else if (WORKGROUP == "group3"){
   grid_w_subspaces  <- c(12)
   grid_n_noise      <- c(75)
   grid_n_tuples_sel <- c(5000)
   grid_n_tuples_exc <- c(75000)
   grid_n_subspaces  <- c(20)
   grid_dedup        <- 0.6
} else if (WORKGROUP == "group4"){
   grid_w_subspaces  <- c(16)
   grid_n_noise      <- c(100)
   grid_n_tuples_sel <- c(1000)
   grid_n_tuples_exc <- c(25000)
   grid_n_subspaces  <- c(10)
   grid_dedup        <-  0.8
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
for (w_subspaces in grid_w_subspaces){

   n_noise <- w_subspaces


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

      soft <- 0.1
      hard <- NULL
      ziggy_view <- wrapper(
         ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
                       soft_dep_threshold = soft,
                       hard_dep_threshold = hard,
                       logfun = writelog, outfun = writeout),
         score_function = score_function, algo = "Ziggy"
      )


}