#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0 || exists("R_TEST")) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")


library(foreign)
library(R.utils)


source("Baselines/Claude.R", chdir = TRUE)
source("Baselines/Baselines.R", chdir = TRUE)
jar_loc <- paste0(getwd(), "/Baselines/4S/4S.jar")
source("../Code/Ziggy.R", chdir = TRUE)


files_location <- "../Data"
file_list <- list.files(path = files_location, pattern = "*.arff$")
sizes <- file.info(paste0(files_location, "/", file_list))$size
file_list <- file_list[order(sizes)]

file_log     <- "results.log"
log_headers <- c("experiment", "file", "K", "size_view",
                 "soft_thres", "hard_thres", "algo", "key", "value\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)

file_out <- "results.out"
out_headers <- c("experiment", "file", "K", "size_view",
                 "soft_thres", "hard_thres", "algo", "view", "key", "value\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)

if (test_mode){
   #file_list <- "magic.arff"
   #file_list <- "adult.arff"
   file_list <- file_list[1:min(2, length(file_list))]
}

wrapper <- function(..., score_function, algo){
   tryCatch({
      out <- evalWithTimeout(... , timeout=3600)
      score_function(out, algo)
   },
   error = function(e){
      cat("Error, or TIMEOUT!\n")
      print(e)
   }
   )
}

###############
# EXPERIMENTS #
###############
for (arff_file in file_list){
   cat("\n**** Doing file", arff_file, "\n")

   tryCatch({

      # Loading and Preprocessing
      cat("Loading file...\n")
      file  <- read.arff(paste0(files_location, "/", arff_file))
      file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]

      target <- names(file)[[ncol(file)]]
      file[[target]] <- if(is.numeric(file[[target]])){
         cat("Converting numeric target...\n")
         cutted <- cut(file[[target]], breaks = 3)
         tab <- table(cutted)
         (cutted == names(which.min(tab)))

      } else if (is.factor(file[[target]])){
         cat("Converting factor target...\n")
         tab <- table(file[[target]])
         (file[[target]] == names(which.min(tab)))

      } else if (is.character(file[[target]])) {
         cat("Converting character target...\n")
         cutted <- factor(file[[target]])
         tab <- table(cutted)
         (file[[target]] == names(which.min(tab)))

      }
      cat(sum(file[[target]]), "rows to describe, out of", nrow(file), "\n")



      clean_data_NB <- preprocess_NB(file, target)
      clean_data_kNN <- preprocess_kNN(file, target)
      clean_data_claude <- claude_preprocess(file, target)
      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Prepares a function to output stuff
      writelog <- function(...){
         line <- paste0(c("VaryAlgos", arff_file, K, D,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ... ), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VaryAlgos", arff_file, K, D,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...) , collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }

      # Creates alternative score function
      score_function <- function(res, algo){
         #get_NB_score(res, clean_data_NB, target, writeout, algo)
         get_kNN_score(res, clean_data_kNN, target, writeout, algo)
      }

      cat("Running algos\n")


      # First run of 4S to get the parameters
      fourS_views <- FourS(clean_data_kNN, target, jar_loc = jar_loc)

      s_4s <- ceiling(mean(
         sapply(fourS_views, function(v) length(v$columns))
      ))
      D <- min(s_4s, ncol(clean_data_ziggy))
      K <- max(length(fourS_views), 2)
      b <- ceiling(max(2.0*K, 2.0*D))
      d_factor <- NULL

      cat("Parameters: K-", K, ", D-", D, "\n", sep = "")


      # My own boys
#       soft <- 0.2
#       hard <- NULL
#       ziggy_view <- wrapper(
#          ziggy_comment(clean_data_ziggy, target_ziggy, K=K, D=D,
#                     soft_dep_threshold = soft,
#                     hard_dep_threshold = hard,
#                     logfun = writelog, outfun = writeout),
#             score_function = score_function, algo = "Ziggy"
#           )

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

      if (!test_mode) {

           #  Baselines
           fourS_views <-  wrapper(
              FourS(clean_data_kNN, target, jar_loc = jar_loc,
                    logfun = writelog, outfun = writeout),
              score_function = score_function,
              algo = "4S"
          )


          beamed_kNN <-  wrapper(
             search_exact_kNN(clean_data_kNN, target, q = K,
                              size_view = D, size_beam = b,
                              logfun = writelog, outfun = writeout),
             score_function = score_function,
             algo = "Wrap_kNN"
          )
      }

       clique_approx <-  wrapper(
          search_cliques(clean_data_claude, target, q = K,
                         size_view = D, size_beam = b,
                         logfun = writelog, outfun = writeout),
          score_function = score_function,
          algo = "Clique"
       )

      approx <- wrapper(
         search_approx(clean_data_claude, target, q = K,
                       size_view = D, size_beam = b, dup_factor = 200,
                       logfun = writelog, outfun = writeout),
         score_function = score_function,
         algo = "Approximative"
      )

      cat("Done\n")
   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })
}


###############################
# SECOND BATCH: DEDUPLICATION #
###############################
cat("\n\n\n\n\n****** ROUND 2: DEDUPLICATION EXPERIMENTS")


for (arff_file in file_list){
   cat("\n**** Doing file", arff_file, "\n")

   tryCatch({

      # Loading and Preprocessing
      cat("Loading file...\n")
      file  <- read.arff(paste0(files_location, "/", arff_file))
      file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]

      target <- names(file)[[ncol(file)]]
      file[[target]] <- if(is.numeric(file[[target]])){
         cat("Converting numeric target...\n")
         cutted <- cut(file[[target]], breaks = 3)
         tab <- table(cutted)
         (cutted == names(which.min(tab)))

      } else if (is.factor(file[[target]])){
         cat("Converting factor target...\n")
         tab <- table(file[[target]])
         (file[[target]] == names(which.min(tab)))

      } else if (is.character(file[[target]])) {
         cat("Converting character target...\n")
         cutted <- factor(file[[target]])
         tab <- table(cutted)
         (file[[target]] == names(which.min(tab)))

      }
      cat(sum(file[[target]]), "rows to describe, out of", nrow(file), "\n")

      target_ziggy <- file[[target]]
      clean_data_ziggy <- preprocess(file[,!names(file)==target])


      # Prepares a function to output stuff
      writelog <- function(...){
         line <- paste0(c("VaryDeduplication", arff_file, K, D,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ... ), collapse="\t")
         cat(line, "\n", file = file_log, append = TRUE, sep = "")
      }
      writeout <- function(...){
         line <- paste0(c("VaryDeduplication", arff_file, K, D,
                          ifelse(is.null(soft), "0", soft),
                          ifelse(is.null(hard), "0", hard),
                          ...) , collapse="\t")
         cat(line, "\n", file = file_out, append = TRUE, sep = "")
      }

      # Creates alternative score function
      score_function <- function(res, algo){
         #get_NB_score(res, clean_data_NB, target, writeout, algo)
         get_kNN_score(res, clean_data_kNN, target, writeout, algo)
      }

      cat("Running algos\n")


      # First run of 4S to get the parameters
      D <- ncol(clean_data_ziggy) / 5
      K <- 5


      grid <- if (test_mode){
         c(0.2, 0.5)
      } else {
         1:10*0.1
      }
      for (dedup in grid){

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

      }

      cat("Done\n")
   },

   error = function(e){
      cat("ERROR!\n")
      print(e)
   })
}