library(ggplot2)
library(dplyr)

# LOADING OUR DATASET
source('../Code/Ziggy.R')
source('../Code/ZiggyTalks.R')

data_file <- read.csv("~/Data/Files/oecd/PatentsAndRegions.csv",
                      row.names=1,
                      stringsAsFactors = F) %>%
             filter(!is.na(patent_applications_per_million))

target_col <- data_file[,'patent_applications_per_million']
selection <- target_col > quantile(target_col, 0.75, na.rm = T)

data_file$PCT_patent_applications <- cut(data_file$PCT_patent_applications,
        breaks = quantile(data_file$PCT_patent_applications, 0:10*0.1, na.rm = T))


# PREPROCESSING
data  <- preprocess(data_file)

# HARD WORK

zig_coefficients <- c(
   'm_mean_diff' = 1,
   'm_variance_ratio' = .5,
   'm_CramersV' = 5,
   'm_corr_diff' = 1,
   'm_CramersV_diff' = 1.5
)



CLOCK1 <- proc.time()['elapsed']

offline_uni_stats   <- compute_uni_stats(data)
offline_bi_stats <- compute_bi_stats(data, offline_uni_stats)

zig_components <- zig_score(selection, data, offline_uni_stats, offline_bi_stats)
all_zig_scores <- zig_aggregate(zig_components, zig_coefficients)

views <- search_views(K = 10, D = 6,
                      all_zig_scores, offline_bi_stats,
                      hard_dep_threshold = 0.75,
                      auto_stop = T)

CLOCK2 <- proc.time()['elapsed']
print(CLOCK2 - CLOCK1)



sapply(1:length(views$views), function(i){
   ziggy_talk_about(views$views[[i]], all_zig_scores)
})