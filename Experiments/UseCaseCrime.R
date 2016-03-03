library(ggplot2)
library(dplyr)
library(foreign)
library(ggthemes)

# LOADING OUR DATASET
source('../Code/Ziggy.R')
source('../Code/ZiggyTalks.R')

data_file <- read.arff('/Users/thib/Data/Files/crime/communities.arff')

target_col <- data_file[,'ViolentCrimesPerPop']
selection <- target_col > quantile(target_col, 0.75, na.rm = T)

names(data_file) <- gsub("\\.y$", '', names(data_file))



# PREPROCESSING
data  <- preprocess(data_file)

# HARD WORK

zig_coefficients <- c(
   'm_mean_diff' = 1,
   'm_variance_ratio' = .5,
   'm_CramersV' = 5,
   'm_corr_diff' = 1.5,
   'm_CramersV_diff' = 1.5
)



CLOCK1 <- proc.time()['elapsed']

offline_uni_stats   <- compute_uni_stats(data)
offline_bi_stats <- compute_bi_stats(data, offline_uni_stats)

zig_components <- zig_score(selection, data, offline_uni_stats, offline_bi_stats)
all_zig_scores <- zig_aggregate(zig_components, zig_coefficients)

views <- search_views(K = 50, D = 7,
                      all_zig_scores, offline_bi_stats,
                      hard_dep_threshold = .99,
                      auto_stop = T)

CLOCK2 <- proc.time()['elapsed']
print(CLOCK2 - CLOCK1)



sapply(1:length(views$views), function(i){
   #ziggy_talk_about(views$views[[i]], all_zig_scores)
   theme <- paste0( views$views[[i]][[1]], collapse = ', ')
   cat('<tr>\n     <td>' ,theme, '</td>\n</tr>\n')
})


library(GGally)
ggpairs(data = data_file,
        columns = c('NumIlleg','NumStreet',
                    'PctUsePubTrans','ViolentCrimesPerPop'),
        upper = list('continuous'='points'),
        mapping = ggplot2::aes(color = selection)
        ) +
theme_few()