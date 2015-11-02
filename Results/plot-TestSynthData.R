#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

FOLDER <- "02-11"

###############
# PREPARATION #
###############
# Out files
files <- list.files(FOLDER, pattern = "synth_results.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), " result files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
out_file <- rbind_all(file_contents)

# Timing files
files <- list.files(FOLDER, pattern = "synth_results.log", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), " log files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
log_file <- rbind_all(file_contents)


##########################
# Filters and Prettifies #
##########################

out_file <- out_file %>%
               mutate(algo = ifelse(algo == "Ziggy",
                                paste0(algo, "_soft", soft_thres,
                                             "_hard", hard_thres),
                                algo))


log_file <- log_file %>%
   mutate(algo = ifelse(algo == "Ziggy",
                        paste0(algo, "_soft", soft_thres,
                               "_hard", hard_thres),
                        algo)) %>%
   filter(algo != '4S_official')

#             filter(!file %in% black_list) %>%
#             mutate(file = sub(".arff", "", file)) %>%
#             mutate(algo = factor(algo, levels = c("ApproximativePrune",
#                                                   "Approximative",
#                                                   "4S",
#                                                   "4S_official",
#                                                   "Clique",
#                                                   "Exhaustive",
#                                                   "Wrap_kNN"),
#                                         labels = c("ApproximativePrune",
#                                                   "Claude",
#                                                   "4S",
#                                                   "4S_official",
#                                                   "Clique",
#                                                   "Exact",
#                                                   "Wrap 5-NN")))
#
# log_file <- log_file %>%
#     filter(!file %in% black_list) %>%
#     filter(!algo %in% c("Wrap_NaiveBayes", "4S_official")) %>%
#     mutate(file = sub(".arff", "", file)) %>%
#     mutate(algo = factor(algo, levels = c("ApproximativePrune",
#                                           "Approximative",
#                                           "4S",
#                                           "4S_official",
#                                           "Clique",
#                                           "Exhaustive",
#                                           "Wrap_kNN"),
#                          labels = c("ApproximativePrune",
#                                     "Claude",
#                                     "4S",
#                                     "4S_official",
#                                     "Clique",
#                                     "Exact",
#                                     "Wrap 5-NN")))

#############################
# Plots Vary Subspace Width #
#############################
to_plot <- out_file %>%
            filter(experiment == "VarySubspaceWidth") %>%
            filter(grepl("- F1", key)) %>%
            mutate(F1 = as.numeric(value)) %>%
            select(n_noise, algo, F1)

to_plot <- to_plot %>%
           group_by(algo, n_noise) %>%
           summarize(F1 = median(F1, na.rm = T)) %>%
           ungroup

all_combis <-  unique(merge(to_plot$n_noise, to_plot$algo))
colnames(all_combis) <- c("n_noise", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_noise", "algo"))

p1 <- ggplot(to_plot, aes(x = n_noise, y = F1, color = algo, fill= algo)) +
        geom_line() +
        scale_x_continuous(name = "Subspace Width") +
        scale_y_continuous(name = "Accuracy - F1", limits=c(0,1))


p1 <- prettify(p1) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p1)

#ggsave("../documents/plots/tmp_view-scores.pdf", p1,
#       width = 16, height = 4, units = "cm")



####################
# Plots Vary Noise #
####################
to_plot <- out_file %>%
   filter(experiment == "vary_noise") %>%
   filter(grepl("- F1", key)) %>%
   mutate(F1 = as.numeric(value)) %>%
   select(n_noise, algo, F1)

to_plot <- to_plot %>%
   group_by(algo, n_noise) %>%
   summarize(F1 = median(F1, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_noise, to_plot$algo))
colnames(all_combis) <- c("n_noise", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_noise", "algo"))

p2 <- ggplot(to_plot, aes(x = n_noise, y = F1, color = algo, fill= algo)) +
   geom_line() +
   scale_x_continuous(name = "Noise Columns") +
   scale_y_continuous(name = "Accuracy - F1", limits=c(0,1))


p2 <- prettify(p2) +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p2)

#ggsave("../documents/plots/tmp_view-scores.pdf", p2,
#       width = 16, height = 4, units = "cm")

