#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

FOLDER <- "03-11"

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
           summarize(F1 = mean(F1, na.rm = T)) %>%
           ungroup

all_combis <-  unique(merge(to_plot$n_noise, to_plot$algo))
colnames(all_combis) <- c("n_noise", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_noise", "algo"))

p1 <- ggplot(to_plot, aes(x = n_noise, y = F1, color = algo, fill= algo)) +
        geom_line() +
         geom_point() +
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
   summarize(F1 = mean(F1, na.rm = T)) %>%
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


##########################
# Plots Vary Size target #
##########################
to_plot <- out_file %>%
   filter(experiment == "vary_size_target") %>%
   filter(grepl("- F1", key)) %>%
   mutate(F1 = as.numeric(value)) %>%
   select(n_tuples_sel, algo, F1)

to_plot <- to_plot %>%
   group_by(algo, n_tuples_sel) %>%
   summarize(F1 = mean(F1, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_tuples_sel, to_plot$algo))
colnames(all_combis) <- c("n_tuples_sel", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_tuples_sel", "algo"))

p3 <- ggplot(to_plot, aes(x = n_tuples_sel, y = F1, color = algo, fill= algo)) +
   geom_line() +
   scale_x_continuous(name = "N tuples in target") +
   scale_y_continuous(name = "Accuracy - F1", limits=c(0,1))


p3 <- prettify(p3) +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p3)



##############################
# Plots Runtime vary ntuples #
##############################
to_plot <- log_file %>%
   filter(experiment == "VaryNumberTuples") %>%
   filter("Time"== key) %>%
   mutate(Time = as.numeric(value), n_tuples = n_tuples_sel + n_tuples_exc) %>%
   select(n_tuples, algo, Time)

to_plot <- to_plot %>%
   group_by(algo, n_tuples) %>%
   summarize(Time = mean(Time, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_tuples, to_plot$algo))
colnames(all_combis) <- c("n_tuples", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_tuples", "algo"))

p4 <- ggplot(to_plot, aes(x = n_tuples, y = Time, color = algo, fill= algo)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(name = "N tuples") +
   scale_y_continuous(name = "Time (s)", limits=c(0,2))


p4 <- prettify(p4) +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p4)

###############################
# Plots Runtime vary ncolumns #
###############################
to_plot <- log_file %>%
   filter(experiment == "VaryNumberSubspaces") %>%
   filter("Time"== key) %>%
   mutate(Time = as.numeric(value), n_columns = n_subspaces*w_subspaces + n_noise) %>%
   select(n_columns, algo, Time)

to_plot <- to_plot %>%
   group_by(algo, n_columns) %>%
   summarize(Time = mean(Time, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_columns, to_plot$algo))
colnames(all_combis) <- c("n_columns", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_columns", "algo"))

p4 <- ggplot(to_plot, aes(x = n_columns, y = Time, color = algo, fill= algo)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(name = "N columns") +
   scale_y_continuous(name = "Time (s)", limits=c(0,20))


p4 <- prettify(p4) +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p4)

#ggsave("../documents/plots/tmp_view-scores.pdf", p2,
#       width = 16, height = 4, units = "cm")


#######################
# Plots deduplication #
#######################
to_plot1 <- out_file %>%
   filter(experiment == "VaryDeduplication") %>%
   filter(grepl("- F1", key)) %>%
   mutate(F1 = as.numeric(value)) %>%
   select(algo, soft_thres, hard_thres, F1)

to_plot1 <- to_plot1 %>%
   group_by(algo, soft_thres, hard_thres) %>%
   summarize(F1 = mean(F1, na.rm = T)) %>%
   ungroup

to_plot2 <- out_file %>%
   filter(experiment == "VaryDeduplication") %>%
   filter(grepl("Diversity", key)) %>%
   mutate(Diversity = as.numeric(value)) %>%
   select(algo, soft_thres, hard_thres, Diversity)

to_plot <- to_plot1 %>%
   inner_join(to_plot2, by=c('algo', 'soft_thres', 'hard_thres')) %>%
   mutate(experiments = ifelse(soft_thres > 0, 'Soft thresold', 'Hard threshold'))


p5 <- ggplot(to_plot, aes(x = Diversity, y = F1, color = experiments, fill= experiments)) +
   geom_point() +
   geom_line() +
   scale_x_continuous(name = "Diversity") +
   scale_y_continuous(name = "Accuracy - F1", limits=c(0,1))


p5 <- prettify(p5) +
   theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p5)

