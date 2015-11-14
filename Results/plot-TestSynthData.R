#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

FOLDER <- "06-11"

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
                                algo)) %>%
   filter(!algo %in% c('4S_official', 'Ziggy_soft0.1_hard0', 'Ziggy_soft0.05_hard0')) %>%
            mutate(algo = factor(algo, levels = c("Ziggy_soft0.15_hard0",
                                                  "Ziggy_soft0_hard0.99",
                                                  "Approximative",
                                                  "4S",
                                                  "Clique",
                                                  "Wrap_kNN"),
                                        labels = c("Ziggy-Soft",
                                                  "Ziggy-Hard",
                                                  "Claude",
                                                  "4S",
                                                  "Clique",
                                                  "Wrap 5-NN")))


log_file <- log_file %>%
   mutate(algo = ifelse(algo == "Ziggy",
                        paste0(algo, "_soft", soft_thres,
                               "_hard", hard_thres),
                        algo)) %>%
   filter(!algo %in% c('4S', 'Ziggy_soft0.1_hard0', 'Ziggy_soft0.05_hard0'))%>%
   mutate(algo = factor(algo, levels = c("Ziggy_soft0.15_hard0",
                                         "Ziggy_soft0_hard0.99",
                                         "Approximative",
                                         "Clique",
                                         "4S_official",
                                         "Wrap_kNN"),
                        labels = c("Ziggy-Soft",
                                   "Ziggy-Hard",
                                   "Claude",
                                   "Clique",
                                   "4S",
                                   "Wrap 5-NN")))


###################
# Diversity Plots #
###################
# Subspace Width
to_plot <- out_file %>%
   filter(experiment == "VarySubspaceWidth") %>%
   filter(grepl("Diversity", key)) %>%
   mutate(Diversity = as.numeric(value)) %>%
   filter(algo != 'Clique') %>%
   select(w_subspaces, algo, Diversity)

to_plot <- to_plot %>%
   group_by(algo, w_subspaces) %>%
   summarize(Diversity = mean(Diversity, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$w_subspaces, to_plot$algo))
colnames(all_combis) <- c("w_subspaces", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("w_subspaces", "algo"))

to_plot <- to_plot %>%
   mutate(Diversity = ifelse(algo=='4S', Diversity + .2, Diversity))

p1bis <- ggplot(to_plot, aes(x = w_subspaces, y = Diversity,
                             color = algo, fill= algo, shape=algo)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(name = "#Dimensions per Subspace") +
   scale_y_continuous(name = "#Distinct Columns", limits=c(0, 100))

p1bis <- prettify(p1bis) +
   theme(legend.position=c(0, 1),
         legend.justification=c(0.1,0.75),
         legend.background = element_rect(fill="transparent"),
         axis.title = element_text(size = 8))
print(p1bis)


# Plots deduplication
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
   select(algo, soft_thres, hard_thres, Diversity) %>%
   group_by(algo, soft_thres, hard_thres) %>%
   summarise(Diversity = mean(Diversity, na.rm = T))

to_plot <- to_plot1 %>%
   inner_join(to_plot2, by=c('algo', 'soft_thres', 'hard_thres')) %>%
   mutate(experiments = ifelse(soft_thres > 0, 'Soft thresold', 'Hard threshold'))

p5ter <-  ggplot(to_plot, aes(x = soft_thres + hard_thres, y = Diversity, color = experiments, fill= experiments)) +
   geom_point() +
   geom_line() +
   scale_x_continuous(name = "Dependency Threshold", limits=c(0.2,0.5)) +
   scale_y_continuous(name = "#Distinct Columns", limits=c(0,20))

p5ter <- prettify(p5ter) +
   theme(legend.position=c(0, 1),
         legend.justification=c(0.1,0.7),
         legend.background = element_rect(fill="transparent"),
         axis.title = element_text(size = 8))
plot(p5ter)

save_plots("Synth-Dedup.pdf", p1bis, p5ter, pdfwidth = 10, pdfheight = 5,
           keep_legend = T)
if (exists("DO_IT")){
   system('source ~/.bash_profile ; pdfcrop Synth-Dedup.pdf ../Paper/Plots/Synth-Dedup.pdf ;
          cd ../Paper ; ./renderPDF.sh')
}




#############################
# Plots Vary Subspace Width #
#############################
to_plot <- out_file %>%
            filter(experiment == "VarySubspaceWidth") %>%
            filter(grepl("- F1", key)) %>%
            mutate(F1 = as.numeric(value)) %>%
            select(w_subspaces, algo, F1)

to_plot <- to_plot %>%
           group_by(algo, w_subspaces) %>%
           summarize(F1 = mean(F1, na.rm = T)) %>%
           ungroup

all_combis <-  unique(merge(to_plot$w_subspaces, to_plot$algo))
colnames(all_combis) <- c("w_subspaces", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("w_subspaces", "algo"))

p1 <- ggplot(to_plot, aes(x = w_subspaces, y = F1, color = algo, fill= algo,
                          shape=algo)) +
        geom_line() +
         geom_point() +
        scale_x_continuous(name = "#Col. per Subspace") +
        scale_y_continuous(name = "Accuracy - F1", limits=c(0,1), breaks=c(0,0.5,1))


p1 <- prettify(p1)

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

p2 <- ggplot(to_plot, aes(x = n_noise, y = F1, color = algo, fill= algo,
                          shape=algo)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(name = "#Non-Separated Col.") +
   scale_y_continuous(name = "Accuracy - F1", limits=c(0,1), breaks=c(0,0.5,1))


p2 <- prettify(p2)

print(p2)

#ggsave("../documents/plots/tmp_view-scores.pdf", p2,
#       width = 16, height = 4, units = "cm")


##########################
# Plots Vary Size target #
##########################
to_plot <- out_file %>%
   filter(experiment == "vary_size_target") %>%
   filter(grepl("- F1", key)) %>%
   mutate(F1 = as.numeric(value),
          n_select=n_tuples_sel*100/(n_tuples_exc+n_tuples_sel)) %>%
   select(n_select, algo, F1)

to_plot <- to_plot %>%
   group_by(algo, n_select) %>%
   summarize(F1 = mean(F1, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_select, to_plot$algo))
colnames(all_combis) <- c("n_select", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_select", "algo"))

p3 <- ggplot(to_plot, aes(x = n_select, y = F1, color = algo, fill= algo,
                          shape=algo)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(name = "#Tuples in selection (% total)", limits=c(0,50)) +
   scale_y_continuous(name = "Accuracy - F1", limits=c(0,1), breaks=c(0,0.5,1))


p3 <- prettify(p3)
print(p3)


save_plots("Synth-Accuracy.pdf", p3, p1, p2)
if (exists("DO_IT")){
   system('source ~/.bash_profile ; pdfcrop Synth-Accuracy.pdf ../Paper/Plots/Synth-Accuracy.pdf')
}




##############################
# Plots Runtime vary ntuples #
##############################
to_plot <- log_file %>%
   filter(experiment == "VaryNumberTuples") %>%
   filter("Time"== key) %>%
   mutate(Time = as.numeric(value), n_tuples = n_tuples_sel + n_tuples_exc) %>%
   filter(!algo %in% c('Clique', '4S')) %>%
   select(n_tuples, algo, Time)

to_plot <- to_plot %>%
   group_by(algo, n_tuples) %>%
   summarize(Time = mean(Time, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_tuples, to_plot$algo))
colnames(all_combis) <- c("n_tuples", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_tuples", "algo")) %>%
            mutate(Time = ifelse(algo == 'Ziggy-Soft', Time+0.25, Time))


p4 <- ggplot(to_plot, aes(x = n_tuples/1000, y = Time, color = algo, fill= algo,
                          shape = algo)) +
   geom_line() +
   geom_point(position='dodge') +
   scale_x_continuous(name = "#Tuples (x1,000)") +
   scale_y_continuous(name = "Time (s)", limits=c(0, 22), breaks = c(0,5,10,15,20))

p4 <- prettify(p4) + theme(legend.position = c(0,1),
                           legend.justification=c(0.15,0.7),
                           legend.background = element_rect(fill="transparent"))

print(p4)

###############################
# Plots Runtime vary ncolumns #
###############################
to_plot <- log_file %>%
   filter(experiment == "VaryNumberSubspaces") %>%
   filter("Time"== key) %>%
   filter(!algo %in% c('Clique', '4S')) %>%
   mutate(Time = as.numeric(value), n_columns = n_subspaces*w_subspaces + n_noise,
          shape = algo) %>%
   select(n_columns, algo, Time)

to_plot <- to_plot %>%
   group_by(algo, n_columns) %>%
   summarize(Time = mean(Time, na.rm = T)) %>%
   ungroup

all_combis <-  unique(merge(to_plot$n_columns, to_plot$algo))
colnames(all_combis) <- c("n_columns", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("n_columns", "algo"))

p4b <- ggplot(to_plot, aes(x = n_columns, y = Time, color = algo, fill= algo,
                           shape = algo)) +
   geom_line() +
   geom_point(position='dodge') +
   scale_x_continuous(name = "#Columns") +
   scale_y_continuous(name = "Time (s)", limits = c(0, 7.5),  breaks = c(0,2.5,5,7.5))

p4b <- prettify(p4b) + theme(legend.position="none")

print(p4b)


save_plots("Synth-Runtime.pdf", p4, p4b, pdfwidth=8, pdfheight = 4, keep_legend = T)
if (exists("DO_IT")){
   system('source ~/.bash_profile ; pdfcrop Synth-Runtime.pdf ../Paper/Plots/Synth-Runtime.pdf')
}

#######################
# Plots Detail Rutime #
#######################

to_plot <- log_file %>%
   filter(experiment == "VaryNumberSubspaces",
          algo == 'Ziggy-Soft',
          key %in% c('Time-Offline', 'Time-Zigscores', 'Time-ViewSearch')) %>%
   mutate(Stage = factor(key,
                         levels = c('Time-Offline', 'Time-Zigscores', 'Time-ViewSearch'),
                         labels = c('Offline Zig-Comp.', 'Online Zig-Comp.', 'View Seach')),
          n_col = n_subspaces*w_subspaces
   )%>%
   group_by(n_col, Stage) %>%
   summarise(Runtime = mean(value, na.rm=T)) %>%
   print

pD <- ggplot(to_plot, aes(x=factor(n_col), y = Runtime,
                          color = Stage, fill = Stage)) +
   geom_histogram(stat = 'identity', position = 'stack') +
   scale_x_discrete('#Columns') +
   scale_y_continuous('Time (s)')

pD <- prettify(pD) + theme(legend.key.width = unit(0.5,'cm'))

plot(pD)

save_plots("Synth-TimeDetail.pdf", pD, pdfwidth=8, pdfheight = 4)
if (exists("DO_IT")){
   system('source ~/.bash_profile ; \
          pdfcrop Synth-TimeDetail.pdf ../Paper/Plots/Synth-TimeDetail.pdf ; \
          cd ../Paper ; ./renderPDF.sh')
}


