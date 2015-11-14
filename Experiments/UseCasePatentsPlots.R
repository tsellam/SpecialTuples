source('UseCase/graph-utils.R')

plot_view <- function(col_set, dataset = data, in_selection = selection,
                      fig_label = NA){

   types <- sapply(col_set, function(col) class(dataset[,col] ))

   cat("Ploting for types:")
   print(types)


   dataset$in_selection <- ifelse(in_selection, 'Inside Selection', 'Outside Selection')


   if (length(col_set) == 1){

      if (types %in% c("numeric", "integer")){
         g <- ggplot(dataset, aes_string(x = col_set,
                                         fill='in_selection',
                                         color = 'in_selection')) +
            geom_density(alpha = 0.2, size = 1) +
            scale_y_continuous('Density Estimation') +
            annotate("text",  x=Inf, y = Inf,
                     label = paste0("Fig. ",fig_label),
                     vjust=1.5, hjust=1.5)

         g <- prettify(g)+ theme(legend.position="top")

         print(g)

         ggsave(paste0("UseCase/", col_set, ".pdf"), g,
               height = 4, width = 4)
      }


      if (types == "factor"){

         g <- ggplot(dataset, aes_string(x = col_set,
                                         fill='in_selection',
                                         color = 'in_selection')) +
            geom_histogram(position = "dodge",
                           aes(y = 100*..count../(sum(..count..))),
                           alpha = 0.2, size = 1) +
            scale_y_continuous('Proportion (%)') +
            annotate("text",  x=Inf, y = Inf,
                     label = paste0("Fig. ",fig_label),
                     vjust=1.5, hjust=1.5, size=5)

         g <- prettify(g) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(legend.position="top")

         print(g)

         ggsave(paste0("UseCase/", col_set, ".pdf"), g,
                height = 4, width = 4)

      }

   }

   if (length(col_set) == 2){

      if (all(types %in% c("numeric", "integer"))){

         g <- ggplot(dataset, aes_string(x = col_set[1],
                                         y = col_set[2],
                                         fill = 'in_selection',
                                         color = 'in_selection')) +
            geom_point(size=1, alpha=0.7) +
            geom_smooth(method = lm, size = 1, se = F) +
            annotate("text",  x=Inf, y = Inf,
                     label = paste0("Fig. ",fig_label),
                     vjust=1.5, hjust=1.5, size=5)


         g <- prettify(g) + theme(legend.position="top")

         print(g)

         ggsave(paste0("UseCase/", col_set[1], '_', col_set[2], ".pdf"), g,
                height = 4, width = 4)

      }


   }

}

names(data) <- gsub('.y$', '', names(data))

plot_view('patent_applications_per_million', fig_label='1')
plot_view('Personal_earnings',  fig_label=2)
plot_view('PCT_patent_applications', fig_label=3)
plot_view(c('patent_applications_per_million', 'Personal_earnings'), fig_label=4)


plot_view('Assault_rate', fig_label='1')
plot_view('Current_account_balance',  fig_label=2)
plot_view(c('Assault_rate', 'Homicide_rate'), fig_label=3)
plot_view(c('Assault_rate', 'Current_account_balance'), fig_label=4)


#plot_view('Dwellings_without_basic_facilities', fig_label='1')
#plot_view('Employees_working_very_long_hours',  fig_label=2)
#plot_view('Educational_attainment', fig_label=3)
#plot_view('Life_expectancy', fig_label=4)