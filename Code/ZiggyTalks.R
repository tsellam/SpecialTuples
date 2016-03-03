library(dplyr)
library(tidyr)


#########
# Utils #
#########
generate_keys <- function(column1, column2){
   sapply(1:length(column1), function(i){
      cols <- sort(c(column1[i], column2[i]))
      paste0(cols, collapse = '-')
   })
}

###########
# Filters #
###########
effect_thresholds <- data_frame(
   observation = c('Mean', 'Variance', 'Table', 'Correlations', 'CatCorrelations'),
   threshold = c(0.2, 0.2, 0.2, 0.4, 0.4)
)

get_exceptional_thresholds <- function(zig_scores, threshold = .90){

   th <- data_frame(
      observation = c(
         'Mean' ,
         'Variance',
         'Table',
         'Correlations',
         'CatCorrelation'
      ),
      threshold = c(
         quantile(zig_scores$zig_num_uni$m_mean_diff, probs=.9),
         quantile(zig_scores$zig_num_uni$m_variance_ratio, probs=.9),
         quantile(zig_scores$zig_cat_uni$m_CramersV, probs=.9),
         quantile(zig_scores$zig_num_bi$m_corr_diff, probs=.9),
         quantile(zig_scores$zig_cat_bi$m_CramersV_diff, probs=.9)
      )
   )

}

################
# Observations #
################
generate_table_diff <- function(tables_sel, tables_exc){
   lapply(1:length(tables_sel), function(i){
      tab_sel <- tables_sel[[i]]
      tab_exc <- tables_exc[[i]]

      diff <- tab_sel/sum(tab_sel) - tab_exc/sum(tab_exc)
      min_diff <- which.min(diff)
      max_diff <- which.max(diff)

      list(
         underrepresented = if (diff[min_diff]<0) names(diff)[min_diff] else NULL,
         overrepresented  = if (diff[max_diff]>0) names(diff)[max_diff] else NULL
      )

   })
}

describe_correlation_variation <- function(sel_corr, exc_corr, cor_diff){

   r <- runif(1) > .5

   lapply(1:length(sel_corr), function(i){



      if (exc_corr[i] >= 0){

         if (cor_diff[i] >= 0){
            list(
               'direction' = 'positive',
               'effect'    = ifelse(r, 'is strengthened',
                                    'appears as stronger'))

         } else if (sel_corr[i] > -0.1) {
            list(
               'direction' = 'positive',
               'effect'    = ifelse(r, 'is weakened or reversed',
                                    'is maybe weaker, maybe reversed'))

         } else if (sel_corr[i] < -0.1){
            list(
               'direction' = 'positive',
               'effect'    = ifelse(r, 'is inverted',
                                    'is turned over'))
         }

      } else if (exc_corr[i] < 0){

         if (cor_diff[i] <= 0){
            list(
               'direction' = 'negative',
               'effect'    = ifelse(r, 'is strengthened',
                                    'seems stronger'))

         } else if (sel_corr[i] < 0.1) {
            list(
               'direction' = 'negative',
               'effect'    = ifelse(r, 'may be weaker, or inverted',
                                    'is weakened or reversed'))

         } else if (sel_corr[i] > 0.1){
            list(
               'direction' = 'negative',
               'effect'    = ifelse(r, 'changes direction',
                                    'is reversed'))
         }
      }
   })
}

describe_cramersV_variation <- function(cor_diff){

   if (length(cor_diff) < 1) return(NULL)

   lapply(1:length(cor_diff), function(i){
      if (cor_diff > 0){
         list('effect' = 'strengthened')
      } else {
         list('effect' = 'weakened')
      }
   })
}

describe_mean_variation <- function(sel_mean, exc_mean){
   lapply(1:length(sel_mean), function(i){
      if (sel_mean[i] >= exc_mean[i]){
         list('effect' = 'increased')
      } else {
         list('effect' = 'decreased')
      }
   })

}

describe_variance_variation <- function(sel_variance, exc_variance){
   lapply(1:length(sel_variance), function(i){
      if (sel_variance[i] >= exc_variance[i]){
         list('effect' = 'increased')
      } else {
         list('effect' = 'decreased')
      }
   })

}



############
# Comments #
############
comment_1d_num <- function(means, variances){


   r <- runif(1) > .5

   sapply(1:length(means), function(i){

      if( means[[i]] == 'increased' & variances[[i]] == 'no_effect'){
         'the average is higher than in the rest of the data'
      } else if( means[[i]] == 'increased' & variances[[i]] == 'increased'){
         'the selection has a high average but also a high variance'
      } else if( means[[i]] == 'increased' & variances[[i]] == 'decreased'){
         ifelse(r,
               'your tuples are concentrated around a higher value',
               'the selection has a high average and a low variance'
         )

      } else if( means[[i]] == 'decreased' & variances[[i]] == 'no_effect'){
         'the average value is lower'
      } else if( means[[i]] == 'decreased' & variances[[i]] == 'increased'){
         'the tuples has a lower average but more variability'
      } else if( means[[i]] == 'decreased' & variances[[i]] == 'decreased'){
         ifelse(r,
               'the data is concentrated around a low value',
               'the data has a lower value and less variability'
         )

      } else if( means[[i]] == 'no_effect' & variances[[i]] == 'no_effect'){
         'something strange is going ong'
      } else if(  means[[i]] == 'no_effect' & variances[[i]] == 'increased'){
         'the tuples are unusually spreaded, but the average is the same'
      } else if( means[[i]] == 'no_effect' & variances[[i]] == 'decreased'){
         'the average is similar, but the tuples are particularly concentrated'
      } else {
         warning('Strange case detected in comment numeric 1d differences')
      }
   })
}

comment_1d_cat <- function(descriptions){
   if (length(descriptions) < 1) return(character(0))

   sapply(1:length(descriptions), function(i){

      paste0('the value ', descriptions[[i]][['underrepresented']],
             ' is underrepresented, while ', descriptions[[i]][['overrepresented']],
             ' is overrepresented')
   })
}

comment_2d_num <- function(descriptions){
   if (length(descriptions) < 1) return(character(0))

   sapply(1:length(descriptions), function(i){

      paste0("the ",
             descriptions[[i]]$direction,
             ' correlation ',
             descriptions[[i]]$effect
          )
   })
}

comment_2d_cat <- function(descriptions){
   if (length(descriptions) < 1) return(character(0))

   sapply(1:length(descriptions), function(i){

      paste0("the dependency is ", descriptions[[i]]$effect)
   })
}


# Gather, interpret, articulate
ziggy_talk_about <- function(view, zig_scores){

   cat("\n\n")

   if (nrow(view) < 1){
      cat('Nothing to say. sorry!\n')
      break
   }

   #############################################
   # For a start, collect things to talk about #
   #############################################
   talk_about <- data.frame(column1=character(0),
                            column2=character(0),
                            observation=character(0),
                            strength=numeric(0),
                            confidence=numeric(0),
                            description=list())

   # First, look at the one-dimension, numeric data
   about_num <- view %>%
                  inner_join(zig_scores$zig_num_uni, by = 'column')

   if(nrow(about_num) > 0){

      about_num1 <- about_num %>%
                     rename(column1 = column, value = m_mean_diff,
                            confidence = p_mean_diff) %>%
                     mutate(observation = 'Mean', column2 = NA) %>%
                     mutate(description = describe_mean_variation(sel_mean,
                                                                  exc_mean))%>%
                     select(column1, column2, observation, value, confidence, description)

      about_num2 <-  about_num %>%
                     rename(column1 = column, value = m_variance_ratio,
                            confidence = p_variance_ratio) %>%
         mutate(observation = 'Variance', column2 = NA) %>%
         mutate(description = describe_variance_variation(sel_variance,
                                                         exc_variance))%>%
         select(column1, column2, observation, value, confidence, description)

      talk_about <- talk_about %>%
                     rbind(about_num1) %>%
                     rbind(about_num2)
   }


   # Then, 1D categorical data
   about_cat <- view %>%
                  inner_join(zig_scores$zig_cat_uni, by = 'column')

   if (nrow(about_cat) > 0){
      table_diff <- data_frame(description =
                                  generate_table_diff(about_cat$sel_hist,
                                                      about_cat$exc_hist))
      about_cat <- cbind(about_cat, table_diff) %>%
                     mutate(observation = 'Table', column2 = NA) %>%
                     select(column1 = column, column2, observation, value = m_CramersV,
                            confidence = p_chi2_test, description)

      talk_about <- rbind(talk_about, about_cat)
   }

   # Then 2D numeric data
   about_2d_num_left <- view %>%
            inner_join(zig_scores$zig_num_bi, by = c('column' = 'column1')) %>%
            rename(inside = column, outside = column2)

   about_2d_num_right <- view %>%
            inner_join(zig_scores$zig_num_bi, by = c('column' = 'column2')) %>%
            rename(inside = column, outside = column1)

   about_2d_num <- rbind(about_2d_num_left, about_2d_num_right) %>%
                  inner_join(view, by = c('outside'= 'column') )


   if (nrow(about_2d_num) > 0) {
      about_2d_num <- about_2d_num %>%
                     rename(column1 = inside,
                            column2 = outside,
                            value = m_corr_diff,
                            confidence=t_corr_diff) %>%
                     mutate(description = describe_correlation_variation(
                                             sel_corr, exc_corr, value),
                            observation = 'Correlations') %>%
                     select(column1, column2, observation,
                            value, confidence, description )

      about_2d_num <- about_2d_num %>%
                  mutate(colkey = generate_keys(column1, column2)) %>%
                  group_by(colkey) %>%
                  summarise(column1 = first(column1),
                            column2 = first(column2),
                            observation = first(observation),
                            value = first(value),
                            confidence = first(confidence),
                            description = list(first(description))) %>%
                  select(-colkey)


      talk_about <- rbind(talk_about, about_2d_num)
   }


   # Then 2D categorical data
   about_2d_cat_left <- view %>%
      inner_join(zig_scores$zig_cat_bi, by = c('column' = 'column1')) %>%
      rename(inside = column, outside = column2)

   about_2d_cat_right <- view %>%
      inner_join(zig_scores$zig_cat_bi, by = c('column' = 'column2')) %>%
      rename(inside = column, outside = column1)

   about_2d_cat <- rbind(about_2d_cat_left, about_2d_cat_right) %>%
      mutate(inside  = gsub('!!NUM$', '', inside)) %>%
      mutate(outside = gsub('!!NUM$', '', outside)) %>%
      inner_join(view, by = c('outside' = 'column') )

   if (nrow(about_2d_cat) > 0) {

      about_2d_cat <- about_2d_cat %>%
               select(column1 = inside, column2 = outside,
                        value = m_CramersV_diff, confidence=t_CramersV_diff) %>%
               mutate(description = describe_cramersV_variation(value),
                      observation = 'CatCorrelations')

      if (nrow(about_2d_cat) > 0){
         about_2d_cat <- about_2d_cat %>%
            mutate(colkey = generate_keys(column1, column2)) %>%
            group_by(colkey) %>%
            summarise(column1 = first(column1),
                      column2 = first(column2),
                      observation = first(observation),
                      value = first(value),
                      confidence = first(confidence),
                      description = list(first(description))) %>%
            select(-colkey)

         talk_about <- rbind(talk_about, about_2d_cat)
      }
   }


   ##########
   # Labels #
   ##########
   # First, based on confidence
   talk_about <- talk_about %>%
                  mutate(weak_confidence = confidence > 0.01)

   # Then, based on effect size
   talk_about <- talk_about %>%
                  inner_join(effect_thresholds, by = 'observation') %>%
                  mutate(weak_effect = abs(value) < threshold) %>%
                  select(-threshold)

   talk_about <- talk_about %>%
                  mutate(exclude = weak_confidence | weak_effect)

   # Finally, gets exceptional sizes
   exceptional_thresholds <- get_exceptional_thresholds(zig_scores)
   talk_about <- talk_about %>%
      inner_join(exceptional_thresholds, by = 'observation') %>%
      mutate(exceptional = abs(value) > threshold) %>%
   select(-threshold)



   ############
   # Comments #
   ############
   # Numeric 1D data
   talk_sections_num_1D <- talk_about %>%
               filter(!exclude) %>%
               filter(observation %in% c('Mean', 'Variance')) %>%
               select(column1, observation, description) %>%
               spread(observation, description, fill = list('effect'='no_effect'))

   if (nrow(talk_sections_num_1D) >= 1){

      nulls <- sapply(talk_sections_num_1D$Mean, is.null)
      talk_sections_num_1D$Mean[nulls] <- list('effect'='no_effect')

      nulls <- sapply(talk_sections_num_1D$Variance, is.null)
      talk_sections_num_1D$Variance[nulls] <- list('effect'='no_effect')


      talk_sections_num_1D <- talk_sections_num_1D %>%
                  mutate(comment = comment_1d_num(Mean, Variance),
                         column2 = NA) %>%
                  select(column1, column2, comment)

   }

   exceptions <- talk_about %>%
      filter(!exclude) %>%
      filter(observation %in% c('Mean', 'Variance'), exceptional) %>%
      select(column1, exceptional) %>%
      distinct()

   talk_sections_num_1D <- talk_sections_num_1D %>%
                           left_join(exceptions, by = 'column1') %>%
                           mutate(exceptional = !is.na(exceptional))

   # Categorical 1D data
   talk_sections_cat_1D <- talk_about %>%
      filter(!exclude) %>%
      filter(observation %in% c('Table')) %>%
      select(column1, description, exceptional) %>%
      mutate(comment = comment_1d_cat(description),
             column2 = NA)

   # Numeric 2D data
   talk_sections_num_2D <- talk_about %>%
      filter(!exclude) %>%
      filter(observation %in% c('Correlations')) %>%
      select(column1, column2, description, exceptional) %>%
      mutate(comment = comment_2d_num(description))

   # Categorical 2D
   talk_sections_cat_2D <- talk_about %>%
      filter(!exclude) %>%
      filter(observation %in% c('CatCorrelations')) %>%
      select(column1, column2, description, exceptional) %>%
      mutate(comment = comment_2d_cat(description))

   all_sections <- list(talk_sections_num_1D,
                        talk_sections_cat_1D,
                        talk_sections_num_2D,
                        talk_sections_cat_2D)


   ##################
   # Final WRAP-UP! #
   ##################

   # The exceptional effects are B-S

   # Intro phrase
   intro_phrases <- c(
      '* Let us focus on columns ',
      '* Take a look at columns ',
      '* Observe the following columns: ',
      '* Oberve the columns ',
      '* Focus on columns ',
      '* I am interested in columns ',
      '* I see differences on columns '
   )
   intro_phrase <- intro_phrases[runif(1, min=1, max=length(intro_phrases))]

   all_columns <- lapply(all_sections, function(d) c(d$column1, d$column2))
   all_columns <- sort(unique(na.omit(unlist(all_columns))))

   cat(intro_phrase, paste0(all_columns, collapse = ', '), '.\n', sep = '')


   for (section_data in all_sections){

      if (nrow(section_data) < 1) next


      comments <- unique(section_data$comment)
      cat(' *** ')

      for(i in 1:length(comments)){

         # Fetches what to talks about
         cur_comment <- comments[[i]]
         to_describe <- section_data %>%
                     filter(comment == cur_comment)

         col1 <- to_describe$column1
         col2 <- to_describe$column2

         # Transition word
         if (length(comments) >= 3){

            if (i == 1){
               cap <- TRUE

            } else if (i == length(comments)){

               if (runif(1) > 0.5){
                  cap <- FALSE
                  conc_words <- c(' Finally,', ' To finish,', ' Lastly,')
                  cat(conc_words[runif(1, min=1, max = length(conc_words))])
               } else {
                  cap <- TRUE
               }

            } else {

               if (runif(1) > 0.5) {
               cap <- FALSE
               trans_words <- c(' Additionally,', ' Also,', ' Moreover,')
               cat(trans_words[runif(1, min=1, max = length(trans_words))])

               } else {
               cap <- TRUE
               }
            }

         } else {
            cap <- TRUE
         }


         # Introducing the columns
         if (all(is.na(col2))){

            if (length(col1) == 1){
               cat(ifelse(cap, ' On ', ' on '),
                   ifelse(runif(1) > 0.6, 'column ', ''),
                   col1[1],
                   sep='')

            } else if (length(col1) == 2){
               cat(ifelse(cap, ' On the columns ', ' on columns '),
                   col1[1], ' and ', col1[2], sep='')

            } else {
            cat(ifelse(cap, ' On the columns ', ' on columns '),
                paste0(col1[1:length(col1)-1],collapse=', '),
                ' and ', col1[length(col1)], sep='')
            }


         } else {

            if (length(col1) == 1){
               cat(ifelse(cap, ' Between columns ',' between columns '),
                   col1[1], ' and ', col2[1], sep='')

            } else if (length(col1) == 2){
               cat(ifelse(cap, ' On the columns ', ' on columns '),
                   col1[1], ' and ', col2[1],
                   ', as well as ', col1[2], ' and ', col2[2],
                   sep='')

            } else {
               cat(ifelse(cap, ' On the pairs ', ' on the pairs '), sep = '')
               sapply(1:(length(col1)-1), function(i){
                  cat(col1[i], '-', col2[i], ', ', sep='')
               })
               cat(' and ', col1[length(col1)], '-', col2[length(col1)], sep='')
            }

         }

         # Writing the actual comment
         cat(',', cur_comment)


#          # Exceptional columns
#          col_exc <- to_describe %>%
#                      filter(exceptional = TRUE) %>%
#                      select(column1, column2) %>%
#                      distinct()
#
#          if(nrow(col_exc) > 1 & length(col1) > 1){
#
#             if (nrow(col_exc) == length(col1)){
#                if (runif(1) > 0.5)
#                   cat(' (these effects have an exceptional intensity)')
#                else
#                   cat(' (these are particularly strong effects)')
#
#             } else {
#
#                e_col1 <- col_exc$column1
#                e_col2 <- col_exc$column2
#
#                cat(' (especially on ')
#                if (all(is.na(col2))){
#                   if (length(col1) == 1){
#                      cat(col1[1], sep='')
#                   } else if (length(col1) == 2){
#                      cat(col1[1], ' and ', col1[2], sep='')
#                   } else {
#                      cat(paste0(col1[1:length(col1 -1)],collapse=', '),
#                          ' and ', col1[length(col1)], sep='')
#                   }
#
#                } else {
#                   if (length(col1) == 1){
#                      cat(col1[1], ' and ', col2[1], sep='')
#                   } else if (length(col1) == 2){
#                      cat(col1[1], '-', col2[1],
#                          ', and ', col1[2], '-', col2[2], sep='')
#                   } else {
#                      sapply(1:(length(col1)-1), function(i){
#                         cat(col1[i], '-', col2[i], ', ', sep='')
#                      })
#                      cat(' and ', col1[length(col1)], '-', col2[length(col1)], sep='')
#                   }
#
#                }
#                cat(")")
#
#             }
#
#          }



         # Done!
         cat('.', sep = '')

      }

      cat('\n')



   }

      # Let's not forget the weak effects
      n_forgotten <- talk_about %>%
                     filter(exclude == TRUE) %>%
                     nrow
      if (n_forgotten > 0)
         cat(' ***  I discarded ', n_forgotten/nrow(talk_about) *100,
             '% effects, considered as weak.\n', sep = '')

      # Reports counts of exceptional effects
      n_exceptional <- talk_about %>%
         filter(exceptional== TRUE) %>%
         nrow
      if (n_exceptional > 0)
         cat(' ***  I reported ', n_exceptional/nrow(talk_about) *100,
             '% exceptional effects.\n', sep = '')




}

# Rewriting: bolden columns names
# Include "and", transition words
# Repetitions, Finally
#
# sapply(1:length(views$views), function(i){
#    ziggy_talk_about(views$views[[i]], all_zig_scores)
# })
