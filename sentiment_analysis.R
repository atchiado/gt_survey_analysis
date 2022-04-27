library(tidytext)
library(textdata)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(syuzhet)


#### DATA PREP ####
## load and split data
survey <- read.csv(file = "~/Desktop/work/data/r/gt_survey_analysis/example_responses.csv")
survey_df <- tibble(survey)

afinn_sentiment <- get_sentiments("afinn")
bing_sentiment <- get_sentiments("bing")

q1_df <- survey_df[survey_df$Question == '1',]
q2_df <- survey_df[survey_df$Question == '2',]
q3_df <- survey_df[survey_df$Question == '3',]
q4_df <- survey_df[survey_df$Question == '4',]
q5_df <- survey_df[survey_df$Question == '5',]
q6_df <- survey_df[survey_df$Question == '6',]
q7_df <- survey_df[survey_df$Question == '7',]


## create character vector of answers only
survey_char <- survey_df %>%
                  subset(select = Answer) %>%
                    as.character()
q1_char <- q1_df %>%
              subset(select = Answer) %>%
                as.character()
q2_char <- q2_df %>%
              subset(select = Answer) %>%
                as.character()
q3_char <- q3_df %>%
              subset(select = Answer) %>%
                as.character()
q4_char <- q4_df %>%
              subset(select = Answer) %>%
                as.character()
q5_char <- q5_df %>%
              subset(select = Answer) %>%
                as.character()
q6_char <- q6_df %>%
              subset(select = Answer) %>%
                as.character()
q7_char <- q7_df %>%
              subset(select = Answer) %>%
                as.character()


## convert data frames to tidy text data
survey_df_tidy <- unnest_tokens(survey_df, word, Answer)
q1_df_tidy <- unnest_tokens(q1_df, word, Answer)
q2_df_tidy <- unnest_tokens(q2_df, word, Answer)
q3_df_tidy <- unnest_tokens(q3_df, word, Answer)
q4_df_tidy <- unnest_tokens(q4_df, word, Answer)
q5_df_tidy <- unnest_tokens(q5_df, word, Answer)
q6_df_tidy <- unnest_tokens(q6_df, word, Answer)
q7_df_tidy <- unnest_tokens(q7_df, word, Answer)


## inner join with sentiment data and aggregate by subject
agg_survey_afinn_sent <- survey_df_tidy %>%
                           inner_join(afinn_sentiment) %>%
                             subset(select = -word) %>%
                               group_by(Subject) %>%
                                 summarise(mean = mean(value)) %>%
                                   mutate(pos = mean >= 0)
agg_q1_afinn_sent <- q1_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0)
agg_q2_afinn_sent <- q2_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0)
agg_q3_afinn_sent <- q3_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0)                         
agg_q4_afinn_sent <- q4_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0)   
agg_q5_afinn_sent <- q5_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0)  
agg_q6_afinn_sent <- q6_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0) 
agg_q7_afinn_sent <- q7_df_tidy %>%
                       inner_join(afinn_sentiment) %>%
                         subset(select = -word) %>%
                           group_by(Subject) %>%
                             summarise(mean = mean(value)) %>%
                               mutate(pos = mean >= 0) 

survey_bing_sent <- inner_join(survey_df_tidy, bing_sentiment)
q1_bing_sent <- inner_join(q1_df_tidy, bing_sentiment)
q19_bing_sent <- inner_join(q19_df_tidy, bing_sentiment)
q33_bing_sent <- inner_join(q33_df_tidy, bing_sentiment)
q37_bing_sent <- inner_join(q37_df_tidy, bing_sentiment)
q38_bing_sent <- inner_join(q38_df_tidy, bing_sentiment)
q39_bing_sent <- inner_join(q39_df_tidy, bing_sentiment)
q40_bing_sent <- inner_join(q40_df_tidy, bing_sentiment)
q42_bing_sent <- inner_join(q42_df_tidy, bing_sentiment)


## create nrc emolex data
survey_nrc_df <- survey_char %>%
                    get_nrc_sentiment() %>%
                      gather(columnNames, values) %>%
                        subset(columnNames!="positive" & columnNames!="negative")
q1_nrc_df <- q1_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q2_nrc_df <- q2_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q3_nrc_df <- q3_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q4_nrc_df <- q4_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q5_nrc_df <- q5_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q6_nrc_df <- q6_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")
q7_nrc_df <- q7_char %>%
                get_nrc_sentiment() %>%
                  gather(columnNames, values) %>%
                    subset(columnNames!="positive" & columnNames!="negative")




#### Overall Survey Viz ####
## visualize sentiment by subject
ggplot(agg_survey_afinn_sent, aes(x = reorder(Subject, mean), y = mean, fill = pos)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous("Occurrences", limits = c(-3, 3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  labs(title = "Overall Sentiment of Survey Responses",
       x = "Subject",
       y = "Sentiment") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip()


## visualize most commonly used pos and neg words
survey_bing_sent %>%
  count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
      group_by(sentiment) %>%
        slice_max(n, n = 10) %>%
          ungroup() %>%
            mutate(word = reorder(word, n)) %>%
              ggplot(aes(n, word, fill = sentiment)) +
              geom_col(show.legend = FALSE) +
              facet_wrap(~sentiment, scales = "free_y") +
              scale_x_continuous("Occurrences", limits = c(0, 15), breaks = c(0, 5, 10, 15)) +
              labs(title = "Most Commonly Used Positive and Negative Words",
                   y = "Word") +
              theme(panel.grid.major.y = element_blank())


## visualize nrc emotion categorization
ggplot(survey_nrc_df, aes(x = columnNames, y = values, fill = columnNames)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("anger" = "#F8766D", "anticipation" = "#FFCC66",
                               "disgust" = "#F8766D", "fear" = "#F8766D", "joy" = "#00BFC4",
                               "sadness" = "#F8766D", "surprise" = "#FFCC66", "trust" = "#00BFC4")) +
  labs(title = "Emotion Classification of Survey Responses",
       x = "Emotions",
       y = "Prominence") +
  theme(panel.grid.major.x = element_blank())




#### Question 1 Viz ####
## visualize sentiment across subject
ggplot(agg_q1_afinn_sent, aes(x = reorder(Subject, mean), y = mean, fill = pos)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous("Occurrences", limits = c(-3, 3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  labs(title = "Sentiment of Survey Responses",
       subtitle = "Question 1",
       x = "Responses",
       y = "Sentiment") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip()


## visualize most common pos and neg words
q1_bing_sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
      group_by(sentiment) %>%
        slice_max(n, n = 10) %>%
          ungroup() %>%
            mutate(word = reorder(word, n)) %>%
              ggplot(aes(n, word, fill = sentiment)) +
              geom_col(show.legend = FALSE) +
              facet_wrap(~sentiment, scales = "free_y") +
              scale_x_continuous("Occurrences", limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
              labs(title = "Most Commonly Used Positive and Negative Words",
                   subtitle = "Question 1",
                   y = "Word") +
              theme(panel.grid.major.y = element_blank())


## visualize nrc emotion categorization
ggplot(q1_nrc_df, aes(x = columnNames, y = values, fill = columnNames)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("anger" = "#F8766D", "anticipation" = "#FFCC66",
                               "disgust" = "#F8766D", "fear" = "#F8766D", "joy" = "#00BFC4",
                               "sadness" = "#F8766D", "surprise" = "#FFCC66", "trust" = "#00BFC4")) +
  labs(title = "Emotion Classification of Survey Responses",
       subtitle = "Question 1",
       x = "Emotions",
       y = "Prominence") +
  theme(panel.grid.major.x = element_blank())