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

q18_df <- survey_df[survey_df$Question == '18',]
q19_df <- survey_df[survey_df$Question == '19',]
q33_df <- survey_df[survey_df$Question == '33',]
q37_df <- survey_df[survey_df$Question == '37',]
q38_df <- survey_df[survey_df$Question == '38',]
q39_df <- survey_df[survey_df$Question == '39',]
q40_df <- survey_df[survey_df$Question == '40',]
q42_df <- survey_df[survey_df$Question == '42',]


## create character vector of answers only
survey_answers_char <- subset(survey_df, select = Answer)
q18_answers_char <- subset(q18_df, select = Answer)
q19_answers_char <- subset(q19_df, select = Answer)
q33_answers_char <- subset(q33_df, select = Answer)
q37_answers_char <- subset(q37_df, select = Answer)
q38_answers_char <- subset(q38_df, select = Answer)
q39_answers_char <- subset(q39_df, select = Answer)
q40_answers_char <- subset(q40_df, select = Answer)
q42_answers_char <- subset(q42_df, select = Answer)

survey_answers_char <- as.character(survey_answers_char)
q18_answers_char <- as.character(q18_answers_char)
q19_answers_char <- as.character(q19_answers_char)
q33_answers_char <- as.character(q33_answers_char)
q37_answers_char <- as.character(q37_answers_char)
q38_answers_char <- as.character(q38_answers_char)
q39_answers_char <- as.character(q39_answers_char)
q40_answers_char <- as.character(q40_answers_char)
q42_answers_char <- as.character(q42_answers_char)


## convert to tidy text data
survey_df_tidy <- unnest_tokens(survey_df, word, Answer)
q18_df_tidy <- unnest_tokens(q18_df, word, Answer)
q19_df_tidy <- unnest_tokens(q19_df, word, Answer)
q33_df_tidy <- unnest_tokens(q33_df, word, Answer)
q37_df_tidy <- unnest_tokens(q37_df, word, Answer)
q38_df_tidy <- unnest_tokens(q38_df, word, Answer)
q39_df_tidy <- unnest_tokens(q39_df, word, Answer)
q40_df_tidy <- unnest_tokens(q40_df, word, Answer)
q42_df_tidy <- unnest_tokens(q42_df, word, Answer)


## inner join with sentiment data
survey_afinn_sentiment <- inner_join(survey_df_tidy, afinn_sentiment)
q18_afinn_sentiment <- inner_join(q18_df_tidy, afinn_sentiment)
q19_afinn_sentiment <- inner_join(q19_df_tidy, afinn_sentiment)
q33_afinn_sentiment <- inner_join(q33_df_tidy, afinn_sentiment)
q37_afinn_sentiment <- inner_join(q37_df_tidy, afinn_sentiment)
q38_afinn_sentiment <- inner_join(q38_df_tidy, afinn_sentiment)
q39_afinn_sentiment <- inner_join(q39_df_tidy, afinn_sentiment)
q40_afinn_sentiment <- inner_join(q40_df_tidy, afinn_sentiment)
q42_afinn_sentiment <- inner_join(q42_df_tidy, afinn_sentiment)

survey_bing_sentiment <- inner_join(survey_df_tidy, bing_sentiment)
q18_bing_sentiment <- inner_join(q18_df_tidy, bing_sentiment)
q19_bing_sentiment <- inner_join(q19_df_tidy, bing_sentiment)
q33_bing_sentiment <- inner_join(q33_df_tidy, bing_sentiment)
q37_bing_sentiment <- inner_join(q37_df_tidy, bing_sentiment)
q38_bing_sentiment <- inner_join(q38_df_tidy, bing_sentiment)
q39_bing_sentiment <- inner_join(q39_df_tidy, bing_sentiment)
q40_bing_sentiment <- inner_join(q40_df_tidy, bing_sentiment)
q42_bing_sentiment <- inner_join(q42_df_tidy, bing_sentiment)


## aggregate by survey subject
agg_survey_afinn_sent <- subset(survey_afinn_sentiment, select = -word)
agg_q18_afinn_sent <- subset(q18_afinn_sentiment, select = -word)
agg_q19_afinn_sent <- subset(q19_afinn_sentiment, select = -word)
agg_q33_afinn_sent <- subset(q33_afinn_sentiment, select = -word)
agg_q37_afinn_sent <- subset(q37_afinn_sentiment, select = -word)
agg_q38_afinn_sent <- subset(q38_afinn_sentiment, select = -word)
agg_q39_afinn_sent <- subset(q39_afinn_sentiment, select = -word)
agg_q40_afinn_sent <- subset(q40_afinn_sentiment, select = -word)
agg_q42_afinn_sent <- subset(q42_afinn_sentiment, select = -word)

agg_survey_afinn_sent <- agg_survey_afinn_sent %>%
                              group_by(Subject) %>%
                                  summarise(mean = mean(value))
agg_q18_afinn_sent <- agg_q18_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q19_afinn_sent <- agg_q19_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q33_afinn_sent <- agg_q33_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q37_afinn_sent <- agg_q37_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q38_afinn_sent <- agg_q38_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q39_afinn_sent <- agg_q39_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q40_afinn_sent <- agg_q40_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))
agg_q42_afinn_sent <- agg_q42_afinn_sent %>%
                          group_by(Subject) %>%
                              summarise(mean = mean(value))

agg_survey_afinn_sent <- agg_survey_afinn_sent %>%
                            mutate(pos = mean >= 0)
agg_q18_afinn_sent <- agg_q18_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q19_afinn_sent <- agg_q19_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q33_afinn_sent <- agg_q33_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q37_afinn_sent <- agg_q37_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q38_afinn_sent <- agg_q38_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q39_afinn_sent <- agg_q39_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q40_afinn_sent <- agg_q40_afinn_sent %>%
                          mutate(pos = mean >= 0)
agg_q42_afinn_sent <- agg_q42_afinn_sent %>%
                          mutate(pos = mean >= 0)


## create nrc emolex data
nrc_survey_data <- get_nrc_sentiment(survey_answers_char)
nrc_survey_data_df <- gather(nrc_survey_data, columnNames, values)
nrc_survey_data_df <- subset(nrc_survey_data_df, columnNames!="positive" & columnNames!="negative")

nrc_q38_data <- get_nrc_sentiment(q38_answers_char)
nrc_q38_data_df <- gather(nrc_q38_data, columnNames, values)
nrc_q38_data_df <- subset(nrc_q38_data_df, columnNames!="positive" & columnNames!="negative")


#### Overall Survey Viz ####
## visualize sentiment by subject
ggplot(agg_survey_afinn_sent, aes(x = reorder(Subject, mean), y = mean, fill = pos)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Overall Sentiment of Survey Responses",
       x = "Subject",
       y = "Sentiment") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip()


## visualize most commonly used pos and neg words
survey_word_counts <- survey_bing_sentiment %>%
                          count(word, sentiment, sort = TRUE) %>%
                              ungroup()
survey_word_counts %>%
  group_by(sentiment) %>%
      slice_max(n, n = 10) %>%
          ungroup() %>%
              mutate(word = reorder(word, n)) %>%
                  ggplot(aes(n, word, fill = sentiment)) +
                  geom_col(show.legend = FALSE) +
                  facet_wrap(~sentiment, scales = "free_y") +
                  scale_x_continuous("Occurrences", limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
                  labs(title = "Most Commonly Used Positive and Negative Words",
                       y = "Word") +
                  theme(panel.grid.major.y = element_blank())


## visualize nrc emotion categorization
ggplot(nrc_survey_data_df, aes(x = columnNames, y = values, fill = columnNames)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Emotion Classification of Survey Responses",
       x = "Emotions",
       y = "Prominence") +
  theme(panel.grid.major.x = element_blank())


#### Question 38 Viz ####
## visualize sentiment across subject
ggplot(agg_q38_afinn_sent, aes(x = reorder(Subject, mean), y = mean, fill = pos)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment of Survey Responses",
       subtitle = "Question 38: question",
       x = "Responses",
       y = "Sentiment") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  coord_flip()


## visualize most common pos and neg words
q38_word_counts <- q38_bing_sentiment %>%
                      count(word, sentiment, sort = TRUE) %>%
                          ungroup()
q38_word_counts %>%
  group_by(sentiment) %>%
      slice_max(n, n = 10) %>%
          ungroup() %>%
              mutate(word = reorder(word, n)) %>%
                  ggplot(aes(n, word, fill = sentiment)) +
                  geom_col(show.legend = FALSE) +
                  facet_wrap(~sentiment, scales = "free_y") +
                  scale_x_continuous("Occurrences", limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
                  labs(title = "Most Commonly Used Positive and Negative Words",
                       subtitle = "Question 38: question",
                       y = "Word") +
                  theme(panel.grid.major.y = element_blank())


## visualize nrc emotion categorization
ggplot(nrc_q38_data_df, aes(x = columnNames, y = values, fill = columnNames)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Emotion Classification of Survey Responses",
       subtitle = "Question 38: question",
       x = "Emotions",
       y = "Prominence") +
  theme(panel.grid.major.x = element_blank())