library(tidytext)
library(textdata)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(syuzhet)


## load and split data
survey <- read.csv(file = "ENTER HERE")
survey_df <- tibble(survey)

survey_df <- rename(survey_df, Question = ENTER HERE)

afinn_sentiment <- get_sentiments("afinn")
bing_sentiment <- get_sentiments("bing")


## create character vector of answers only
survey_answers_char <- subset(survey_dfm, select = Answer)
survey_answers_char <- as.character(survey_answers_char)


## convert to tidy text data
survey_df_tidy <- unnest_tokens(survey_df, word, Answer)


## inner join with sentiment data
survey_afinn_sentiment <- inner_join(survey_df_tidy, afinn_sentiment)

survey_bing_sentiment <- inner_join(survey_df_tidy, bing_sentiment)


## aggregate by survey subject
agg_survey_afinn_sent <- subset(survey_afinn_sentiment, select = -word)

agg_survey_afinn_sent <- agg_survey_afinn_sent %>%
                              group_by(Subject) %>%
                              summarise(mean = mean(value))

agg_survey_afinn_sent <- agg_survey_afinn_sent %>%
                              mutate(pos = mean >= 0)


## create nrc emolex data
nrc_survey_data <- get_nrc_sentiment(survey_answers_char)
nrc_survey_data_df <- gather(nrc_survey_data, columnNames, values)
nrc_survey_data_df <- subset(nrc_survey_data_df, columnNames!="positive" & columnNames!="negative")


