# appstore sandbox
library(tidyverse)
appstore_games <- read_csv("Data/appstore_games.csv")

games2 <- separate(appstore_games, `In-app Purchases`, into= c("value1", "value2", "value3", "value4", "value5",
                                                               "value6", "value7", "value8", "value9", "value10",
                                                               "value11", "value12", "value13", "value14", "value15", "value16"),
                   sep = ",", remove = FALSE,convert = TRUE)

games2$mean_purchase_value <- rowMeans(games2[,10:24], na.rm = TRUE)
games2$has_in_app_purchases <- ifelse(games2$mean_purchase_value == "NaN", FALSE, TRUE)

# make any empty space in column names into underscore (_) from janitor package
games2 <- games2 %>%
  janitor::clean_names("snake")

# reduce down to just the games that have any user ratings as a proxy for downloads
games2 <- games2 %>% 
  filter(!is.na(user_rating_count))
library(ggthemes)
games2 %>%
  filter(!is.na(average_user_rating)) %>%
  ggplot(aes(x = fct_rev(as.factor(has_in_app_purchases)))) +
  geom_bar(col = "black", fill = "#0f50d2", alpha = 0.8) +
  labs(title = "Presence of In App Purchases x Age Group",
      caption = "iTunes API data of 7,561 games, pulled on 3rd August 2019 and posted to Kaggle - LonelyManOfData",
      x = "Has In-App Purchases", 
      y = "n")+
  theme_minimal() +
  theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(size = 13, angle = 0, vjust = 0.5),
                 axis.title.x = ggplot2::element_text(size = 13),
                 axis.text.y = ggplot2::element_text(size = 10),
                 axis.text.x = ggplot2::element_text(size = 10),
                 text = element_text(family="Arial", face="bold", size=12)) +
  facet_wrap(~ fct_infreq(as.factor(age_rating))) 

games2 %>%
  filter(!is.na(average_user_rating)) %>%
  ggplot(aes(age_rating, mean_purchase_value)) +
  geom_boxplot() +
  labs(title = "Presence of In App Purchases x Age Group",
       caption = "iTunes API data of 7,561 games, pulled on 3rd August 2019 and posted to Kaggle - LonelyManOfData",
       x = "Has In-App Purchases", 
       y = "n")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = ggplot2::element_text(size = 13, angle = 0, vjust = 0.5),
        axis.title.x = ggplot2::element_text(size = 13),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 10),
        text = element_text(family="Arial", face="bold", size=12))
