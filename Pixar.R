library(ggplot2)
library(ggsci)
library(tidyverse)
library(pixarfilms)
library(showtext)
library(ggrepel)

#join box_office, academy, public_response 
df_3<-full_join(
  box_office,
  public_response,
  by = join_by(film)
)

academy_resume <- academy %>%
  mutate(Nominated_Count = ifelse(status %in% c("Nominated", "Won Special Achievement","Won"), 1, 0),
         Won_Count = ifelse(status %in% c("Won","Won Special Achievment"), 1, 0)) %>%
  group_by(film) %>%
  summarise(Nominated = sum(Nominated_Count),
            Won = sum(Won_Count))

df_3<-full_join(
  df_3,
  academy_resume,
  by = join_by(film)
)

df_3<- subset(df_3, select = c("film","budget", "box_office_worldwide", "rotten_tomatoes", "metacritic","critics_choice","Nominated","Won"  ))

df_3 <- df_3 %>%
  mutate(Grade = rowMeans(select(., rotten_tomatoes, metacritic, critics_choice), na.rm = TRUE))%>%
  subset( select = c("film","budget", "box_office_worldwide", "Grade","Nominated","Won"  ))
df_3 <- df_3 %>%
  mutate(Worth = rowSums(select(., box_office_worldwide, -budget), na.rm = TRUE))%>%
  subset( select = c("film","Worth", "Grade","Nominated","Won"  ))

ggplot(df_3,aes(y=Worth/1e6, x =Grade)) +
geom_point(aes(size =Nominated*4), fill = "#bf0000", color = "#bf0000")+
geom_text_repel(aes(label = str_wrap(film,10)), color= "black",box.padding = unit(1, "lines")) +
theme_light() +
theme(,
  panel.grid = element_blank()   
  ) +
labs(y= "Average grade of the movie",
      x="Movie Profit (Millions od US Dollars)",
      title = "Pixar's success",
     caption = 'Data : Eric T. Leung')

pixar_money_time<- pixar_films %>%
  inner_join(box_office) 

pixar_money_time %>%
  distinct(film, release_date, budget, box_office_worldwide) %>%
  pivot_longer(c(budget,box_office_worldwide),names_to = "money", values_to = "value" ) %>%
  mutate(value = value/1e6) %>%
  ggplot(aes(x= release_date, y=value)) +
  geom_col(aes(fill = money))
  scale_fill_manual(values= c("#53a6a6","#bf0000"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",guide = guide_axis(n.dodge = 2)) +
  
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs( x= "Release date",
        y="Box-office worldwide (US$ mi)")

