library(ggplot2)
library(ggsci)
library(tidyverse)
library(pixarfilms)
library(showtext)
library(ggrepel)

####################################################################
########################## Graph 1 #################################
####################################################################


####################################################################
########################## Graph 2 #################################
####################################################################

#Data Preparation :Collect the budget and worldwide Revenue per film -----------
pixar_money_time<- pixar_films %>%
  inner_join(box_office) 

pixar_money_time %>%
  distinct(film, release_date, budget, box_office_worldwide) %>%
  pivot_longer(c(budget,box_office_worldwide),names_to = "money", values_to = "value" ) %>%
  mutate(value = value/1e6)

# Plot Graph 2 : Bar plot Box office revenue and budget over time
ggplot(pixar_money_time,aes(x= release_date, y=value)) +
geom_col(aes(fill = money))+
scale_fill_manual(values= c("#53a6a6","#bf0000"))+
scale_x_date(date_breaks = "1 year", date_labels = "%Y",guide = guide_axis(n.dodge = 2)) +
theme(
  panel.grid.major.y = element_line(color = "gray75", linewidth = 0.3),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 12, color = "black"),
  axis.title = element_text(size = 14, face = "bold"),
  legend.title = element_blank(),
  legend.position = "top",
  plot.title = element_text(size = 14, face = "bold")
) +
labs(
  x = "Year of Release",
  y = "Box Office Earnings (in Millions of US$)",
  title = "Pixar: A Billion-Dollar Franchise",
  subtitle = "Comparing Budgets vs. Revenues Over Time",
  caption = "Data by Eric Leung"
) +
geom_text(data = pixar_plot_data %>% filter(value == max(value)), 
          aes(label = film), vjust = -0.5, size = 4, color = "black")  # Highlight top-grossing film



####################################################################
########################## Graph 3 #################################
####################################################################

#Data Preparation : Join box_office, academy, public_response ------------------
pixar_data<-full_join(
  box_office,
  public_response,
  by = join_by(film)
)

#Data Preparation : Mutating Academy to get the number of Nominations and Oscar won per film
academy_resume <- academy %>%
  mutate(Nominated_Count = ifelse(status %in% c("Nominated", "Won Special Achievement","Won"), 1, 0),
         Won_Count = ifelse(status %in% c("Won","Won Special Achievement"), 1, 0)) %>%
  group_by(film) %>%
  summarise(Nominated = sum(Nominated_Count),
            Won = sum(Won_Count))

pixar_data <- pixar_data %>%
  full_join(academy_resume,by = join_by(film))%>%
  subset(select = c("film","budget", "box_office_worldwide", "rotten_tomatoes", "metacritic","critics_choice","Nominated","Won"))%>%
  mutate(Grade = rowMeans(select(., rotten_tomatoes, metacritic, critics_choice), na.rm = TRUE),
         Worth = rowSums(select(., box_office_worldwide, -budget), na.rm = TRUE))%>%
  subset(select = c("film","Worth", "Grade","Nominated","Won"))

#Plot Graph 3 : Map of the Worth of each film and its perceived crtics response
ggplot(pixar_data,aes(x = Grade, y = Worth/1e6)) +
  geom_point(aes(size =Nominated^2, color = Won > 0))+
  geom_text_repel(aes(label = str_wrap(film,8)), color= "black",box.padding = unit(1, "lines")) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(color = "black",
                                size = 11, face = "bold")
  ) +
  scale_color_manual(
    values = c("#bf0000", "#53a6a6"),
    labels = c("Won at least 1 Oscar", "Didn't win"),
    name = "Statut aux Oscars"
  )+
  scale_size_continuous(
    name = "Number of Nominations",
    breaks = c(0, 4, 16, 36),
    labels = c("None", "2 ", "4", "6") # Personnalisation des labels
  ) +
  labs(y= "Average grade of the movie",
       x="Movie Profit (Millions of US Dollars)",
       title = "Pixar's success",
       caption = 'Data : Eric T. Leung',
       color = "Oscar Status",
       size = 'Times Nominated for the Oscar'
  )