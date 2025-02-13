library(ggplot2)
library(ggsci)
library(tidyverse)
library(pixarfilms)
library(showtext)
library(ggrepel)
library(igraph)
library(ggraph)

####################################################################
########################## Graph 1 #################################
####################################################################

#Prepare Data -- Drop NA, Merging duplicated names and select columns-----------
edges <-pixar_people %>%
  drop_na()%>%
  mutate(name= case_when(
    name == "Stanton" ~ "Andrew Stanton",
    name=="Docter" ~ "Pete Docter",
    name=="Lasseter" ~ "John Lasseter",
    name=="Unkrich" ~ "Lee Unkrich",
    name=="Scanlon" ~ "Dan Scanlon",
    name=="Folsom" ~ "Stephany Folsom",
    name=="Ranft" ~ "Joe Ranft",
    name=="Peterson" ~ "Bob Peterson",
    name=="Bird" ~ "Brad Bird",
    name=="Klubien" ~ "Jorgen Klubien",
    name=="Unkrich" ~ "Lee Unkrich",
    name=="Andrews" ~ "Mark Andrews",
    name=="Chapman" ~ "Brenda Chapman",
    name=="Cooley" ~ "John Cooley",
    name=="LeFauve" ~ "Meg LeFauve",
    name=="Sohn" ~ "Peter Sohn",
    name=="Fee" ~ "Brian Fee",
    name=="Aldrich" ~ "Matthew Aldrich",
    name=="Molina" ~ "Adrian Molina",
    TRUE ~ name))%>% 
  select(film, name)

# Search for the top 5 contributors
top_contributors <- edges %>%
  count(name, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(name)

# Select edges and create graph
filtered_edges <- edges %>%
  filter(name %in% top_contributors)
g <- graph_from_data_frame(filtered_edges, directed = FALSE)

V(g)$type <- V(g)$name %in% filtered_edges$film



palette <- c("TRUE" = "#bf0000", "FALSE" = "#53a6a6")

# Plot graph
ggraph(g, layout = "auto") + 
  geom_edge_link(alpha = 0.5, color = "gray70") + 
  geom_node_point(aes(color = as.factor(V(g)$type), size = degree(g)), alpha = 0.8) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 4) + 
  scale_color_manual(values = palette, labels = c("Films", "Contributors")) +  
  theme_void() +  
  labs(title = "Pixar's Architects",
       subtitle = "Showing Pixar's top 5 contributors and the movies they participated in",
       color = "Type", 
       size = "Number of Contributions")


####################################################################
########################## Graph 2 #################################
####################################################################

#Data Preparation :Collect the budget and worldwide Revenue per film -----------
pixar_money_time<- pixar_films %>%
  inner_join(box_office) %>%
  distinct(film, release_date, budget, box_office_worldwide) %>%
  pivot_longer(c(budget,box_office_worldwide),names_to = "money", values_to = "value" ) %>%
  mutate(value = value/1e6)

# Plot Graph 2 : Bar plot Box office revenue and budget over time
ggplot(pixar_money_time,aes(x= release_date, y=value)) +
geom_col(aes(fill = money))+
scale_fill_manual(values= c("#53a6a6","#bf0000"),labels = c("Worldwide Box-Office","Budget"))+
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
)+ annotate("text", x= as.Date("1995-11-22"), y= 450, label= "Toy Story", color = "black", fontface = "bold")+
  annotate("text", x= as.Date("2018-06-15"), y= 1480, label= "Incredibles 2", color = "black", fontface = "bold")+
  annotate("text", x= as.Date("2010-06-18"), y= 1310, label= "Toy Story 3", color = "black", fontface = "bold")+
  annotate("text", x= as.Date("2019-06-21"), y= 1310, label= "Toy Story 4", color = "black",hjust = 0.2, fontface = "bold" )+
  annotate("text", x= as.Date("2020-12-25"), y= 300, label= "Soul", color = "black",hjust = 0.2 , fontface = "bold")

####################################################################
########################## Graph 3 #################################
####################################################################

#Data Preparation : Join box_office, academy, public_response ------------------
pixar_data<-full_join(
  box_office,
  public_response,
  by = join_by(film)
)
pixar_data$year<-format(pixar_films$release_date[1:24],"%Y")

#Data Preparation : Mutating Academy to get the number of Nominations and Oscar won per film
academy_resume <- academy %>%
  mutate(Nominated_Count = ifelse(status %in% c("Nominated", "Won Special Achievement","Won"), 1, 0),
         Won_Count = ifelse(status %in% c("Won","Won Special Achievement"), 1, 0)) %>%
  group_by(film) %>%
  summarise(Nominated = sum(Nominated_Count),
            Won = sum(Won_Count))

pixar_data <- pixar_data %>%
  full_join(academy_resume,by = join_by(film))%>%
  subset(select = c("film","budget", "box_office_worldwide", "rotten_tomatoes", "metacritic","critics_choice","Nominated","Won","year"))%>%
  mutate(Grade = rowMeans(select(., rotten_tomatoes, metacritic, critics_choice), na.rm = TRUE),
         Worth = rowSums(select(., box_office_worldwide, -budget), na.rm = TRUE),
         Era = ifelse(year > 2010, ifelse(year > 2019, "New Age (2020-Now)","Sequel Era (2010-2019)"),"Golden Era (1995-2009)"))%>%
  subset(select = c("film","Worth", "Grade","Nominated","Won","Era"))


#Plot Graph 3 : Map of the Worth of each film and its perceived crtics response

ggplot(pixar_data, aes(x = Grade, y = Worth / 1e6)) +
  geom_rect(aes(xmin = 75, xmax = Inf, ymin = 500, ymax = Inf), fill = "lightgreen", alpha = 0.02) +
  geom_rect(aes(xmin = -Inf, xmax = 75, ymin = 500, ymax = Inf), fill = "lightblue", alpha = 0.02) +
  geom_rect(aes(xmin = 75, xmax = Inf, ymin = -Inf, ymax = 500), fill = "lightyellow", alpha = 0.02) +
  geom_rect(aes(xmin = -Inf, xmax = 75, ymin = -Inf, ymax = 500), fill = "lightpink", alpha = 0.02) +
  geom_point(aes(size = Nominated^2, shape = Era),color = "black") +
  geom_text_repel(aes(label = str_wrap(film, 8)), color = "black", box.padding = unit(1, "lines")) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(color = "black", size = 11, face = "bold")
  ) +
  scale_size_continuous(
    name = "Number of Nominations",
    breaks = c(0, 4, 16, 36),
    labels = c("None", "2 ", "4", "6")  
  ) +
  labs(
    y = "Movie Profit (Millions of US Dollars)",
    x = "Average Grade",
    title = "Pixar's Success: A Data-Driven View",
    caption = "Data: Eric T. Leung"
  ) +
  annotate("text", x = 85, y = 900, label = "Pixar Legends️", size =4, fontface = "bold", color = "darkgreen") +
  annotate("text", x = 60, y = 900, label = "Box Office Hits ", size = 4, fontface = "bold", color = "darkblue") +
  annotate("text", x = 85, y = 200, label = "Critics' Darlings", size = 4, fontface = "bold", color = "goldenrod") +
  annotate("text", x = 60, y = 200, label = "Underdogs", size = 4, fontface = "bold", color = "darkred") #+
  
