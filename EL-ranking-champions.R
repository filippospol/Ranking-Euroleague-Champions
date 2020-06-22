library(tidyverse)
library(ggbump)
library(cowplot)
library(ggrepel)
library(ggimage)
library(gt)

# BUMP for teams that won EL Title during 2011-2020 ----------------------------------------------

colors <- c("darkorchid", "royalblue4", "gold1", "firebrick1","forestgreen", "dodgerblue2")
Team <- rep(c("CSKA Moscow","Fenerbahce","Maccabi",
              "Olympiacos","Panathinaikos","Real Madrid"),10)
tm <- rep(c("CSKA","FNB","MAC","OLY","PAO","RMD"),10)
year <- c(rep(2011,6),rep(2012,6),rep(2013,6),rep(2014,6),rep(2015,6),
          rep(2016,6),rep(2017,6),rep(2018,6),rep(2019,6),rep(2020,6))

netrtg <- c(-8.4,10.1,13.5,10.1,13.8,10.4,
            19.0,3.7,8.0,3.4,13.0,14.3,
            10.3,-1.6,14.3,7.1,3.6,13.0,
            8.1,14.1,7.4,11.0,4.5,32.6,
            22.8,7.8,1.9,5.2,3.5,13.1,
            17.4,8.9,-5.9,9.6,6.4,6.2,
            10.3,2,-7.6,5.2,4.3,10.6,
            13.5,8.1,-4.1,1.5,2,8.8,
            9.0,12.4,1.4,1.4,1.5,10.8,
            8.7,-1.6,6.8,-2.1,-0.3,10.2)

champ <- c(0,0,0,0,1,0,
           0,0,0,1,0,0,
           0,0,0,1,0,0,
           0,0,1,0,0,0,
           0,0,0,0,0,1,
           1,0,0,0,0,0,
           0,1,0,0,0,0,
           0,0,0,0,0,1,
           1,0,0,0,0,0,
           0,0,0,0,0,0)


net2 <- data.frame(Team,tm,year,netrtg, champ)

net2 <- net2 %>% 
  group_by(year) %>% 
  mutate(rank = rank(-netrtg, ties.method = "random")) %>% 
  ungroup()


my_bump <- ggplot(net2, aes(year, -rank, color = Team)) +
  geom_bump(size = 1.5, smooth = 6) + 
  geom_point(size = ifelse(net2$champ == 1, 8, 1.5)) +
  scale_x_continuous(limits = c(2010,2021), breaks = seq(2011, 2020, 1)) +
  scale_y_continuous(limits = c(-6,0),breaks = NULL) +
  labs(title = "Ranking EL champions based on Net Rating \n(2011-20)",
       subtitle = "Bigger point indicates champions", y = "Rank", x = NULL,
       caption = "Source: @acbbstatscy")  + 
  theme(legend.background = element_rect(color = NA, fill = "gray20"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.title = element_text(face= "bold"),
        legend.text = element_text(),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray20", color = "transparent"),
        plot.background = element_rect(fill = "gray20"),
        text = element_text(color = "white"),
        axis.text.x = element_text(size = 10, color = "white"),
        plot.title = element_text(size = 16, face="bold", color = "white")) +
  #geom_image(data = net2 %>% filter(champ == 1),aes(image=image), 
  #           size=0.09) +                
  scale_color_manual(values = colors) + 
  geom_text(data = tibble(x = 2010, y = -6:-1), aes(x = x, y = y, label = -y), 
            inherit.aes = F,
            color = "white")




new_net <- matrix(c(2011, "-8.4 (20th)", "10.1 (5th)", "13.5 (3rd)", "13.8 (2nd)", "10.1 (6th)", "10.4 (4th)", 
                    2012, "19.0 (2nd)", "3.7 (9th)", "8.0 (6th)", "13.0 (4th)", "3.4 (10th)", "14.3 (3rd)",
                    2013, "10.3 (5th)", "-1.6 (13th)", "14.3 (3rd)", "3.6 (9th)", "7.1 (6th)", "13.0 (4th)",
                    2014, "8.1 (5th)", "14.1 (2nd)", "7.4 (6th)", "4.5 (8th)", "11.0 (3rd)", "32.6 (1st)",
                    2015, "22.8 (1st)", "7.8 (4th)", "1.9 (10th)", "3.5 (9th)", "5.2 (6th)", "13.1 (3rd)",
                    2016, "17.4 (1st)", "8.9 (6th)", "-5.9 (17th)", "6.4 (10th)", "9.6 (5th)", "6.2 (12th)",
                    2017, "10.3 (2nd)", "2.0 (6th)", "-7.6 (15th)", "4.3 (4th)", "5.2 (3rd)", "10.6 (1st)",
                    2018, "13.5 (1st)", "8.1 (3rd)", "-4.1 (11th)", "2.0 (6th)", "1.5 (7th)", "8.8 (2nd)",
                    2019, "9.0 (3rd)", "12.4 (1st)", "1.4 (9th)", "1.5 (8th)", "1.4 (10th)", "10.8 (2nd)",
                    2020, "8.7 (3rd)", "-1.6 (10th)", "6.8 (5th)", "0.3 (7th)", "-2.1 (11th)", "10.2 (2nd)"),
                  10,7,byrow=T)
new_net <- data.frame(new_net)
colnames(new_net) <- c("Year", Team[c(1,2,3,5,4,6)])
new_net$champ <- c(5,4,4,3,6,1,2,6,1,0)

my_gt <- gt(new_net)
my_gt <- my_gt %>% 
  tab_header(
    title = md("**Ranking EL champions based on Net Rating (2011-20)**"),
    subtitle = "Coloured cells indicate champions. \nNumbers in parentheses show overall ranking "
  )   %>%
  tab_source_note(md("Note that EL format changed at 2017 (from 24 to 16 teams) and in 2020 (from 16 to 18 teams)"))

my_gt <- my_gt %>% 
  cols_align(align = c("right"), columns = TRUE)

my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "forestgreen"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 1,
      columns = 5))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "firebrick1"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 2,
      columns = 6))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "firebrick1"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 3,
      columns = 6))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "gold1"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 4,
      columns = 4))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "dodgerblue2"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 5,
      columns = 7))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "darkorchid"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 6,
      columns = 2))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "royalblue4"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 7,
      columns = 3))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "dodgerblue2"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 8,
      columns = 7))
my_gt <- my_gt %>% 
  tab_style(
    style = list(cell_fill(color = "darkorchid"),
                 cell_text(color = "white")),
    locations = cells_body(
      rows = 9,
      columns = 2))

my_gt <- my_gt %>% 
  tab_source_note(md("Source: @acbbstatscy"))

my_gt %>% 
  cols_hide(columns = vars(champ))