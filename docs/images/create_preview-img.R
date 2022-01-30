library(tidyverse)
library(worldfootballR)
library(janitor)
library(ggtext)

sl_table <- worldfootballR::tm_matchday_table(country_name = "Denmark",
                              start_year = "2021",
                              matchday = seq(1:17)) %>% 
  clean_names()

sl_table <- sl_table %>% 
  mutate(squad = gsub("ö", "ø", squad)) %>% 
  mutate(squad = gsub("ae", "æ", squad))

temp <- sl_table %>% 
  filter(matchday == 1) %>% 
  mutate(across(c(p, matchday, pts, w, d, l, gf, ga, g_diff),
                ~ 0)) %>% 
  arrange(squad) %>% 
  mutate(rk = row_number())

plot_df <- rbind(temp,
                 sl_table)

plot_df2 <- plot_df %>% 
  rename(squad2 = squad)

ggplot() + 
  geom_line(data = plot_df2,
            aes(x = matchday, y = pts, group = squad2),
            colour = "white",
            size = .1,
            alpha = 0.5) +
  geom_line(data = plot_df,
            aes(x = matchday, y = pts, group = squad),
            color = "chartreuse3",
            size = 0.75) + 
  facet_wrap(~ squad, scales = "fixed") +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "gray14", fill = "gray14"),
        panel.background = element_rect(color = "gray14", fill = "gray14"),
        panel.grid.major = element_line(color = "gray40", size = .1),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(color = "white",
                                  size = 20,
                                  family = "Montserrat"),
        plot.subtitle = element_text(color = "white",
                                     family = "Montserrat"),
        plot.caption = ggtext::element_markdown(color = "gray70",
                                                family = "Montserrat"),
        legend.position = "none")

getwd()
ggsave(plot = last_plot(),
       filename = "images/sl_plot.png",
       dpi = 320, scale = 1, width = 7, height = 6, units = c("in"))

