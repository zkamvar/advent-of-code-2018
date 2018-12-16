library("tidyverse")
library("here")
first_attempt  <- here("d09", "zkamvar-pt2-timings.txt")
second_attempt <- here("d09", "zkamvar-pt2-timings-2.txt")
get_times <- . %>%
  read_table(col_names = c("Date", "Time", "Marbles")) %>%
  mutate(seconds = lead(Time) - Time)

tim <- bind_rows(first  = first_attempt %>% get_times,
                 second = second_attempt %>% get_times,
                 .id    = "run"
                ) %>%
  mutate(run = fct_recode(run, "without parent" = "first", 
                               "with parent" = "second"))
{
p <- ggplot(tim, aes(x = Marbles, y = seconds, group = run)) +
  geom_line(aes(linetype = run, color = run), size = 1.25) +
  scale_color_grey(start = 0.6, end = 0.1) +
  scale_linetype_manual(values = c(2, 1), guide = "none") + 
  scale_x_continuous(labels = scales::comma) + 
  theme_minimal(base_size = 16) +
  theme(legend.position = c(0.2, 0.7)) +
  theme(axis.text = element_text(color = "grey40")) +
  theme(plot.caption = element_text(color = "grey60")) + 
  labs(title = "Insertion times for a 7.1M marble game",
       caption = "Advent of code day 9 using R",
       x     = "Number of marbles before allocation",
       y     = "Time (seconds) to insert 100K marbles",
       color = "Using environments") 
}
ggsave(file = here("d09", "zkamvar-timings.png"), width = 10)
