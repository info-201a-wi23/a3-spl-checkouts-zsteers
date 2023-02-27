# How has the checkout trends of physical books and ebooks changed over time?
library("dplyr")
library("ggplot2")
library("plotly")
spl_data <- read.csv("/Users/zoesteers/Downloads/2017-2023-10-Checkouts-SPL-Data.csv")

ebook_checkouts_per_year <- spl_data %>% 
  filter(MaterialType == "EBOOK") %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(checkouts_per_year = sum(Checkouts))

physical_book_checkouts_per_year <- spl_data %>% 
  filter(MaterialType == "BOOK") %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  summarise(checkouts_per_year = sum(Checkouts))

ggplot() +
  geom_line(data = ebook_checkouts_per_year, 
            aes(x = CheckoutYear, 
                y = checkouts_per_year, 
                color = MaterialType)) +
  geom_line(data = physical_book_checkouts_per_year, 
            aes(x = CheckoutYear, 
                y = checkouts_per_year, 
                color = MaterialType)) +
  geom_point(data = ebook_checkouts_per_year, 
             aes(x = CheckoutYear, 
                 y = checkouts_per_year)) +
  geom_point(data = physical_book_checkouts_per_year, 
             aes(x = CheckoutYear, 
                 y = checkouts_per_year))

# Average number of checkouts per item?

avg_checkouts <- spl_data %>% 
  summarise(avg = mean(Checkouts)) %>% 
  pull(avg)

# Top 5 most popular publishers

popular_publishers <- spl_data %>% 
  group_by(Publisher) %>% 
  summarize(top_checkouts = sum(n_distinct(Checkouts, na.rm = TRUE))) %>%
  arrange(desc(top_checkouts)) %>% 
  filter(!Publisher %in% c(" ", "NULL")) %>% 
  filter(row_number() <= 5)
bar_plot <- ggplot(data = popular_publishers) +
  geom_col(mapping = aes(
    x = Publisher,
    y = top_checkouts,
    fill = Publisher,
    text = paste("Checkouts:", top_checkouts))
    )
ggplotly(bar_plot, tooltip = "text")

# Usage Class checkouts over time

digital_usage_class <- spl_data %>% 
  filter(UsageClass == "Digital") %>% 
  group_by(UsageClass, CheckoutYear) %>%
  summarize(digital_checkouts_per_year = sum(Checkouts))
physical_usage_class <- spl_data %>% 
  filter(UsageClass == "Physical") %>% 
  group_by(UsageClass, CheckoutYear) %>%
  summarize(physical_checkouts_per_year = sum(Checkouts))
ggplot() +
  geom_line(data = digital_usage_class,
            aes(x = CheckoutYear,
                y = digital_checkouts_per_year,
                color = UsageClass)) +
  geom_line(data = physical_usage_class,
            aes(x = CheckoutYear,
                y = physical_checkouts_per_year,
                color = UsageClass))

# 