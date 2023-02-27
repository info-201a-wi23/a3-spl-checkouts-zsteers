# How has the checkout trends of physical books and ebooks changed over time?
library("dplyr")
library("ggplot2")
library("plotly")
spl_data <- read.csv("/Users/zoesteers/Downloads/2017-2023-10-Checkouts-SPL-Data.csv")
ebook_checkouts_per_year <- spl_data %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear, MaterialType) %>% summarise(checkouts_per_year = sum(Checkouts))
physical_book_checkouts_per_year <- spl_data %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear, MaterialType) %>% summarise(checkouts_per_year = sum(Checkouts))
ggplot() +
  geom_line(data = ebook_checkouts_per_year, aes(x = CheckoutYear, y = checkouts_per_year, color = MaterialType)) +
  geom_line(data = physical_book_checkouts_per_year, aes(x = CheckoutYear, y = checkouts_per_year, color = MaterialType)) +
  geom_point(data = ebook_checkouts_per_year, aes(x = CheckoutYear, y = checkouts_per_year)) +
  geom_point(data = physical_book_checkouts_per_year, aes(x = CheckoutYear, y = checkouts_per_year))
# Average number of checkouts per item?
avg_checkouts <- spl_data %>% summarise(avg = mean(Checkouts)) %>% pull(avg)
# Top 5 most popular subjects
popular_subjects <- spl_data %>% group_by(Subjects) %>%
