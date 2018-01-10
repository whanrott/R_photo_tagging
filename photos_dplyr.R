library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

source("run_exiftool.R")
run_exiftool()

## define new date type. This is nice, but I don't actually need it here.
# setClass('myDate')
# setAs("character","myDate", function(from) as.Date(from, format="%Y:%m:%d %H:%M:%S") )

# read in data
image_df <- read_csv("photos.txt")
image_df$DateTimeOriginal <- as.Date(strptime(image_df$DateTimeOriginal, format = "%Y:%m:%d %T"))

# Tag field contains irregular number of values. Reshape the data frame by breaking the tag field
# apart, then return 3 relevant columns
images_tidy <- select(image_df,  SourceFile, DateTimeOriginal, LastKeywordXMP) %>%
  separate_rows(LastKeywordXMP, sep = ", ") %>%
  mutate(yearmon = zoo::as.yearmon(DateTimeOriginal)) %>%
  filter(grepl("_untagged_location|_untagged_people|_untagged_rating",LastKeywordXMP) & yearmon >= "2008-01-01" ) %>%
  group_by(yearmon, LastKeywordXMP) %>%
  summarise(n())

names(images_tidy) <- c("DateTimeOriginal", "LastKeywordXMP", "fqcy")

p <- ggplot(images_tidy, aes(DateTimeOriginal,log(fqcy), colour = LastKeywordXMP)) +
  geom_line() +
  labs(x = "Date Photograph Taken", y = "Number of Photographs Taken") +
  scale_colour_brewer(palette = "Dark2")

ggsave("photos_by_date_and_tag.svg", device = "svg", width = 50, height = 40, units = "cm")


images_tidy_rated <- select(image_df, SourceFile, DateTimeOriginal, Rating, LastKeywordXMP) %>%
  separate_rows(LastKeywordXMP, sep = ", ") %>%
  mutate(yearmon = zoo::as.yearmon(DateTimeOriginal)) %>%
  filter( yearmon >= "2008-01-01" ) %>% #grepl("tmp: inc children",LastKeywordXMP) &
  group_by(yearmon, Rating, LastKeywordXMP) %>%
  summarise(n()) %>%
  filter(Rating>0)


names(images_tidy_rated) <- c("DateTimeOriginal", "Rating", "LastKeywordXMP", "fqcy")

q <- ggplot(images_tidy_rated, aes(DateTimeOriginal,fqcy)) + 
  geom_line() + 
  facet_wrap(~ Rating, ncol = 3) + 
  labs(x = "Date Photograph Taken", y = "Number of Photographs Taken") +
  scale_colour_brewer(palette = "Dark2") + 
  geom_smooth(method = "loess")
# ggsave(filename = q.png)
ggsave("photos_by_date_and_rating.svg", device = "svg", width = 50, height = 40, units = "cm")

top_cameras_by_photos <- image_df %>% group_by(Model) %>% summarise(n = n()) %>% top_n(13) %>% select(Model) %>% unlist()

images_tidy_model <- select(image_df, SourceFile, DateTimeOriginal, Rating, Model) %>%
  mutate(Year = lubridate::year(DateTimeOriginal)) %>%
  select(-DateTimeOriginal) %>%
  filter( Year >= 2008) %>%
  group_by(Year, Rating, Model) %>%
  summarise(n()) %>%
  filter(Rating>0 & Model %in% top_cameras_by_photos)
  

names(images_tidy_model) <- c("Year", "Rating", "Model", "fqcy")

r <- ggplot(images_tidy_model, aes(Year,(fqcy), fill = Rating)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Model, ncol = 6) +
  labs(x = "Date Photograph Taken", y = "Number of Photographs Taken")
# ggsave(filename = r.png)
ggsave("photos_by_date_and_rating_and_camera.svg", device = "svg", width = 50, height = 40, units = "cm")


images_tidy_hist <- select(image_df,  SourceFile, DateTimeOriginal, LastKeywordXMP) %>%
  separate_rows(LastKeywordXMP, sep = ", ") %>%
  mutate(yearmon = zoo::as.yearmon(DateTimeOriginal)) %>%
  filter(grepl("_untagged_location|_untagged_people|_untagged_rating",LastKeywordXMP) & yearmon >= "2008-01-01" ) %>%
  group_by(yearmon, LastKeywordXMP)

s <- ggplot(images_tidy_hist, aes(zoo::as.Date(yearmon), fill = LastKeywordXMP)) +
  geom_histogram(bins = length(unique(images_tidy_hist$yearmon))) +
  labs(x = "Date Photograph Taken", y = "Number of Photographs Taken") +
  scale_colour_brewer(palette = "Dark2")
# ggsave(filename = s.png)

## show any photos which are unrated but don't have an _untagged_rating tag
# as.data.frame(image_df[!grepl("_untagged_rating",image_df$LastKeywordXMP) & image_df$Rating %in% !1:5,])


## number of untagged per day

# untagged_by_day <- image_df[grepl(x = image_df$LastKeywordXMP, pattern = "_untagged_rating"),] %>%
#   mutate(mthday = substr(DateTimeOriginal,6,10)) %>%
#   group_by(mthday) %>%
#   summarise(n = n())

unrated_by_day <- image_df[!image_df$Rating >= 1,] %>%
  mutate(mthday = substr(DateTimeOriginal,6,10)) %>%
  group_by(mthday) %>%
  summarise(n = n())

# untagged_by_month <- image_df[grepl(x = image_df$LastKeywordXMP, pattern = "_untagged_rating"),] %>%
#   mutate(mth = substr(DateTimeOriginal,6,7)) %>%
#   group_by(mth) %>%
#   summarise(n = n())

unrated_by_month <- image_df[image_df$Rating == 0 | is.na(image_df$Rating),] %>%
  mutate(mth = substr(DateTimeOriginal,6,7)) %>%
  group_by(mth) %>%
  summarise(n = n())

dated_and_rated_by_month <- image_df[grepl("Date/", image_df$LastKeywordXMP), ] %>%
  mutate(thedate = factor(str_extract(LastKeywordXMP, pattern = "Date/[A-z]+"), levels = paste0("Date/", month.name))) %>%
  mutate(rated = str_extract(LastKeywordXMP, pattern = "_untagged_rating|_tagged_rating")) %>%
  group_by(thedate, rated) %>%
  summarise(n = n()) %>%
  spread(rated, n) %>% as.data.frame()
names(dated_and_rated_by_month) <- c("Month", "Rated", "Unrated", "NA")
