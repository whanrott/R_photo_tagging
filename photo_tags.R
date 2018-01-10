
library(dplyr)
library(ggplot2)
library(tidyr)

source("run_exiftool.R")
run_exiftool()

## define new date type. This is nice, but I don't actually need it here.
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%Y:%m:%d %H:%M:%S") )

# read in data
image_df <- read.csv("photos.txt", header = TRUE, fill = TRUE, skipNul = TRUE)

# Tag field contains irregular number of values. Reshape the data frame by breaking the tag field
# apart, then return 3 relevant columns
images_tidy <- separate_rows(image_df, LastKeywordXMP, sep = ", ")#[, c(1,5,16)]
images_tidy <- images_tidy[, c("SourceFile","DateTimeOriginal", "LastKeywordXMP")]

# filter result to include only relevant tags
images_tidy                  <- images_tidy[grep("_untagged_location|_untagged_people|_untagged_rating",images_tidy$LastKeywordXMP),]

# fix date and round it to nearest month
images_tidy$DateTimeOriginal <- zoo::as.Date(zoo::as.yearqtr(images_tidy$DateTimeOriginal,format = "%Y:%m:%d %H:%M:%S"),frac = 0)

# create frequency table summarising tags by month
images_summary               <- data.frame(table(images_tidy$DateTimeOriginal,images_tidy$LastKeywordXMP))
names(images_summary) <- c("DateTimeOriginal", "LastKeywordXMP", "fqcy")

# images_summary$DateTimeOriginal <- as.Date(images_summary$DateTimeOriginal) #,format = "%Y:%m:%d %H:%M:%S")

# tidy up tag text (In some of the tags I've used an hierarchical naming convention for the tags)
images_summary$LastKeywordXMP   <- sub(pattern = "Person/Person: ",x = images_summary$LastKeywordXMP, replacement = "")
images_summary$LastKeywordXMP   <- sub(pattern = "Person: ",x = images_summary$LastKeywordXMP, replacement = "")

# fix starting date
images_summary$DateTimeOriginal <- as.Date(images_summary$DateTimeOriginal) #,format = "%Y:%m:%d %H:%M:%S")
images_summary <- images_summary[images_summary$DateTimeOriginal >= "2008-01-01",]

# plot graph
p <- ggplot(images_summary, aes(DateTimeOriginal,log(fqcy), colour = LastKeywordXMP)) + geom_line() #+ facet_wrap(~LastKeywordXMP, ncol = 1)
print(p + labs(x = "Date Photograph Taken", y = "Number of Photographs Taken") + scale_colour_brewer(palette = "Dark2"))
