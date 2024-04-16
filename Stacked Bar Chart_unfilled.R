library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Load the dataset
New <- read_excel("Location/File_name.xlsx")

###Organizing the data in categories for the bar chart####
#Key
#column of interest=the numeric value you are interested in for this chart#
#group of interest= groups that can be stacked with each other (ie. cognitive diagnosis-normal vs. dementia at last visit)
# Filter missing codes (B & E)#
File_name <- File_name %>%
  group_by(id) %>%
  filter(!(column_of_interest %in% c("B", "E"))) %>% 
  ungroup()

#ensure the column of interest is numeric#
File_name$column_of_interest <- as.numeric(File_name$column_of_interest)

#filter to include participants with longitudinal visits#
File_filtered  <- File_name %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  ungroup()

# Filter and categorize the column of interest values
File_filtered <- File_name  %>%
  filter(!is.na(column_of_interest)) %>%
  mutate(column_of_interest = cut(column_of_interest,
                    breaks = c(-Inf, 0.01, 40, 80, 160, 320, Inf),
                    labels = c("0", "40", "80", "160", "320", "640"),
                    right = FALSE))

# Ensure column_of_interest is ordered with "0" first for plotting
File_filtered$column_of_interest <- factor(File_filtered$column_of_interest, levels = c("0", "40", "80", "160", "320", "640"))

# Selecting the first time point (ie. age) per identifier#
first_timepoint <- File_filtered %>%
  group_by(identifier) %>%
  slice(which.min(age)) %>%
  ungroup()

# Selecting the last time point (ie. age) per identifier# 
last_timepoint <- File_filtered %>%
  group_by(identifier) %>%
  slice(which.max(age)) %>%
  ungroup()

# Count colum of interest levels (ie. IL-1 levels) for the first time point within each group#
visit_counts <- first_timepoint %>%
  count(group_of_interest, column_of_interest)

# Calculate proportions for the stacked bar chart
visit_proportions <- visit_counts %>%
  group_by(group_of_interest) %>%
  mutate(Total = sum(n)) %>%
  ungroup() %>%
  mutate(Proportion = n / Total)

###Stacked Bar Chart####
# Create a green color palette, the number of colors corresponds with the number of categories#
green_palette <- colorRampPalette(c("#e6f4ea", "#a9dba9", "#1a9850"))

# Calculate the number of unique column_of_interest categories
num_colors <- length(unique(visit_proportions$column_of_interest))

# Merge the participant counts with the visit_proportions data
visit_proportions_with_counts <- merge(visit_proportions, participants_per_group, by = "group_of_interest")

# Plotting the chart with participant counts annotated
ggplot(visit_proportions_with_counts, aes(x = column_of_interest, y = Proportion, fill = column_of_interest)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = green_palette(num_colors)) +
  geom_text(aes(label = paste0("n=", number_of_participants), y = max(Proportion)), 
            hjust = -.2,  # Adjust this to move the label further to the right
            size = 3.5, 
            color = "black") +
  labs(title = "Proportion of column_of_interest levels for by category (ie. cognitive diagnosis) at First Visit",
       x = "Category name",
       y = "Proportion (%)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = margin(5.5, 40, 5.5, 5.5)  # Adjust the right margin to make space for labels
  )
