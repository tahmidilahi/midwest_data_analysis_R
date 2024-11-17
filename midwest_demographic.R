### Big Data 2: Final Project ###

#Summary statistics of 'midwest' dataframe
summary(midwest)

#Frequency for 'state' variable
table(midwest$state)

## Visual 1: Total Population by State ------------------------------------------

# Calculate the total population for each state
state_population <- aggregate(poptotal ~ state, data = midwest, FUN = sum)

# Convert population to millions for easier readability
state_population$poptotal_millions <- state_population$poptotal / 1e6

# Create the bar chart with data labels
ggplot(state_population, aes(x = state, y = poptotal_millions, fill = state)) +
  geom_bar(stat = "identity") + # Use bars to represent total population
  geom_text(aes(label = sprintf("%.2fM", poptotal_millions)), # Format labels to two decimal places
            position = position_stack(vjust = 0.5), # Position labels in the middle of the bars
            color = "white", # Set text color to white for better readability
            size = 3.5) + # Set text size
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1)) +
  theme_minimal() +
  labs(x = "State", y = "Total Population (Millions)", title = "Total Population by State") +
  theme(legend.position = "none") # Remove the legend

## Visual 2: Population Density Distribution -----------------------------------

# Calculate percentage population density by state
state_popdensity <- midwest %>%
  group_by(state) %>%
  summarise(percentage_density = sum(popdensity) / sum(midwest$popdensity) * 100)

# Create pie chart
ggplot(state_popdensity, aes(x = "", y = percentage_density, fill = state, label = paste0(round(percentage_density, 2), "%"))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(title = "Population Density Distribution by State",
       fill = "State") +
  theme_void() +
  theme(legend.position = "bottom")

## Visual 3:Population Density Percentage by State (Top 5 Counties)--------------

# Calculate the total population density for each state
state_density_totals <- midwest %>%
  group_by(state) %>%
  summarise(total_density = sum(popdensity))

#  Join the totals back to the original dataframe and calculate percentages
midwest <- midwest %>%
  left_join(state_density_totals, by = "state") %>%
  mutate(density_percentage = (popdensity / total_density) * 100)

#  Select the top 5 counties by population density percentage in each state
top_counties <- midwest %>%
  group_by(state) %>%
  top_n(5, density_percentage)

#  Create the grouped bar plot
ggplot(top_counties, aes(x = reorder(county, density_percentage), y = density_percentage, fill = state)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state, scales = "free_x") +
  theme_minimal() +
  labs(x = "County", y = "Population Density Percentage", title = "Top 5 Counties by Population Density Percentage in Each State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "none")

# Visual 4:American Indian, Asian and Others by State (in Thousands)------------

# Summarize the total population of each ethnicity by state and convert to thousands
ethnic_totals <- midwest %>%
  group_by(state) %>%
  summarise(
    american_indian = sum(popamerindian)/1e3,
    asian = sum(popasian)/1e3,
    other = sum(popother)/1e3
  ) %>%
  pivot_longer(cols = american_indian:other, names_to = "ethnicity", values_to = "population_millions")

# Create the bar chart with population in thousands and add data labels
ggplot(ethnic_totals, aes(x = state, y = population_millions, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(population_millions, 2)), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("american_indian" = "red", "asian" = "blue", "other" = "green")) +
  theme_minimal() +
  labs(x = "State", y = "Population (Thousands)", title = "American Indian, Asian and Others by State (in Thousands)") +
  theme(legend.position = "bottom")

# Visual 5: White and Black Population by State (in Millions)-------------------

# Summarize the total population of white and black race by state
ethnic_totals <- midwest %>%
  group_by(state) %>%
  summarise(
    total_white = sum(popwhite)/1e6,
    total_black = sum(popblack)/1e6
  ) %>%
  pivot_longer(cols = total_white:total_black, names_to = "ethnicity", values_to = "population")

# Create the bar chart
ggplot(ethnic_totals, aes(x = state, y = population, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(population, 2)), vjust = -0.3, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("total_white" = "orange", "total_black" = "blue")) +
  theme_minimal() +
  labs(x = "State", y = "Population (in Millions)", title = "White and Black Population by State (in Millions)") +
  theme(legend.position = "bottom")

# Visual 6: Total Percent of Adults by State------------------------------------

state_adults_percentage <- midwest %>%
  group_by(state) %>%
  summarise(total_adults = sum(popadults),
            total_population = sum(poptotal),
            percent_adults = (total_adults / total_population) * 100)

# Create the pie chart with percentage labels
ggplot(state_adults_percentage, aes(x = "", y = percent_adults, fill = state)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%.1f%%", percent_adults)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3) +
  theme_void() +
  labs(fill = "State",
       title = "Total Percent of Adults by State")

# Visual 7: Boxplot of Adult Poverty Percentage by State----------------------- 

ggplot(midwest, mapping = aes(x = state, y = percadultpoverty))+
  geom_boxplot()

# Visual 8: Counties with Child Poverty(%) Above 35 ---------------------------

# Filter the data
filtered_data <- midwest %>%
  filter(percchildbelowpovert > 35)

# Create the horizontal bar plot
ggplot(filtered_data, aes(x = percchildbelowpovert, y = reorder(county, percchildbelowpovert), fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Counties with Child Poverty > 35%", x = "Percentage of Child Poverty", y = "County") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = guide_legend(title = "State"))

# Visual 10: Poverty Percentage vs Percentage with High School Diploma----------

# Selecting relevant columns
plot_data <- midwest[, c("percbelowpoverty", "perchsd", "state")]

# Creating a vector of unique colors for each state
state_colors <- rainbow(length(unique(plot_data$state)))

# Creating the scatter plot with facet wrap and custom colors for each state
ggplot(plot_data, aes(x = perchsd, y = percbelowpoverty, color = state)) +
  geom_point(alpha = 0.7) +
  labs(title = "Poverty Percentage vs. Percentage with High School Diploma",
       x = "Percentage with High School Diploma",
       y = "Poverty Percentage") +
  facet_wrap(~state, nrow = 1) +  # Split into facets by state
  scale_color_manual(values = state_colors) +  # Assign custom colors to each state
  theme_minimal()

# Visual 11: College vs Professional Degree-------------------------------------

ggplot(midwest, mapping = aes(x = percollege, y = percprof, color = state))+
  geom_point()+
  geom_smooth()+
  ggtitle("College/Professional Work Scatter Plot")

# Visual 12: Metro vs Non-metro by State----------------------------------------

# Count total counties under each state having metro or not
urbanization <- midwest %>%
  group_by(state, inmetro) %>%
  summarise(total_counties = n()) %>%
  mutate(urbanization_status = ifelse(inmetro == 1, "Metro", "Non-Metro"))

# Plot the urbanization data with side-by-side bars and data labels
ggplot(urbanization, aes(x = state, y = total_counties, fill = urbanization_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = total_counties), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size = 3) +  # Add data labels
  labs(title = "Urbanization of States by County Count",
       x = "State",
       y = "Total Counties") +
  scale_fill_manual(values = c("Metro" = "blue", "Non-Metro" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))



