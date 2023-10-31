# Install packages
install.packages("readxl")
install.packages("writexl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("car")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")

#Load libraries
library("readxl")
library("writexl")
library("tidyr")
library("dplyr")
library("car")
library("readr")
library("ggplot2")
library("gridExtra")


# Set working directory
setwd("~/Core R Code/Local")


# --------------------------- Data Import --------------------------------------
# Load and view raw data for participation
raw_part <- read_excel("raw_merged_hhparticipation.xlsx")
# Load and view raw data for wealth
raw_wealth <- read_excel("raw_merged_hhwealth.xlsx")
# Load and view country codes
country_codes <- read_excel("country_codes.xlsx")

# Convert both to data frame
raw_part_df <- data.frame(raw_part)
raw_wealth_df <- data.frame(raw_wealth)
country_codes_df <- data.frame(country_codes)

# -------------------------- Data Cleaning -------------------------------------

# Remove header rows
raw_part_df <- filter(raw_part_df, rowSums(is.na(raw_part_df[-1])) != ncol(raw_part_df[-1]))
raw_wealth_df <- filter(raw_wealth_df, rowSums(is.na(raw_wealth_df[-1])) != ncol(raw_wealth_df[-1]))

# Take transpose, nullify row names and add country column as a variable
part_df = setNames(data.frame(t(raw_part_df[,-1])), raw_part_df[,1])
part_df <- part_df %>% mutate(country=rownames(part_df), .before="Financial assets")
rownames(part_df) <- NULL

wealth_df = setNames(data.frame(t(raw_wealth_df[,-1])), raw_wealth_df[,1])
wealth_df <- wealth_df %>% mutate(country=rownames(wealth_df), .before="Mean household assets (thousands of US dollars)")
rownames(wealth_df) <- NULL

# Now issue is inconsistent values of observations of each variable -- some are 
# floats and some are percentages.
# Combine mutate() and across() from dplyr:: and parse_number() from readr:: to 
# pick out the numbers for each data set

part_df <- part_df %>%
  mutate(across(!country, ~ if_else(grepl("%", part_df[[cur_column()]], fixed=TRUE), 
                                    parse_number(part_df[[cur_column()]])/100, 
                                    parse_number(part_df[[cur_column()]]))))


wealth_df <- wealth_df %>%
  mutate(across(!country, ~ if_else(grepl("%", wealth_df[[cur_column()]], fixed=TRUE), 
                                    parse_number(wealth_df[[cur_column()]])/100, 
                                    parse_number(wealth_df[[cur_column()]]))))

# Note that this preserves NAs as needed

# Replace "." in country name with space " "
part_df$country <- chartr('.', ' ', part_df$country)
wealth_df$country <- chartr('.', ' ', wealth_df$country)

# Now merge with country codes
part_df <- merge(part_df,country_codes,by='country') %>% 
  relocate(code, .after='country')
wealth_df <- merge(wealth_df,country_codes,by='country') %>%
  relocate(code, .after='country')

# ------------------------- Export Data to XLSX --------------------------------

# If needed as Excel files
write_xlsx(part_df, "~/CORE R Code/clean_merged_hhparticipation.xlsx")
write_xlsx(wealth_df, "~/CORE R Code/clean_merged_hhwealth.xlsx")

# ------------------------- First Scatter Plots --------------------------------

part_df <- part_df %>% 
  rename(gdppc = "GDPPC PPP")

wealth_df <- wealth_df %>% 
  rename(gdppc = "GDPPC PPP")

# Plot function
colnames_part <- names(part_df[,3:(ncol(part_df)-1)])
colnames_wealth <- names(wealth_df[,3:(ncol(wealth_df)-1)])

plot_data_column = function (data, column) {
  ggplot(data, aes(x=gdppc, y=.data[[column]])) +
    geom_point(alpha=0) +
    geom_smooth(method = "lm", se = FALSE, linewidth=0.5) +
    ggtitle(paste(as.character(column))) +
    labs(x="GDP per capita (PPP)", y="") +
    geom_text(hjust=0.5, vjust=0.5, aes(label=code), size=2) +
    theme(plot.title=element_text(size=8), axis.title.x=element_text(size=8), ) 
}
# Plot participation
plots_list <- lapply(colnames_part, plot_data_column, data = part_df)
do.call("grid.arrange", c(plots_list, ncol=5))

# Plot wealth
plots_list <- lapply(colnames_wealth, plot_data_column, data = wealth_df)
do.call("grid.arrange", c(plots_list, ncol=5))


# --------------------- Conditional Participation Rates -----------------------

