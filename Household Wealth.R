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
setwd("~/Core R Code")

# --------------------------- Data Import --------------------------------------
# Load and view excel file
raw_data <- read_excel("~/Core R Code/household_wealth_and_gdppc.xlsx")
View(raw_data)

# Convert to data frame
mode(raw_data)
raw_df <- data.frame(raw_data)
View(raw_df)

# -------------------------- Data Cleaning -------------------------------------

# Remove header rows
raw_df <- filter(raw_df, rowSums(is.na(raw_df[-1])) != ncol(raw_df[-1]))

# Take transpose, nullify rownames and add country column as a variable
df = setNames(data.frame(t(raw_df[,-1])), raw_df[,1])
View(df)
df <- df %>% mutate(country=rownames(df), .before="Mean household assets (thousands of US dollars)")
rownames(df) <- NULL

# Rename all variables
# df <- df %>% # Assets
#   rename(mean_ast = "Mean household assets (thousands of US dollars)",
#          med_ast = "Median household assets (thousands of US dollars)",
#          fin_ast = "Financial assets",
#         deposits = "Deposits",
#         ret_ast = "Retirement assets and life insurance",
#         dir_stock = "Directly held stocks",
#         mutual_funds = "Mutual funds",
#         bonds = "Bonds",
#         other_ast = "Other assets",
#         nonfin_ast = "Nonfinancial assets",
#         veh_val_other_ast = "Vehicles, valuables, and other assets",
#         main_res = "Main residence",
#         other_realest = "Other real estate",
#         priv_biz = "Private businesses",
#         Liabilities
#         mean_hh_lib = "Mean household liabilities (thousands of US dollars)",
#         med_hh_lib = "Median of positive household liabilities (thousands of US dollars)",
#         veh_stu_other_debt = "Vehicle loans, student loans, and other debt",
#         credit_cards = "Credit cards",
#         od_creditlines = "Overdrafts and credit lines",
#         prim_mort = "Mortgage debt for primary residence",
#         other_sec_realest = "Other debt secured with real estate",
#         mean_net_worth = "Mean household net worth (thousands of US dollars)",
#         med_net_worth = "Median household net worth (thousands US dollars)",
#         #GDPPC PPP
#         gdppc = "GDPPC PPP")

df <- df %>%
  rename(gdppc = "GDPPC PPP")
View(df)

# Now issue is inconsistent values of observations of each variable -- some are 
# floats and some are percentages
# Combine mutate and across from dplyr and parse_number from readr to pick out the numbers

df <- df %>%
  mutate(across(!country, ~ if_else(grepl("%", df[[cur_column()]], fixed=TRUE), 
                                    parse_number(df[[cur_column()]])/100, 
                                    parse_number(df[[cur_column()]]))))

# Note that this preserves NAs as needed

# Export to excel if needed
write_xlsx(df, "~/CORE R Code/wealthclean.xlsx")

# Now generate plots
colnames <- names(df[,2:(ncol(df)-1)])

plot_data_column = function (data, column) {
  ggplot(data, aes(x=gdppc, y=.data[[column]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, linewidth=0.5) +
    ggtitle(paste(as.character(column))) +
    labs(x="GDP per capita (PPP)", y="") +
    theme(plot.title=element_text(size=8), axis.title.x=element_text(size=8), axis.text.x=element_text(size=6)) 
}
plots_list <- lapply(colnames, plot_data_column, data = df)
do.call("grid.arrange", c(plots_list, ncol=5))

# test
