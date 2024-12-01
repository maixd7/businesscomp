# Setting up ----
library(tidyverse)
library(geomtextpath)
library(plotly)
library(htmlwidgets)
library(maps)
library(dplyr)
df <- read.csv("SBAnational.csv")
## filter out missing values, limit data rangeto 2010-1999 ----
naics_mapping <- list(
  "11" = "Agriculture, Forestry, Fishing and Hunting",
  "21" = "Mining, Quarrying, and Oil and Gas Extraction",
  "22" = "Utilities",
  "23" = "Construction",
  "31" = "Manufacturing",
  "32" = "Manufacturing",
  "33" = "Manufacturing",
  "42" = "Wholesale Trade",
  "44" = "Retail Trade",
  "45" = "Retail Trade",
  "48" = "Transportation and Warehousing",
  "49" = "Transportation and Warehousing",
  "51" = "Information",
  "52" = "Finance and Insurance",
  "53" = "Real Estate and Rental and Leasing",
  "54" = "Professional, Scientific, and Technical Services",
  "55" = "Management of Companies and Enterprises",
  "56" = "Administrative and Support Services",
  "61" = "Educational Services",
  "62" = "Health Care and Social Assistance",
  "71" = "Arts, Entertainment, and Recreation",
  "72" = "Accommodation and Food Services",
  "81" = "Other Services",
  "92" = "Public Administration"
)
df <- df %>%
  filter(df$MIS_Status != "", df$GrAppv != "", df$SBA_Appv != "",
         df$NAICS != 0, df$State != "", df$NoEmp <= 1500,
         df$LowDoc == "Y" | df$LowDoc == "N", df$Term > 0,
         df$ApprovalFY < 2011 & df$ApprovalFY > 1999, df$NewExist != 0,
         df$RevLineCr == "Y" | df$RevLineCr == "N", df$UrbanRural > 0)

df$GrAppv <- as.numeric(gsub("[$,]", "", df$GrAppv))
df$SBA_Appv <- as.numeric(gsub("[$,]", "", df$SBA_Appv))
df$DisbursementGross <- as.numeric(gsub("[$,]", "", df$DisbursementGross))
df$ApprovalFY <- as.numeric(df$ApprovalFY)
## Convert variables as needed ----
df <- df %>%
  mutate(RealEstate = ifelse(df$Term >= 240, 1, 0)) %>%
  mutate(MIS_Status = ifelse(df$MIS_Status == "CHGOFF", 1, 0)) %>%
  mutate(FranchiseCode = ifelse(df$FranchiseCode > 1, 1, 0)) %>%
  mutate(UrbanRural = ifelse(df$UrbanRural == 1, 1, 0)) %>%
  mutate(NewExist = ifelse(df$NewExist == 1, 1, 0)) %>%
  mutate(RevLineCr = ifelse(df$RevLineCr == "Y", 1, 0)) %>%
  mutate(LowDoc = ifelse(df$LowDoc == "Y", 1, 0)) %>%
  mutate(NAICS = as.integer(NAICS / 10000)) %>%
  mutate(Industry = as.character(naics_mapping[as.character(NAICS)])) %>%
  mutate(Proportion = SBA_Appv / GrAppv) %>%
  mutate(Recession = ifelse(df$ApprovalFY > 2007 & df$ApprovalFY <= 2009,
                            1, 0))
## select desired values ----
df <- df %>%
  select(State, Industry, FranchiseCode, UrbanRural,
         Term, NoEmp, NewExist, RevLineCr, Recession,
         RealEstate, DisbursementGross, LowDoc, NoEmp, Proportion, MIS_Status)

## Recession anaylsis ----
recession <- df %>%
  select(MIS_Status, Recession)
chisq.test(table(recession))

recession_summary <- recession %>%
  group_by(Recession, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Recession) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

recession_plot <- ggplotly(
  ggplot(recession_summary, aes(
    x = factor(Recession),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Loan Occured during the Recession: ", ifelse(Recession == 1, "Yes",
                                                    "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "Loan Occured during the Recession vs Default Rate",
      x = "Loan Occured during the Recession",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

recession_plot

## filter out data that occured in the recession ----
df <- df %>%
  filter(df$Recession != 1)
df <- df %>% select(-Recession)

## get base default rate ----
percentage_defaulted <- df %>%
  summarise(
    total_count = n(),
    defaulted_count = sum(MIS_Status == 1),
    percentage_defaulted = (defaulted_count / total_count) * 100
  )
percentage_defaulted

# Categorical Data (chisq test and barcharts) ----
## state anaylsis ----
state_mapping <- setNames(state.name, state.abb)
state_test <- df %>%
  select(State, MIS_Status)
chisq.test(table(state_test))
state <- df %>%
  mutate(StateName = tolower(state_mapping[State])) %>%
  group_by(StateName) %>%
  summarise(default_rate = mean(MIS_Status == 1))
map <- map_data("state")
map <- left_join(map, state, by = c("region" = "StateName"))
state_plot <- ggplotly(
  ggplot(map, aes(x = long, y = lat, group = group, fill = default_rate,
    text = paste(
      "State: ", tools::toTitleCase(region),
      "<br>Default Rate: ", scales::percent(default_rate, accuracy = 0.01)
    )
  )) +
    geom_polygon(color = "white") +
    scale_fill_gradient(low = "lightblue", high = "red",
                        labels = scales::percent) +
    labs(
      title = "Loan Default Rate by State",
      fill = "Default Rate (%)"
    ),
  tooltip = "text"
)
state_plot

## industry anylsis ----
industry <- df %>%
  select(MIS_Status, Industry)
chisq.test(table(industry))

industry_summary <- industry %>%
  group_by(Industry, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Industry) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

industry_summary2 <- industry %>%
  group_by(Industry) %>%
  summarise(default_rate = mean(MIS_Status == 1))

industry_plot <- ggplotly(
  ggplot(industry_summary, aes(
    x = factor(Industry),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Industry: ", Industry,
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Industry vs Default Rate",
      x = "Industry",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)
industry_plot

## Real Estate analysis----
real_estate <- df %>%
  select(MIS_Status, RealEstate)
chisq.test(table(real_estate))

real_estate_summary <- real_estate %>%
  group_by(RealEstate, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RealEstate) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

real_estate_plot <- ggplotly(
  ggplot(real_estate_summary, aes(
    x = factor(RealEstate),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Backed by Real Estate: ", ifelse(RealEstate == 1, "Yes", "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "Real Estate vs Default Rate",
      x = "Backed By Real Estate",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

real_estate_plot

## Is a Franchise ----
franchise <- df %>%
  select(MIS_Status, FranchiseCode)
chisq.test(table(franchise))

franchise_summary <- franchise %>%
  group_by(FranchiseCode, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(FranchiseCode) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

franchise_plot <- ggplotly(
  ggplot(franchise_summary, aes(
    x = factor(FranchiseCode),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Is Franchise: ", ifelse(FranchiseCode == 1, "Yes", "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "Is Franchise vs Default Rate",
      x = "Is Franchise",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

franchise_plot

## New vs Exisitng ----
new <- df %>%
  select(MIS_Status, NewExist)
chisq.test(table(new))

new_summary <- new %>%
  group_by(NewExist, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(NewExist) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

new_plot <- ggplotly(
  ggplot(new_summary, aes(
    x = factor(NewExist),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Existed for more than 2 years: ", ifelse(NewExist == 1, "Yes", "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "Is Existing vs Default Rate",
      x = "Is Existing",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

new_plot

## Revolving line of credit ----
credit <- df %>%
  select(MIS_Status, RevLineCr)
chisq.test(table(credit))

credit_summary <- credit %>%
  group_by(RevLineCr, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(RevLineCr) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

credit_plot <- ggplotly(
  ggplot(credit_summary, aes(
    x = factor(RevLineCr),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Has Revolving Line of Credit: ", ifelse(RevLineCr == 1, "Yes", "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "Has Revolving Line of Credit vs Default Rate",
      x = "Has Revolving Line of Credit",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

credit_plot

## Urban vs Rural ----
urban <- df %>%
  select(MIS_Status, UrbanRural)
chisq.test(table(urban))

urban_summary <- urban %>%
  group_by(UrbanRural, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(UrbanRural) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

urban_plot <- ggplotly(
  ggplot(urban_summary, aes(
    x = factor(UrbanRural),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "Urban or Rural: ", ifelse(UrbanRural == 1, "Urban", "Rural"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "Rural", "1" = "Urban")) +
    labs(
      title = "Ubran/Rural vs Default Rate",
      x = "Ubran/Rural",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

urban_plot

##Lowdoc program ----
lowdoc <- df %>%
  select(MIS_Status, LowDoc)
chisq.test(table(lowdoc))

lowdoc_summary <- lowdoc %>%
  group_by(LowDoc, MIS_Status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(LowDoc) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

lowdoc_plot <- ggplotly(
  ggplot(lowdoc_summary, aes(
    x = factor(LowDoc),
    y = percentage,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
    text = paste(
      "In LowDoc program: ", ifelse(LowDoc == 1, "Yes", "No"),
      "<br>Default Status: ", ifelse(MIS_Status == 1, "Defaulted",
                                     "Paid In Full"),
      "<br>Percentage: ", scales::percent(percentage, accuracy = 0.01)
    )
  )) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.75) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
    labs(
      title = "In LowDoc Program vs Default Rate",
      x = "In LowDoc Programt",
      y = "Percentage",
      fill = "Default Status"
    ),
  tooltip = "text"
)

lowdoc_plot
# Quantitative Data (t-test and various graphs)----

## SBA's guaranteed proportion ----
sba_prop <- df %>%
  select(MIS_Status, Proportion)
t.test(Proportion ~ MIS_Status, data = sba_prop, alternative = "two.sided")
sba_prop_plot <- ggplotly(
  ggplot(sba_prop, aes(
    x = Proportion,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
                   text = paste(
                     "Term: ", after_stat(xmin), " to ", after_stat(xmax),
                     "<br>Count: ", after_stat(count)
                   ))) +
    geom_histogram(
                   position = "dodge",
                   binwidth = 0.25,
                   boundary = 0,
                   alpha = 0.7) +
    xlim(0, max(sba_prop$Proportion)) +
    facet_wrap(~ MIS_Status,
               scales = "free_y",
               labeller = as_labeller(c("0" = "Paid In Full",
                                        "1" = "Defaulted"))) +
    labs(
      title = "Histogram of loan portion gauranteed by MIS Status",
      x = "Porportion of loan gauranteed by SBA",
      y = "Count",
      fill = "Default Status"
    ),
  tooltip = "text"
)
sba_prop_plot

## Gross Disbursement ----
disbur <- df %>%
  mutate(
    Quartile = cut(
      DisbursementGross,
      breaks = quantile(DisbursementGross, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("0-25%", "25-50%", "50-75%", "75-100%")
    )
  ) %>%
  select(MIS_Status, DisbursementGross, Quartile)
t.test(DisbursementGross ~ MIS_Status, data = disbur,
       alternative = "two.sided")
disbur_prop <- disbur %>%
  group_by(MIS_Status, Quartile) %>%
  summarise(Count = n()) %>%
  group_by(MIS_Status) %>%
  mutate(Proportion = Count / sum(Count))
disbur_plot <- ggplotly(
  ggplot(disbur_prop, aes(x = Quartile, y = Proportion, fill = Quartile,
   text = paste(
    "Quartile: ", Quartile,
    "<br>Percentage: ", scales::percent(Proportion, accuracy = 0.01)
  ))) +
    geom_bar(stat = "identity", alpha = 0.75) +
    facet_wrap(~ MIS_Status, labeller =
                 as_labeller(c(`0` = "Paid In Full", `1` = "Default"))) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Gross Disbursement Distribution by Quartiles",
      x = "Gross Disbursement Quartile",
      y = "Proportion",
      fill = "Quartile"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ),
  tooltip = "text"
)
disbur_plot

## Number of employees ----
emp <- df %>%
  mutate(
    Quartile = cut(
      NoEmp,
      breaks = quantile(NoEmp, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("0-25%", "25-50%", "50-75%", "75-100%")
    )
  ) %>%
  select(MIS_Status, NoEmp, Quartile)
t.test(NoEmp ~ MIS_Status, data = emp,
       alternative = "two.sided")
emp_prop <- emp %>%
  group_by(MIS_Status, Quartile) %>%
  summarise(Count = n()) %>%
  group_by(MIS_Status) %>%
  mutate(Proportion = Count / sum(Count))
emp_plot <- ggplotly(
  ggplot(emp_prop, aes(x = Quartile, y = Proportion, fill = Quartile,
  text = paste (
    "Quartile: ", Quartile,
    "<br>Percentage: ", scales::percent(Proportion, accuracy = 0.01)
  ))) +
    geom_bar(stat = "identity", alpha = 0.75) +
    facet_wrap(~ MIS_Status, labeller =
                 as_labeller(c(`0` = "Paid In Full", `1` = "Default"))) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Number of employee Distribution by Quartiles",
      x = "Disbursement Quartile",
      y = "Proportion",
      fill = "Quartile"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ),
  tooltip = "text"
)
emp_plot

## Term Length ----
term <- df %>%
  select(MIS_Status, Term)
t.test(Term ~ MIS_Status, data = term, alternative = "two.sided")
#balance the graph a little
term <- term %>%
  filter(Term <= 300)

term_plot <- ggplotly(
  ggplot(term, aes(
    x = Term,
    fill = factor(MIS_Status, labels = c("Paid In Full", "Defaulted")),
                   text = paste(
                     "Term: ", after_stat(xmin), " to ", after_stat(xmax),
                     "<br>Count: ", after_stat(count)
                   ))) +
    geom_histogram(
                   position = "dodge",
                   binwidth = 25,
                   boundary = 0,
                   alpha = 0.7) +
    xlim(0, max(term$Term)) +
    facet_wrap(~ MIS_Status,
               scales = "free_y",
               labeller = as_labeller(c("0" = "Paid In Full",
                                        "1" = "Defaulted"))) +
    labs(
      title = "Histogram of Term by MIS Status",
      x = "Term",
      y = "Count",
      fill = "Default Status"
    ),
  tooltip = "text"
)
term_plot


# Exporting data ----
df <- df %>%
  select(State, Industry, FranchiseCode, UrbanRural,
         Term, NoEmp, RealEstate, DisbursementGross,
         LowDoc, Proportion, MIS_Status)
write.csv(df, "output.csv", row.names = FALSE)
write.csv(state, "state.csv", row.names = FALSE)
write.csv(industry_summary2, "industry.csv", row.names = FALSE)
saveWidget(state_plot, file = "report/graphs/state.html")
saveWidget(industry_plot, file = "report/graphs/industry.html")
saveWidget(real_estate_plot, file = "report/graphs/realestate.html")
saveWidget(franchise_plot, file = "report/graphs/franchise.html")
saveWidget(new_plot, file = "report/graphs/new.html")
saveWidget(credit_plot, file = "report/graphs/credit.html")
saveWidget(urban_plot, file = "report/graphs/urban.html")
saveWidget(lowdoc_plot, file = "report/graphs/lowdoc.html")
saveWidget(recession_plot, file = "report/graphs/recession.html")
saveWidget(sba_prop_plot, file = "report/graphs/sbaproportion.html")
saveWidget(disbur_plot, file = "report/graphs/disbursement.html")
saveWidget(term_plot, file = "report/graphs/term.html")
saveWidget(emp_plot, file = "report/graphs/emp.html")