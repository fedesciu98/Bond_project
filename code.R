library(readr)
library(BatchGetSymbols)
library(tidyr)

getQuarter <- function(months) {
  quarters <- ((months - 1) %/% 3) + 1
  quarters[quarters > 4] <- 4  # Adjust for December
  quarters
}

# Start with importing bond data 

bond_df <- read_csv("bond_data.csv")

bond_data <-  bond_df %>% select(DATE,T_DATE,company_symbol, BOND_TYPE, SECURITY_LEVEL,
                                 OFFERING_PRICE, TREASURY_MATURITY, COUPON, DAY_COUNT_BASIS,
                                 NCOUPS, AMOUNT_OUTSTANDING, R_SP, R_MR, R_FR,PRICE_EOM,
                                 COUPACC,TMT, REMCOUPS, DEFAULTED) %>% 
  mutate(YEAR = substr(DATE,1,4),
         QUARTER = getQuarter(as.numeric(substr(DATE,6,7)) ), .after = "DATE") %>% 
  filter(!is.na(company_symbol))


# Now get the risk free rate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# we will get the 5 years and 30 years US treasury rate 

tickers_rf <- c("^FVX","^TYX")

data_rf <- BatchGetSymbols(tickers_rf, 
                           first.date = "2001-01-01", 
                           last.date = Sys.Date(),
                           thresh.bad.data = 0.1)

table_rf <- data_rf$df.tickers %>% 
  select(price.close, ref.date, ticker) %>%
  pivot_wider(names_from = ticker, values_from = price.close) %>%
  rename("T_DATE" = "ref.date")


# join the tables now 

table_joint <- left_join(bond_data, table_rf, by = "T_DATE")


# get the company financial ratios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



fin_ratios <- read_csv("Financial_ratios.csv") %>% 
  select(-gvkey, -permno, -adate, -qdate, -cusip) %>%  
  rename("DATE" = "public_date") %>% mutate(YEAR = substr(DATE,1,4),
                                            QUARTER = getQuarter(as.numeric(substr(DATE,6,7)) ), .after = "DATE") %>%
  select(-DATE) %>% mutate(divyield = as.numeric(gsub("%","",divyield))/100 )

# join the tables 

table_joint <- table_joint %>% rename("TICKER" = "company_symbol") %>% left_join(fin_ratios, by = c("YEAR","QUARTER","TICKER"))




# get the financial industy data 

comp_insustry <- read.csv("company_industry.csv") %>% select(tic, fyearq, fqtr,
                                                             actq, lctq, gsector, ggroup, gind, gsubind) %>% 
  rename("YEAR" = fyearq,
         "QUARTER" = fqtr,
         "TICKER" = tic) %>% mutate(gsector = as.character(gsector),
                                    ggroup = as.character(ggroup),
                                    gind = as.character(gind),
                                    gsubind = as.character(gsubind),
                                    YEAR = as.character(YEAR))

# join results 

table_joint <- table_joint %>% left_join(comp_insustry, by = c("YEAR","QUARTER","TICKER"))

# filter for common tickers 

tic_filt <- list(unique(bond_data$company_symbol), unique(fin_ratios$TICKER), unique(comp_insustry$TICKER)) %>% Reduce("intersect",.)

table_joint <- table_joint %>% filter(TICKER %in% tic_filt)

