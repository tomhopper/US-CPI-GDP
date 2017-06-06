# citation: U.S. Bureau of Economic Analysis, “Table name,” [the URL] (accessed [date]).

#Table 1.1.6. Real Gross Domestic Product, Chained Dollars
# \url{https://www.bea.gov/iTable/iTableHtml.cfm?reqid=9&step=3&isuri=1&904=2015&903=6&906=a&905=2017&910=x&911=1}
#Table 1.1.5. Gross Domestic Product
# \url{https://www.bea.gov/iTable/iTableHtml.cfm?reqid=9&step=3&isuri=1&904=2015&903=5&906=a&905=2017&910=x&911=1}
#Table 1.1.1. Percent Change From Preceding Period in Real Gross Domestic Product
# \url{https://www.bea.gov/iTable/iTableHtml.cfm?reqid=9&step=3&isuri=1&904=2015&903=1&906=a&905=2017&910=x&911=1}
#2015 Digest of Education Statistics \url{https://nces.ed.gov/programs/digest/d15/}
#Table 106.10, total expenditures by year
# \url{https://nces.ed.gov/programs/digest/d15/tables/dt15_106.10.asp?referrer=report}
#

#library(rvest)
library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

####
# Uses self-built function in .env strip_na_rows()
####

table1_1_6 <- read_csv("data/table_1-1-6.csv", 
                       col_types = cols(`1929` = col_number(), `1930` = col_number(), 
                                        `1931` = col_number(), `1932` = col_number(), 
                                        `1933` = col_number(), `1934` = col_number(), 
                                        `1935` = col_number(), `1936` = col_number(), 
                                        `1937` = col_number(), `1938` = col_number(), 
                                        `1939` = col_number(), `1940` = col_number(), 
                                        `1941` = col_number(), `1942` = col_number(), 
                                        `1943` = col_number(), `1944` = col_number(), 
                                        `1945` = col_number(), `1946` = col_number(),
                                        `1947` = col_number(), `1948` = col_number(),
                                        `1949` = col_number(), `1950` = col_number(),
                                        `1951` = col_number(), `1952` = col_number(),
                                        `1953` = col_number(), `1954` = col_number(),
                                        `1955` = col_number(), `1956` = col_number(),
                                        `1957` = col_number(), `1958` = col_number(),
                                        `1959` = col_number(), `1960` = col_number(),
                                        `1961` = col_number(), `1962` = col_number(),
                                        `1963` = col_number(), `1964` = col_number(),
                                        `1965` = col_number(), `1966` = col_number(),
                                        `1967` = col_number(), `1968` = col_number(),
                                        `1969` = col_number(), `1970` = col_number(),
                                        `1971` = col_number(), `1972` = col_number(),
                                        `1973` = col_number(), `1974` = col_number(),
                                        `1975` = col_number(), `1976` = col_number(),
                                        `1977` = col_number(), `1978` = col_number(),
                                        `1979` = col_number(), `1980` = col_number(),
                                        `1981` = col_number(), `1982` = col_number(),
                                        `1983` = col_number(), `1984` = col_number(),
                                        `1985` = col_number(), `1986` = col_number(),
                                        `1987` = col_number(), `1988` = col_number(),
                                        `1989` = col_number(), `1990` = col_number(),
                                        `1991` = col_number(), `1992` = col_number(),
                                        `1993` = col_number(), `1994` = col_number(),
                                        `1995` = col_number(), `1996` = col_number(),
                                        `1997` = col_number(), `1998` = col_number(),
                                        `1999` = col_number(), `2000` = col_number(),
                                        `2001` = col_number(), `2002` = col_number(),
                                        `2003` = col_number(), `2004` = col_number(),
                                        `2005` = col_number(), `2006` = col_number(),
                                        `2007` = col_number(), `2008` = col_number(),
                                        `2009` = col_number(), `2010` = col_number(),
                                        `2011` = col_number(), `2012` = col_number(),
                                        `2013` = col_number(), `2014` = col_number(),
                                        `2015` = col_number(), `2016` = col_number()), 
                       skip = 4) %>% 
  select(-Line)

names_table <- colnames(table1_1_6)
names_table[1] <- "Measure"
colnames(table1_1_6) <- names_table

table1_1_6 <- table1_1_6 %>% 
  filter(Measure == "Gross domestic product") %>% 
  gather(year, amount, -Measure) %>% 
  mutate(year = as.numeric(year), amount = as.numeric(amount))

table1_1_5 <- read_csv("data/table_1-1-5.csv", 
                       col_types = cols(`1929` = col_number(), `1930` = col_number(), 
                                        `1931` = col_number(), `1932` = col_number(), 
                                        `1933` = col_number(), `1934` = col_number(), 
                                        `1935` = col_number(), `1936` = col_number(), 
                                        `1937` = col_number(), `1938` = col_number(), 
                                        `1939` = col_number(), `1940` = col_number(), 
                                        `1941` = col_number(), `1942` = col_number(), 
                                        `1943` = col_number(), `1944` = col_number(), 
                                        `1945` = col_number(), `1946` = col_number(),
                                        `1947` = col_number(), `1948` = col_number(),
                                        `1949` = col_number(), `1950` = col_number(),
                                        `1951` = col_number(), `1952` = col_number(),
                                        `1953` = col_number(), `1954` = col_number(),
                                        `1955` = col_number(), `1956` = col_number(),
                                        `1957` = col_number(), `1958` = col_number(),
                                        `1959` = col_number(), `1960` = col_number(),
                                        `1961` = col_number(), `1962` = col_number(),
                                        `1963` = col_number(), `1964` = col_number(),
                                        `1965` = col_number(), `1966` = col_number(),
                                        `1967` = col_number(), `1968` = col_number(),
                                        `1969` = col_number(), `1970` = col_number(),
                                        `1971` = col_number(), `1972` = col_number(),
                                        `1973` = col_number(), `1974` = col_number(),
                                        `1975` = col_number(), `1976` = col_number(),
                                        `1977` = col_number(), `1978` = col_number(),
                                        `1979` = col_number(), `1980` = col_number(),
                                        `1981` = col_number(), `1982` = col_number(),
                                        `1983` = col_number(), `1984` = col_number(),
                                        `1985` = col_number(), `1986` = col_number(),
                                        `1987` = col_number(), `1988` = col_number(),
                                        `1989` = col_number(), `1990` = col_number(),
                                        `1991` = col_number(), `1992` = col_number(),
                                        `1993` = col_number(), `1994` = col_number(),
                                        `1995` = col_number(), `1996` = col_number(),
                                        `1997` = col_number(), `1998` = col_number(),
                                        `1999` = col_number(), `2000` = col_number(),
                                        `2001` = col_number(), `2002` = col_number(),
                                        `2003` = col_number(), `2004` = col_number(),
                                        `2005` = col_number(), `2006` = col_number(),
                                        `2007` = col_number(), `2008` = col_number(),
                                        `2009` = col_number(), `2010` = col_number(),
                                        `2011` = col_number(), `2012` = col_number(),
                                        `2013` = col_number(), `2014` = col_number(),
                                        `2015` = col_number(), `2016` = col_number()), 
                       skip = 4) %>% 
  select(-Line)

names_table <- colnames(table1_1_5)
names_table[1] <- "Measure"
colnames(table1_1_5) <- names_table

table1_1_5 <- table1_1_5 %>% 
  filter(Measure == "Gross domestic product") %>% 
  gather(year, amount, -Measure) %>% 
  mutate(year = as.numeric(year), amount = as.numeric(amount))

table1_1_1 <- read_csv("data/table_1-1-1.csv", 
                       col_types = cols(`1929` = col_number(), `1930` = col_number(), 
                                        `1931` = col_number(), `1932` = col_number(), 
                                        `1933` = col_number(), `1934` = col_number(), 
                                        `1935` = col_number(), `1936` = col_number(), 
                                        `1937` = col_number(), `1938` = col_number(), 
                                        `1939` = col_number(), `1940` = col_number(), 
                                        `1941` = col_number(), `1942` = col_number(), 
                                        `1943` = col_number(), `1944` = col_number(), 
                                        `1945` = col_number(), `1946` = col_number(),
                                        `1947` = col_number(), `1948` = col_number(),
                                        `1949` = col_number(), `1950` = col_number(),
                                        `1951` = col_number(), `1952` = col_number(),
                                        `1953` = col_number(), `1954` = col_number(),
                                        `1955` = col_number(), `1956` = col_number(),
                                        `1957` = col_number(), `1958` = col_number(),
                                        `1959` = col_number(), `1960` = col_number(),
                                        `1961` = col_number(), `1962` = col_number(),
                                        `1963` = col_number(), `1964` = col_number(),
                                        `1965` = col_number(), `1966` = col_number(),
                                        `1967` = col_number(), `1968` = col_number(),
                                        `1969` = col_number(), `1970` = col_number(),
                                        `1971` = col_number(), `1972` = col_number(),
                                        `1973` = col_number(), `1974` = col_number(),
                                        `1975` = col_number(), `1976` = col_number(),
                                        `1977` = col_number(), `1978` = col_number(),
                                        `1979` = col_number(), `1980` = col_number(),
                                        `1981` = col_number(), `1982` = col_number(),
                                        `1983` = col_number(), `1984` = col_number(),
                                        `1985` = col_number(), `1986` = col_number(),
                                        `1987` = col_number(), `1988` = col_number(),
                                        `1989` = col_number(), `1990` = col_number(),
                                        `1991` = col_number(), `1992` = col_number(),
                                        `1993` = col_number(), `1994` = col_number(),
                                        `1995` = col_number(), `1996` = col_number(),
                                        `1997` = col_number(), `1998` = col_number(),
                                        `1999` = col_number(), `2000` = col_number(),
                                        `2001` = col_number(), `2002` = col_number(),
                                        `2003` = col_number(), `2004` = col_number(),
                                        `2005` = col_number(), `2006` = col_number(),
                                        `2007` = col_number(), `2008` = col_number(),
                                        `2009` = col_number(), `2010` = col_number(),
                                        `2011` = col_number(), `2012` = col_number(),
                                        `2013` = col_number(), `2014` = col_number(),
                                        `2015` = col_number(), `2016` = col_number()), 
                       skip = 4) %>% 
  select(-Line)

names_table <- colnames(table1_1_1)
names_table[1] <- "Measure"
colnames(table1_1_1) <- names_table

table1_1_1 <- table1_1_1 %>% 
  filter(Measure == "Gross domestic product") %>% 
  gather(year, amount, -Measure) %>% 
  mutate(year = as.numeric(year), amount = as.numeric(amount))

ggplot() +
  aes(x = year, y = amount) +
  geom_point(data = table1_1_6) +
  geom_point(data = table1_1_5)

ggplot(table1_1_1) +
  aes(x = year, y = amount) +
  geom_point() +
  geom_hline(yintercept = 3) +
  geom_smooth(method = "lm")

# tabn106_10 <- read_excel("~/Documents/Analyses/US CPI GDP/data/tabn106.10.xls", 
#                          col_names = FALSE, col_types = c("text", 
#                                                           "numeric", "skip", "skip", "numeric", 
#                                                           "text", "numeric", "numeric", "numeric", 
#                                                           "skip", "numeric"), 
#                          skip = 5,
#                          na = c("NA", "---", ""), n_max = 64) %>% 
#   strip_na_rows() %>% 
#   setNames(c("Year","GDP_current_BUSD","All_MUSD","All_percGDP","Prim_Sec_MUSD","Prim_sec_percGDP","PostSec_MUSD","PostSec_percGDP")) %>% 
#   mutate(Year = as.numeric(LEFT(Year, 4)))
# 
# Ed_GDP_df <- table1_1_6 %>% inner_join(tabn106_10, by = c("year" = "Year"))
# 
# ed_gdp_ccf <- ccf(x = Ed_GDP_df$amount, Ed_GDP_df$All_MUSD, na.action = na.pass, lag.max = 50)
# ed_gdp_ccf$lag[which(ed_gdp_ccf$acf == max(ed_gdp_ccf$acf))]
# 
# Ed_GDP_df %>% 
#   select(year, amount, All_MUSD, Prim_Sec_MUSD, PostSec_MUSD) %>% 
#   na.omit() %>% 
#   ggplot() +
#   aes(x = All_MUSD, y = amount) +
#   geom_point()
# 
# Ed_GDP_df %>% 
#   select(year, amount, All_MUSD, Prim_Sec_MUSD, PostSec_MUSD) %>% 
#   na.omit() %>% 
#   ggplot() +
#   aes(x = Prim_Sec_MUSD, y = amount) +
#   geom_point()
# 
# Ed_GDP_df %>% 
#   select(year, amount, All_MUSD, Prim_Sec_MUSD, PostSec_MUSD) %>% 
#   na.omit() %>% 
#   ggplot() +
#   aes(x = PostSec_MUSD, y = amount) +
#   geom_point()
