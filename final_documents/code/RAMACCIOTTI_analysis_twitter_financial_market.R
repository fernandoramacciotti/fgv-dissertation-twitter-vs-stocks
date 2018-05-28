# Fernando Martinelli Ramacciotti
# fernandoramacciotti@gmail.com
# Mestrado Profissional em Economia FGV
# Influencia do Twitter no mercado financeiro 

rm(list=ls())

# Libraries and custom functions ------------------------------------------

library(vars)
library(astsa)
library(stargazer)
library(xtable)
library(tseries)
library(FactoMineR)
library(ggplot2)
library(xts)
library(zoo)
library(stargazer)
library(Quandl)
library(dplyr)
library(quantmod)
library(reshape2)

# plotting, stationary tests and ACF/PACF plots
plot_and_test_ts <- function(x, title, fontsize=16) {
  
  print({ggplot(x) + 
      geom_line(aes(x = index(x), y = x), col = 'steelblue') + 
      theme_bw() + 
      xlab('') + 
      ylab('') + 
      #ggtitle(title) +
      theme(panel.grid.minor.y = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dashed'),
            panel.border = element_blank(),
            axis.line.x = element_line(), 
            axis.line.y = element_line(colour = 'lightgrey', linetype = 'dashed'),
            text=element_text(size=fontsize),
            panel.background = element_blank())
  })
  ggsave(paste('./plots/', title, '.png', sep = ''))
  
  # tests
  print(adf.test(x))
  print(kpss.test(x, null = 'Level'))
  print(kpss.test(x, null = 'Trend'))
  print(Box.test(x))
  print(PP.test(x))
  
  # ACF and PACFs plots
  
  png(paste('./plots/', title, '_ACF_PACF', '.png', sep = ''), width = 677, height = 552, units = 'px')
  
  par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
  print(acf(x, main = title))
  print(pacf(x, main = ''))
  
  dev.off()
  par(mfrow = c(1,1)) # reset subplots
  
  par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
  print(acf(x, main = title))
  print(pacf(x, main = ''))
  par(mfrow = c(1,1)) # reset subplots
  
}


# Descriptive statistics of tweets ----------------------------------------
tweets <- read.csv('../out/tweets_data.csv', sep = ';')

grouped_tweets <- tweets %>% 
        mutate(yr  = format(as.Date(timestamp), format = '%Y'), 
               mth = format(as.Date(timestamp), format = '%m')) %>%
        group_by(yr, mth) %>%
        summarize(no_tweets = n()) %>%
        mutate(yr_mth = paste(yr, mth, sep= '-'))

grouped_tweets %>% group_by(yr) %>% summarise(N           = sum(no_tweets),
                                              avg_monthly = mean(no_tweets),
                                              sd_no       = sd(no_tweets),
                                              min_no      = min(no_tweets),
                                              max_no      = max(no_tweets)
                                              ) %>% as.data.frame()
plt_tw <- grouped_tweets %>% group_by(yr_mth) %>% summarise(n = sum(no_tweets))
plt_tw <- plt_tw[1:(nrow(plt_tw)-1) ,]
plot(format(plt_tw$yr_mth, format = '%Y-%m'), plt_tw$n)

# General Inquirer data ---------------------------------------------------

data <- read.csv('../input/GeneralInquirer-output', sep = '\t')

# cleaning infintes and NAN ----

#data <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),NA)))
#data <- data[complete.cases(data), ]

raw_count <- subset(data, format == 'r')
scale_count <- subset(data, format == 's')
#scale_count <- scale_count[5:ncol(scale_count)-1]
data_to_use <- scale_count



# PCA ----

categories = c('Pstv', 'Ngtv', 'Strong', 'Weak'
               #'Active', 'Passive', 'Pleasur', 
               #'Pain', 'Virtue', 'Vice', 'Ovrst', 'Undrst', 'Causal', 'WltPt', 'WltTran'
#               'POLIT', 'ECON', 'Work', 'EVAL', 'Self', 'Know'
)

# LATEX correlation table
sent_cor <- cor(data_to_use[categories])
#sent_cor <- round(sent_cor, 2)
sent_cor[upper.tri(sent_cor)] <- NA
sent_cor
xtable(sent_cor, floating=FALSE, latex.environments=NULL, booktabs=TRUE)

summary(data_to_use[categories])

# columns with cte variance
#names(scale_count[, sapply(scale_count, function(v) var(v, na.rm=TRUE)==0)])

pca <- prcomp(data_to_use[categories] / 100, scale. = T, center = T, retx = T)
#pca$loadings
#screeplot(pca)
#biplot(pca, scale=0)
xtable(summary(pca), floating=FALSE, latex.environments=NULL, booktabs=TRUE)
xtable(pca, floating=FALSE, latex.environments=NULL, booktabs=TRUE)

summary(pca)
pca
# plotting
# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(data_to_use[categories], pca$x))

# data frame with arrows coordinates
arrows = data.frame(x1 = rep(0, length(categories)), y1 = rep(0, length(categories)), x2 = correlations$PC1, 
                    y2 = correlations$PC2)

# geom_path will do open circles
ggplot() + geom_path(data = corcir, aes(x = -x, y = -y), colour = 'black') + 
  geom_segment(data = arrows, aes(x = -x1, y = -y1, xend = -x2, yend = -y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = -PC1, y = -PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + 
  geom_vline(xintercept = 0, colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs", y = "pc2 axis") + 
  #ggtitle("Circle of correlations") + 
  xlab('First Component Axis') + 
  ylab('Second Component Axis') +
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major = element_line(colour = 'lightgrey', linetype = 'dashed'),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggsave('./plots/circle_pca_comp.png')



# create dates vector -----------------------------------------------------

dates <- as.Date((gsub('.txt', '', data[data$format == 's', ]$file)), 
                    format = '%Y-%m-%d')



# creating factors --------------------------------------------------------

# 1st component
factor_1 <- -1 * pca$x[, 1]
factor_1 <- xts(factor_1, order.by = dates)

# 2nd component
factor_2 <- -1 * pca$x[, 2]
factor_2 <- xts(factor_2, order.by = dates)

# merging
factors <- merge.xts(factor_1, factor_2)
names(factors) <- c('EF', 'OF')

plot_and_test_ts(factors$EF, 'Engagement Factor')
plot_and_test_ts(factors$OF, 'Optimism Factor')
adf.test(diff(factors$OF)[2:nrow(factors)])

# plot of categories alltogether
df <- xts(data_to_use[categories], order.by=dates)
plot_and_test_ts(df$Pstv, 'pstv_factor')
plot_and_test_ts(df$Ngtv, 'ngtv_factor')
plot_and_test_ts(df$Strong, 'strong_factor')
plot_and_test_ts(df$Weak, 'weak_factor')


# importing financial data ------------------------------------------------

# raw data
sp500 <- read.csv('../input/SP500.csv')
tbills <- Quandl('USTREASURY/YIELD') # rate of treasury bills
vix <- read.csv('../input/VIX.csv')

# setting xts time series data
tbill_3MO_xts <- xts(tbills$`3 MO` / 100, tbills$Date)
sp500_xts <- xts(sp500[, 2:ncol(sp500)], as.Date(sp500$Date))
vix_xts <- xts(vix$Adj.Close, as.Date(vix$Date))

# merging financials
financials <- merge.xts(sp500_xts[ ,c('Adj.Close', 'Volume')], tbill_3MO_xts, join = 'inner')
names(financials) <- c('SP500_AdjClose', 'SP500_Vlm', 'TBILL_3MO')
financials <- merge.xts(financials, vix_xts, join = 'inner')
names(financials) <- c('SP500_AdjClose', 'SP500_Vlm', 'TBILL_3MO', 'VIX')

# excess of S&P500 returns wrt 3-MO T-Bill, change of log volume and change of log VIX
financials$sp500_log_ix <- log(financials$SP500_AdjClose)
financials$sp500_ret <- diff(financials$sp500_log_ix)

financials$TBILL_3MO_daily_ret <- (1 + financials$TBILL_3MO) ^ (1/252) - 1
financials$sp500_xsret <- financials$sp500_ret - financials$TBILL_3MO_daily_ret

financials$sp500_log_vlm <- log(financials$SP500_Vlm)
financials$sp500_log_vlm_diff <- diff(financials$sp500_log_vlm)
financials$vix_log <- log(financials$VIX)
financials$vix_log_diff <- diff(financials$vix_log)

# filter by dates of interest
financials <- financials[paste(dates[1], dates[length(dates)], sep = '::')]
financials <- financials[complete.cases(financials), ] # removing NAs

# Descriptive statisticas of financial data -------------------------------

xtable(apply(financials$SP500_AdjClose, function(x) cbind(mean(x), sd(x), min(x), max(x))))
apply(financials$VIX, function(x) cbind(mean(x), sd(x), min(x), max(x)))
apply(financials$SP500_Vlm, function(x) cbind(mean(x), sd(x), min(x), max(x)))

xtable(summary(financials[, c('SP500_AdjClose', 'VIX', 'SP500_Vlm')]))
sd(financials[, 'SP500_AdjClose'])  ## 451.28
sd(financials[, 'VIX'])             ## 9.63
sd(financials[, 'SP500_Vlm']/1e9)   ## 1.16 billion

# Plotting financials ----




# VIX, log of VIX and first diff of VIX --
plot_and_test_ts(financials$VIX, 'VIX (Level)', fontsize = 20) # level - stationary
plot_and_test_ts(financials$VIX, 'VIX (Level) 16') # level - stationary
plot_and_test_ts(financials$vix_log, 'Log of VIX') # log - stationary
plot_and_test_ts(financials$vix_log_diff, 'VIX log first difference') # first log diff - stationary

# SP&500 level, log-return and excess of return --
plot_and_test_ts(tbills$`3 MO`, 'Risk_free') # level - non-stationary

# SP&500 level, log-return and excess of return --
plot_and_test_ts(financials$SP500_AdjClose, 'S&P 500 (Level)', fontsize = 20) # level - non-stationary
plot_and_test_ts(financials$SP500_AdjClose, 'S&P 500 (Level) 16') # level - non-stationary
plot_and_test_ts(financials$sp500_ret, 'S&P 500 log return') # log return - stationary (some volatitly clusters are notorious)
plot_and_test_ts(financials$sp500_xsret, 'S&P 500 log of excess of return') # Excess of return - stationary (but up to 2009 it seems not)

# Volume of SP&500 level, log volume and first log diff --
plot_and_test_ts(financials$SP500_Vlm, 'Trading volume of S&P 500 (Level)', fontsize = 20) # level - non-stationary
plot_and_test_ts(financials$SP500_Vlm, 'Trading volume of S&P 500 (Level) 16') # level - non-stationary
plot_and_test_ts(financials$sp500_log_vlm, 'Log of trading volume of S&P 500') # log return - stationary (some volatitly clusters are notorious)
plot_and_test_ts(financials$sp500_log_vlm_diff, 'Trading volume of S&P 500 log first difference') # Excess of return - stationary



# VAR Model ---------------------------------------------------------------

financials_to_use <- c('sp500_xsret', 'sp500_log_vlm_diff', 'vix_log_diff')
var_data <- merge.xts(factors, financials[, financials_to_use], join = 'inner')

var_selection <- VARselect(var_data)
var_selection
xtable(var_selection$criteria, booktabs = TRUE, digits = 3)

names(var_data) <- c('EF', 'OF', 'Ret', 'Vlm', 'VIX')

lag_order <- 5
model <- VAR(var_data, p = lag_order)
summary(model)
plot(model)


# LATEX tables ------------------------------------------------------------


list_variables <- names(var_data)
variables_order <- c( 1,  6, 11, 16, 21, # EF
                      2,  7, 12, 17, 22, # OF
                      3,  8, 13, 18, 23, # Ret
                      4,  9, 14, 19, 24, # Vlm
                      5, 10, 15, 20, 25, # VIX
                      26)                # Cte

for (v in list_variables) {
  model_to_print <- (model$varresult)[v]
  cov_labels <- list()
  
  for (c in names(model_to_print[[1]][1]$coefficients)) {
    if (c=='const') {
      cov_labels <- c(cov_labels, '$const$')
    }
    else {
      name <- strsplit(c, split = '.', fixed = TRUE)[[1]][1]
      lag <- substr(strsplit(c, split = '.', fixed = TRUE)[[1]][2], 2, 2)
      cov_labels <- c(cov_labels, paste('$', name, '_{t-', lag, '}$', sep = ''))
    }
  }
  
  #names(model_to_print[[1]][1]$coefficients) <- unlist(cov_labels)
  # print model with stargazer
  assign(paste('table_', v, sep = ''), stargazer(model_to_print,
                                                  out = paste('./tables/', v, '.tex', sep = ''),
                                                  dep.var.labels=v, 
                                                  title = paste('Regression on', v), 
                                                  intercept.bottom = FALSE, 
                                                  single.row = TRUE, 
                                                  initial.zero = FALSE, 
                                                  align = TRUE,
                                                  order = variables_order,
                                                  covariate.labels = unlist(cov_labels[variables_order])
                                        )
  )
}

stargazer(model$varresult$EF, model$varresult$OF, model$varresult$Ret, model$varresult$VIX, model$varresult$Vlm, 
          title = 'VAR Results',
          column.labels = list_variables,
          intercept.bottom = FALSE, 
          single.row = TRUE, 
          initial.zero = FALSE, 
          align = TRUE,
          order = variables_order,
          covariate.labels = unlist(cov_labels[variables_order])
          )
