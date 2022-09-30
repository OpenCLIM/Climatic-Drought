# 28/09/2022
# Ben Smith
# This script is set ot test the workings of the SPEI12 calculation in
# spei_v5_looped.R. Run that script until you have the spei_map. Then 
# you can make the following comparisons


# Test the SPEI Package ---------------------------------------------------
data("wichita")
wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
wichita$BAL <- wichita$PRCP-wichita$PET

# Convert to a ts (time series) object for convenience
wichita <- ts(wichita[,-c(1,2)], end=c(2011,10), frequency=12)
plot(wichita)

# Tvelwe-months SPEI
spei12 <- spei(wichita[,'BAL'], 12)

# Extract information from spei object: summary, call function, fitted values, and coefficients
summary(spei12)
names(spei12)
spei12$call
spei12$fitted
spei12$coefficients

# Plot spei object
plot(spei12, main='Wichita, SPEI-12')

# Using a particular reference period (1980-2000) for computing the parameters
plot(spei(ts(wichita[,'BAL'], freq=12, start=c(1980,6)), 12,
          ref.start=c(1980,1), ref.end=c(2000,1)))

# Test with our data - you'll have to load/ calculate this and come back up.
cell_row = 29
cell_col = 21
test_data = data.frame("PR" = pr_masked[cell_row, cell_col,],
                       "PET"= pet_masked[cell_row, cell_col,])
test_data$BAL <- test_data$PR - test_data$PET
test_data$month = sort(rep(1:(12*10*3), 30))

# Aggregate to monthly:
test_data_monthly <- data.frame(
  PR = do.call(rbind, as.list(by(data = test_data$PR, test_data$month, sum))),
  PET = do.call(rbind, as.list(by(data = test_data$PET, test_data$month, sum))),
  BAL = do.call(rbind, as.list(by(data = test_data$BAL, test_data$month, sum))))

test_data_monthly <- ts(test_data_monthly, start= c(1980,11), frequency=12)
plot(test_data_monthly)
# So at this stage, the BAL is the same as the climate_balance_monthly_map, so that makes sence!

spei12 <- spei(test_data_monthly[,'BAL'], 12)

# Extract information from spei object: summary, call function, fitted values, and coefficients
summary(spei12)
names(spei12)
spei12$call
spei12$fitted
spei12$coefficients

# Plot spei object
plot(spei12, main='Liverpool Area, SPEI-12')

plot(as.vector(spei12$fitted), type = "l")
lines(spei_map[cell_row, cell_col,], type = "l", col = 2)
# So, the length of time that you calculate it for affects the value of SPEI
# so you can't compare a 10 year calculation to a 30 year calculation. They 
# follow the same patterns, but aren't identical. When calculated for the same 
# timeframe, we get the same results above as we do below. So we calculate is
# correctly in the main script I think.

# plot_ly(x = 1:360, y = test_data_monthly[,"PR"], type = 'scatter', mode = 'lines', name ='pr') %>%
#   add_lines(x = 1:360, y = test_data_monthly[,"PET"], type = 'scatter', mode = 'lines', name ='pet') %>%
#   add_lines(x = 1:360, y = test_data_monthly[,"BAL"], type = 'scatter', mode = 'lines', name ='bal') %>%
#   add_lines(x = 1:360, y = spei_map[cell_row, cell_col,], type = 'scatter', mode = 'lines', name ='SPEI', yaxis = "y2") %>%
#   layout(yaxis2 = list(side='right', overlaying='y'))
# 
# plot_ly(x = 1:90, y = climate_balance_monthy_map[cell_row, cell_col, 1:90], 
#         type = 'scatter', mode = 'lines', name ='pr') %>%
#   add_lines(x = 1:90, y = spei_map[cell_row, cell_col, 1:90], type = 'scatter', 
#             mode = 'lines', name ='SPEI', yaxis = "y2") %>%
#   layout(yaxis2 = list(side='right', overlaying='y'))

# Try a plot using the moving average of climate balance and SPEI:
ma <- function(x, n = 12){stats::filter(x, rep(1 / n, n), sides = 1)}

plot_ly(x = 1:90, y = ma(climate_balance_monthy_map[cell_row, cell_col, 1:90]), 
        type = 'scatter', mode = 'lines', name ='pr') %>%
  add_lines(x = 1:90, y = spei_map[cell_row, cell_col, 1:90], type = 'scatter', 
            mode = 'lines', name ='SPEI', yaxis = "y2") %>%
  layout(yaxis2 = list(side='right', overlaying='y'))

# WOW - SPEI is just a moving average by the look of it... But this shows that
# the calculations are matching up.



levelplot(climate_balance_monthy_map[,,45],
          col.regions = terrain.colors(12),
          at=seq(min(climate_balance_monthy_map[,,45], na.rm = T),
                 max(climate_balance_monthy_map[,,45], na.rm = T),
                 length.out=12))
levelplot(spei_map[,,64],
          col.regions = terrain.colors(12),
          at=seq(min(spei_map[,,64], na.rm = T),
                 max(spei_map[,,64], na.rm = T),
                 length.out=12))

# So, it seems to work... or at least the method we use creates the same SPEI 
# values as we do if we do it cell by cell using their method. This does not
# seem to require negative climate balances.

# So, I'm pretty sure that this is all correct!