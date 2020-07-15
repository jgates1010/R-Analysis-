#Call Packages 
install.packages("xts")
install.packages("viridis")
library(xts)
library(readr)
library(viridis)
library(rugarch)

#Load Fed Bond Data 
yc_raw <- read.csv("FED-SVENY.csv")

#Convert to xts 
date <- as.Date(yc_raw$Date)
yc_all <- as.xts(yc_raw[,-1], order.by = date)

#View Tail
yc_all_tail <- tail(yc_all[,c(1,5,10,20,30)])
yc_all_tail


##################
#Visualize Yields#
#################
#Define Arguments 
yields <- yc_all
plot.type <- "single"
plot.palette <- viridis(30)
asset.names <- colnames(yc_all)

#Plot Yields 
x11()
plot.zoo(yields, plot.type = plot.type, col = plot.palette,
         xlab = "Year", main = "US Treasury Bond Yields")
legend("topleft", legend = asset.names,
     col = plot.palette, cex = 0.45, lwd = 3)
#The Bond Yield Data Is Clearly Noto Stationary 

###############
#Differentiate#
##############
ycc_all <- diff.xts(yc_all)

#View Tail For Maturities of Interest 
ycc_all_tail <- tail(ycc_all[,c(1,5,10,20,30)])
ycc_all_tail


####################
#Plot Daily Returns#
###################
#Define Arguments 
returns <- ycc_all
plot.type <- "multiple" 


#Plot Returns 
x11()
plot.zoo(returns, plot.type = plot.type, 
     ylim = c(-0.5, 0.5), cex.axis = 0.7, 
     ylab = 1:30, col = plot.palette)

#####################################
#Autocorrelations For Recent Returns#
####################################
#Returns 2000 onward
ycc <- ycc_all["2000/"]
head(ycc)
 
#1 and 20 year maturities 
x_1 <- ycc[,1]
x_20 <- ycc[,20]

#Plot return autocorrelations 
par(mfrow = c(2,2))
acf(x_1)
acf(x_20)
#Note there is little to no significant autocorrelation
#Plot absolute return autocorrelation
acf(abs(x_1))
acf(abs(x_20))

#############
#GARCH Model#
############
#Specify Model 
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(1,1), 
                                     include.mean = TRUE),
                   distribution.model = "sstd")
#Fit model 
fit_1 <- ugarchfit(spec = spec, data = x_1)

#Volatilities and residuals 
vol_1 <- fit_1@fit$sigma
res_1 <- scale(residuals(fit_1, standardize = TRUE))*sd(x_1)+mean(x_1)

#Plot returns with estimated volatility and residuals 
merge_1 <- merge.xts(x_1, vol_1, res_1)
x11()
plot.zoo(merge_1, 
         main = "1 Year Bond Returns, Volatility and Residuals",
         xlab = "Year")

#20 Year Bond GARCH Model 
#Fit model 
fit_20 <- ugarchfit(spec = spec, data = x_20)

#Volatilities and residuals 
vol_20 <- fit_20@fit$sigma
res_20 <- scale(residuals(fit_20, standardize = TRUE))*sd(x_20)+mean(x_20)

#Plot returns with estimated volatility and residuals 
merge_20 <- merge.xts(x_20, vol_20, res_20)
x11()
plot.zoo(merge_20, 
         main = "20 Year Bond Returns, Volatility and Residuals",
         xlab = "Year")
###############
#Distributions#
##############
#Kernel Density For 1 Year Maturities 
density_x_1 <- density(x_1)
density_res_1 <- density(res_1)

#Plot 1 Year Maturity Density Diagram 
x11()
plot(density_x_1, main = "1 Year Bond Returns & Residuals Density")
lines(density_res_1, col = "red")

norm_dist <- dnorm(seq(-0.4, 0.4, by = .01), mean = mean(x_1), sd = sd(x_1))
lines(seq(-0.4, 0.4, by = .01), norm_dist, col = "darkgreen")
legend <- c("Before GARCH", "Afert Garch", "Normal Distribution")
legend("topleft", legend = legend, 
       col = c("black", "red", "darkgreen"), lty=c(1,1))

########
#QQPlot#
#######
# Define the data to plot: the 1-year maturity yield changes and residuals 
data_orig <- x_1
data_res <- res_1

# Define the benchmark distribution
distribution <- qnorm

# Make the Q-Q plot of original data with the line of normal distribution
qqnorm(data_orig, ylim = c(-0.5, 0.5))
qqline(data_orig, distribution = distribution, col = "darkgreen")

# Make the Q-Q plot of GARCH residuals with the line of normal distribution
par(new=TRUE)
qqnorm(data_res * 0.614256270265139, col = "red", ylim = c(-0.5, 0.5))
qqline(data_res * 0.614256270265139, distribution = distribution, col = "darkgreen")
legend("topleft", c("Before GARCH", "After GARCH"), col = c("black", "red"), pch=c(1,1))
