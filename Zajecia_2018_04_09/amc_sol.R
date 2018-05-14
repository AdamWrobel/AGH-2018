library(ggplot2)
library(ghyp)
library(MASS)
library(dplyr)

TSGeneration <- function(NPoints, NOutliers, mean, sigma, distr = "GAUSS") {
    distrib <- NULL
    if (distr == "GAUSS") {
        alpha <- 100000 #tail heavyness
        beta <- 0 # asymetry
        delta <- sigma ^ 2 * alpha
        mu <- mean - delta * beta / sqrt(alpha ^ 2 - beta ^ 2)
        distrib <- NIG.ad(mu = mu, delta = sigma * sigma * alpha, alpha = alpha, beta = beta, data = NULL)
    }
    else {
        alpha <- 10 #tail heavyness
        beta <- 9 # asymetry
        delta <- sigma ^ 2 * alpha
        mu <- mean - delta * beta / sqrt(alpha ^ 2 - beta ^ 2)
        distrib <- NIG.ad(mu = mu, delta = sigma * sigma * alpha, alpha = alpha, beta = beta, data = NULL)
    }

    outliers <- sample(1:NPoints, NOutliers, replace = F)
    brownians <- rghyp(NPoints, distrib)
    for (i in outliers) {
        brownians[i] <- brownians[i] * 10
    }
    brownians
}

MCSimP <- function(from, to, r, vol, S, K) {
    days_in_year <- 360
    df <- data.frame()
    days <- to - from


    brownians <- TSGeneration(days + 1, 0, 0, 1, distr = "GAUSS")
    sprices <- numeric()
    for (day in 1:(days + 1)) {
        if (day == 1) {
            sprices[day] <- S
        }
        else {
            p <- sprices[day - 1]
            sprices[day] <- p * (1 + vol / sqrt(days_in_year) * brownians[day] + r / days_in_year)
        }
    }
    sprices
}

MCSim <- function(nPaths, from, to, r, vol, S, K) {
    dates <- seq.Date(from = from, to = to, by = 1)
    df <- data.frame()
    prices <- numeric()
    for (path in 1:nPaths) {
        prices <- MCSimP(from, to, r, vol, S, K)
        ndf <- data.frame(Date = dates, Path = paste0("Path ", path), Rate = prices)
        df <- rbind(df, ndf)
    }
    df
}

makeProfile <- function(df, from, to, freq, K) {
    sdf <- data.frame()
    now <- from + freq
    while (now < to) {
        ndf <- df %>% filter(Date == now)
        pdf <- df %>% filter(Date == now & Rate > K)
        fitResults <- ndf$Rate %>% fitdistr("log-normal")
        MLEh <- qlnorm(0.95, meanlog = fitResults$estimate[[1]], sdlog = fitResults$estimate[[2]]) - K
        MLEl <- qlnorm(0.05, meanlog = fitResults$estimate[[1]], sdlog = fitResults$estimate[[2]]) - K
        EPE <- ((pdf %>% filter(Rate > K))$Rate %>% mean) - K
        nMLEh <- data.frame(Date = now, ValueType = "Q95", Exposure = MLEh)
        nEPE <- data.frame(Date = now, ValueType = "EPE", Exposure = EPE)
        nMEAN <- data.frame(Date = now, ValueType = "MEAN", Exposure = mean(ndf$Rate) - K)
        nMLEl <- data.frame(Date = now, ValueType = "Q05", Exposure = MLEl)
        sdf <- rbind(sdf, nMLEh, nEPE, nMLEl, nMEAN)

        now <- now + freq
    }
    sdf
}

makeTradeProfile <- function(df) {
    sdf <- data.frame()

    for (now in unique(df$Date)) {
        ndf <- df %>% filter(Date == now)
        pdf <- df %>% filter(Date == now & TV > 0)
        fitResults <- ndf$TV %>% fitdistr("normal")
        MLEh <- qnorm(0.95, mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]])
        MLEl <- qnorm(0.05, mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]])
        EPE <- ((pdf %>% filter(TV > 0))$TV %>% mean)
        nMLEh <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "Q95", Exposure = MLEh)
        nEPE <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "EPE", Exposure = EPE)
        nMEAN <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "MEAN", Exposure = mean(ndf$TV))
        nMLEl <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "Q05", Exposure = MLEl)
        sdf <- rbind(sdf, nMLEh, nEPE, nMLEl, nMEAN)
    }
    x11(width = 10, height = 6)
    (sdf %>% ggplot(aes(Date, Exposure, group = ValueType, color = ValueType)) + geom_line()) %>% print
    sdf
}

makeTradeProfileProper <- function(df) {
    sdf <- data.frame()

    for (now in unique(df$Date)) {
        ndf <- df %>% filter(Date == now)
        pdf <- df %>% filter(Date == now & TV > 0)
        fitResults <- ndf$TV %>% fitdistr("normal")
        MLEh <- qnorm(0.95, mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]])
        MLEl <- qnorm(0.05, mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]])
        EPE <- ((pdf %>% filter(TV > 0))$TV %>% mean)
        nMLEh <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "Q95", Exposure = MLEh)
        nEPE <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "EPE", Exposure = EPE)
        nMEAN <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "MEAN", Exposure = mean(ndf$TV))
        nMLEl <- data.frame(Date = as.Date(now, origin = "1970-01-01"), ValueType = "Q05", Exposure = MLEl)
        sdf <- rbind(sdf, nMLEh, nEPE, nMLEl, nMEAN)
    }
    x11(width = 10, height = 6)
    (sdf %>% ggplot(aes(Date, Exposure, group = ValueType, color = ValueType)) + geom_line()) %>% print
    sdf
}

calculateCashFlows = function(notional, data, fixedRate, freq) {
    range = as.double(max(data$Date) - min(data$Date))
    nCFs = as.integer(range / freq)
    dates = seq(from = min(data$Date), to = max(data$Date), by = "quarter")[-1]
    cashflows = data %>% filter(is.element(Date, dates)) %>% mutate(CF = (notional * (Rate - fixedRate) / 4))
    #cashflows = data %>% filter(is.element(Date, dates)) %>% mutate(CF = Date-min(data$Date)) # for testing purposes
    firstRow = data.frame(Path = unique(data$Path))
    firstRow$Date = min(data$Date)
    firstRow$Rate = (data %>% filter(Date == min(data$Date)))$Rate[[1]]
    firstRow$CF = 0
    cashflows = rbind(firstRow, cashflows)
}

integrateCashFlows = function(cfData) {
    newRow = list()
    dates = unique(cfData$Date)
    #newData = cfData %>% group_by(Path) %>% mutate(TV = (cumsum(rev(lag(CF, 1)))))
    i = 0
    for (path in unique(cfData$Path)) {
        pathData = cfData %>% filter(Path == path)
        for (date in dates) {
            -13
            i = i + 1
            todayPathData = pathData %>% filter(Date == date)
            newRow[[i]] = data.frame(Date = as.Date(date, origin = "1970-01-01"), Path = path, Rate = todayPathData$Rate[[1]], CF = todayPathData$CF[[1]], TV = sum((pathData %>% filter(Date > date))$CF))
        }
    }
    outData = rbind_all(newRow)
    outData
}

plotDistrForDate = function(cfData, date) {
    x11(width = 10, height = 6)
    (cfData %>% filter(Date == date) %>% ggplot(aes(TV)) + geom_histogram(aes(y = ..density..))) %>% print
}

plotCFvsRF = function(cfData, date) {
    x11(width = 10, height = 6)
    (cfData %>% filter(Date == date) %>% ggplot(aes(Rate, TV)) + geom_point()) %>% print
}

plotCFvsRFAllDates = function(cfData) {
    dates = unique(cfData$Date)
    for (i in (1:length(dates))) {
        if ((i %% 5) == 0) plotCFvsRF(cfData, dates[[i]])
        }
}

regressCFs = function(cfData) {
    output = cfData %>% group_by(Date) %>%
    do(mod = lm(TV ~ Rate, data = .)) %>%
    mutate(Slope = summary(mod)$coeff[2], Offset = summary(mod)$coeff[1]) %>% dplyr::select(-mod)
    output = cfData %>% left_join(output, by = "Date")
    output = output %>% ungroup() %>% mutate(TV = Slope * Rate + Offset)
    output
}

from <- as.Date("2015-01-01")
to <- as.Date("2020-01-01")
nPaths <- 1000

S <- 1
K <- 0
r <- 0.1
vol <- 0.1

data <- MCSim(nPaths, from, to, r, vol, S, K)
#(data %>% ggplot(aes(Date, Price, group = Path, color = Path)) + geom_line()) %>% 
sdf <- makeProfile(data, from, to, 10, K)
(sdf %>% ggplot(aes(Date, Exposure, group = ValueType, color = ValueType)) + geom_line()) %>% print


notional = 100000
fixedRate = 1.33
freq = 90

cfData = calculateCashFlows(notional, data, fixedRate, freq)
cfData = integrateCashFlows(cfData)
plotDistrForDate(cfData, from)
profile = makeTradeProfile(cfData)

plotCFvsRFAllDates(cfData)

extra = regressCFs(cfData)
profile = makeTradeProfile(extra)

print(mean((cfData %>% filter(Date == from))$TV))
