import_path = "Import/no_p_p_o.csv"

df = read.csv(import_path, header=TRUE, sep=",", stringsAsFactors = TRUE)

df$count_log = log(df$ct)

r <- hist(df$count_log)
plot(r$breaks[-1], 
     r$counts, 
     type='l', 
     log = 'y',
     main = "log-log plot of no pull requests to frequency per owner 
     (considers all pull requests between 2014-01-01 and 2017-07-31)", 
     xlab = "log(count pull requests per owner)",
     ylab = "log(frequency)")

quantile(df$ct, prob = c(0.8, 0.9, 0.98, 0.99, 0.999))

# hist(df$count)

summary(df)
summary(df$count) 

boxplot(df$count)
hist(df$ct)
