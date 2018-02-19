# import_path = "Import/repo_b_100_comm.csv"
import_path = "Import/issue_c_per_repo.csv"
df = read.csv(import_path, header=TRUE, sep=",", stringsAsFactors = TRUE)

df$count_log = log(df$count)

r <- hist(df$count_log)
plot(r$breaks[-1], 
     r$counts, 
     type='l', 
     log = 'y',
     main = "log-log plot of no issues to frequency per repository", 
     xlab = "log(count issues per repository)",
     ylab = "log(frequency)")

# hist(df$count)

summary(df$count) 

boxplot(df$count)

import_path = "Import/comment_count_all_excl_none.csv"
df = read.csv(import_path, header=TRUE, sep=",", stringsAsFactors = TRUE)
df[is.na(df)] <- 0
df$sum = df$i_count + df$c_count + df$p_count



r <- hist(log(df$c_count))
r <- hist(log(df$i_count))
r <- hist(log(df$p_count))


r <- hist(log(df$sum))

plot(r$breaks[-1] , 
     r$counts, 
     type='l',
     log= 'y',
     main = "log-log plot of no comments to frequency per repository", 
     xlab = "log(count comments per repository)",
     ylab = "log(frequency)",
     axes = TRUE)


df_sample <- df[sample(nrow(df), nrow(df)*0.01), ]
df_sample <- df_sample[,-1]
boxplot((df_sample$sum), 
        main = "No of comments to repositories", 
        ylab = "no of comments to a repository (log)",
        sub = paste("sample size: ",  toString(round(nrow(df)*0.005))),
        log = "y")

plot(df$ , 
     r$counts, 
     type='p',
     log= 'xy',
     main = "log-log plot of no comments to frequency per repository", 
     xlab = "log(count comments per repository)",
     ylab = "log(frequency)",
     axes = TRUE)
