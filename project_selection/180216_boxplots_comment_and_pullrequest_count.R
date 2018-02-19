import_path = "Import/180216_no_comments_and_pull_requests_per_owner.csv"
df1 = read.csv(import_path, header=TRUE, sep=",", stringsAsFactors = TRUE)

import_path = "Import/180216_count_commits_per_owner.csv"
df2 = read.csv(import_path, header=TRUE, sep=",", stringsAsFactors = TRUE)

df1$comment_count_log = log(df1$comment_count)
df1$pull_request_count_log = log(df1$pull_request_count)
df2$commit_count_log = log(df2$commit_count)

r_comment_count <- hist(df1$comment_count_log)
r_pull_request_count <- hist(df1$pull_request_count_log)
r_commit_count <- hist(df2$commit_count_log)

logplot <- function(r, title, xlab){
  plot(r$breaks[-1], 
       r$counts, 
       type='l', 
       log = 'y',
       main = title, 
       xlab = xlab,
       ylab = "log(frequency)")
}

logplot(r=r_comment_count,
        title="log-log plot of no comments to frequency per owner 
       (considers all comments between 2014-01-01 and 2017-07-31)",
        xlab = "log(count comments per owner)")
logplot(r_pull_request_count, 
        title = "log-log plot of no pull_requests to frequency per owner 
       (considers all pull_requtests between 2014-01-01 and 2017-07-31)",
        xlab = "log(count pull requests per owner)")
logplot(r_commit_count, 
        title = "log-log plot of no commits to frequency per owner 
        (considers all commits between 2014-01-01 and 2017-07-31)",
        xlab = "log(count commits per owner)")


quantile(df1$comment_count, prob = c(0.8, 0.9, 0.98, 0.985, 0.99, 0.999))
quantile(df1$pull_request_count, prob = c(0.8, 0.9, 0.98, 0.985, 0.99, 0.999))
quantile(df2$commit_count, prob = c(0.8, 0.9, 0.98, 0.985, 0.99, 0.999))
# hist(df$count)

summary(df1)
summary(df2)

boxplot_sample <- function(data, c_name, sample_factor){
  df_sample <- data.frame(na.omit(data[, c_name]))
  sample_size = round(nrow(df_sample)*sample_factor)
  df_sample <- df_sample[sample(nrow(df_sample), sample_size), ]
  boxplot(df_sample,
          ylab = c_name,
          sub = paste(
                      "population: ", toString(nrow(data)), 
                      "\nsample size: ",  toString(sample_size), 
                      " (", toString(sample_factor*100),"%)" ),
          log = "y")
}

boxplot_sample(df1, "comment_count", 0.002)
boxplot_sample(df1, "pull_request_count", 0.002)
boxplot_sample(df2, "commit_count", 0.001)

df3 = merge(x = df1, 
      y = df2,
      by = "owner_id",
      all = TRUE)

############# filters
lim_commit_count = 500
lim_pull_request_count = 100
lim_comment_count = 2000

df4 <- select(filter(df3, comment_count >= lim_comment_count), c(owner_id, comment_count, pull_request_count, commit_count))
df5 <- select(filter(df4, (pull_request_count >= lim_pull_request_count | commit_count >= lim_commit_count)), 
              c(owner_id, comment_count, pull_request_count, commit_count))

boxplot(df5[,-1], log='y',
        ylab = 'count (log-transformed)',
        main = 'Distribution of the remaining owner data after applying selection criteria',
        sub = paste(
              'remaining number of owners: ', toString(nrow(df5)), 
              '(', toString(round(nrow(df5)/nrow(df3)*100, digits = 2)), '% of original data)',
              '\ncriteria: ',
              'no commits >= ', toString(lim_commit_count), ' OR', 
              ' no pull requests >= ', toString(lim_pull_request_count), 'AND',
              ' no comments >= ', toString(lim_comment_count))
        )


df3_log = data.frame(commit_count = log(df3$commit_count), 
                     pull_request_count = log(df3$pull_request_count), 
                     comment_count = log(df3$comment_count))

plot(df3_log)

r <- hist(na.omit(df3_log$commit_count))
plot(r$breaks[-1], 
     r$counts, 
     type='l',
     main = "Number of commits to owners on a log scale to frequency",
     sub = "considers data between 2014-01-01 and 2017-07-31", 
     xlab = "log(count commits per owner)",
     ylab = "frequency")
abline(v=log(lim_commit_count), lty = 2, col = 'blue')

