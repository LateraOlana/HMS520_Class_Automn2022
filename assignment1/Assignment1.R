# Name, place of birth and push ups
hometown <- "Nekemte"
my_name <- "Latera T. Olana"
#On Earth sure, maximum 5 - 5 is even only on a good day. 
#But I am imagining my self on planet with 0 gravity.
# Question for the philosophers, would there be push 'ups' in 0 gravity, if
# nothing is pulling us 'down' -- mind blown
num_push_ups <- Inf
print(typeof(num_push_ups))
need_exercise <- c(num_push_ups <= 5)

#Creating a vector,
vec1 <- c(1, 2, 3, 4, 5)
vec2 <- rep(10,100)
vec3<- rep(vec1,2)
vec4 <- rep(vec1, each=2)
vec5 <- seq(0, 1, by=0.01)  

#Work on data rivers
data("rivers")
# The following line of code checks if the rivers data-type is vector or not.
#Accordingly, it returns True if it is vector, false if not. In this case
# it returned True, implying rivers is a vector.
is.vector(rivers)
# The following line of code checks if the rivers data-type is list or not.
#Accordingly, it returns True if it is list, false if not. In this case
# it returned False, implying rivers is not a list.
is.list(rivers)
# The following line of code returns variable types of rivers elements.
#Accordingly, it returned double implying elements of rivers are double.
typeof(rivers) 

log_rivers <- log10(rivers)
summary <- c(length = length(log_rivers), sum = sum(log_rivers), median = median(log_rivers), mean = mean(log_rivers), minimum = min(log_rivers), maximum = max(log_rivers), sd = sd(log_rivers))
summary
names(summary) <- c("Length","Summation", "Median","Mean","Minimum","Maximum","Standard Devition")
summary

#sorted in ascending order
log_rivers <- sort(log_rivers)
log_rivers

#Removing highest and lowest from log_rivers
n <- length(rivers)-10
lowest <- log_rivers[c(1:10)]
lowest
highest <- log_rivers[-c(1:n)]
highest

trimmed_log_rivers <- log_rivers[! (log_rivers%in% lowest | log_rivers%in% highest)]
trimmed_log_rivers


#Summary for trimmed summary
trimmed_summary <- c(length = length(trimmed_log_rivers), sum = sum(trimmed_log_rivers), median = median(trimmed_log_rivers), mean = mean(trimmed_log_rivers), minimum = min(trimmed_log_rivers), maximum = max(trimmed_log_rivers), sd = sd(trimmed_log_rivers))
trimmed_summary
names(trimmed_summary) <- c("Length","Summation", "Median","Mean","Minimum","Maximum","Standard Devition")
trimmed_summary


#Creating list and using lists
info <- list(x = c(5, 6, 7, 8), y = c("a", "b", "c", "d"))
info$y <- c(1, 2, 3, 4)
mean_x <- mean(info$x)
mean_y <- mean(info$y)
sapply(info, mean)

log_x <- log(info$x)
log_x <- list(logx = c(log_x))
info <- append(info, log_x)

distance <- sqrt((info$x)^2 + (info$y)^2)
distance <- list(dist = c(distance))
info <- append(info, distance)
info$logx <- NULL
info


#Manipulating string through vector
words <- c("brilliant", "joy", "positivity", "strength", "health", "insight")
words_of_the_day <- paste(words, "is the word of the day!")

#Extracting depending on the first character
a_h_words <- substring(words,first=1, last = 1)
a_h_words <- words[a_h_words<= 'h']
a_h_words

#Extracting depending on the last character
h_y_words <- substring(words,nchar(words)-1+1, nchar(words))
h_y_words <- words[h_y_words<= 'y' & h_y_words>= 'h']
h_y_words
