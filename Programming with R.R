setwd("~/Desktop/r-novice-inflammation/")
read.csv(file = "data/inflammation-01.csv", header = FALSE)
dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
class(dat)
dim(dat)
dat[1, 1]

#fist row, all of the colums
patient_1 <- dat[1, ]
max(patient_1)

# max inflammation for patient 2
max(dat[2, ])

?apply

avg_patient_inflammation <- apply(dat, 1, mean)
avg_patient_inflammation
avg_day_inflammation <- apply(dat, 2, mean)

whichPatients <- seq(2, 60, 2) # i.e., which rows
whichDays <- seq(1, 5)         # i.e., which columns
dat2 <- dat
# check the size of your subset: returns `30 5`, that is 30 [rows=patients] by 5 [columns=days]
dim(dat2[whichPatients, whichDays])
dat2[whichPatients, whichDays] <- dat2[whichPatients, whichDays] / 2
dat2

whichPatients
?seq

max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}
fahrenheit_to_celsius(32)
fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}
celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}
fahrenheit_to_kelvin(32)

celsius_to_kelvin(fahrenheit_to_celsius(32))

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.
highlight(best_practice, asterisk)

highlight <- function(content, wrapper) {
  answer <- c(wrapper, content, wrapper)
  return(answer)
}

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
edges(dry_principle)

edges <- function(v) {
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first, last)
  return(answer)
}
edges(dry_principle)

input_1 <- 20
mySum <- function(input_1, input_2 = 10) {
  output <- input_1 + input_2
  return(output)
}
mySum(input_1 = 1,3)
mySum(3)


center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

z <- c(0, 0, 0, 0)
z
center(z, 3)

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
centered <- center(dat[, 4], 0)
head(centered)

min(dat[, 4])
mean(dat[, 4])
max(dat[, 4])
mean(centered)
max(centered)
sd(dat[, 4])
sd(centered)
all.equal(sd(dat[, 4]), sd(centered))
sd(dat[, 4]) == sd(centered)

# new data object and set one value in column 4 to NA
datNA <- dat
datNA[10,4] <- NA

# returns all NA values
center(datNA[,4], 0)

center <- function(data, midpoint) {
  new_data <- (data - mean(data, na.rm = T)) + midpoint
  return(new_data)
}
center(datNA[, 4], 0)
center(datNA[,4], 0)

datNA[,1] <- as.factor(datNA[,1])
datNA[,2] <- as.character(datNA[,2])

center(datNA[,1], 0)
center(datNA[,2], 0)

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("data/inflammation-02.csv")

rescale <- function(v) {
  # Rescales a vector, v, to lie in the range 0 to 1.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L)
  return(result)
}

rescale(avg_patient_inflammation)

dat <- read.csv(header = FALSE, file = "data/inflammation-01.csv")
head(dat)

center <- function(data, midpoint = 0) {
  # return a new vector containing the original data centered around the
  # midpoint (0 by default).
  # Example: center(c(1, 2, 3), 0) => c(-1, 0, 1)
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

test_data <- c(0, 0, 0, 0)
center(test_data, 3)

more_data <- 5 + test_data
more_data
center(more_data)

display <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("a", "b", "c")  # This names each element of the vector
  return(result)
}

# no arguments
display()
display(55, 6, 80)
display(c = 77)

dat <- read.csv(FALSE, "data/inflammation-01.csv")

rescale <- function(v, lower = 0, upper = 1) {
  # Rescales a vector, v, to lie in the range lower to upper.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L) * (upper - lower) + lower
  return(result)
}

analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("data/inflammation-01.csv")

best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}
print_words(best_practice)
best_practice[-6]
print_words(best_practice[-6])

print_words <- function(sentence) {
  for (word in sentence) {
    print(word)
  }
}
print_words(best_practice)
print_words(best_practice[-6])

len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  len <- len + 1
}
len

letter <- "z"
for (letter in c("a", "b", "c")) {
  print(letter)
}
letter
length(vowels)
length(letter)
length(max_day_inflammation)

seq(100)
print_N(3)

print_N <- function(N) {
  nseq <- seq(N)
  for (num in nseq) {
    print(num)
  }
}
print_N(3)

ex_vec <- c(4, 8, 15, 16, 23, 42)

total <- function(vec) {
  #calculated the sum of values in a vector
  vec_sum <- 0
  for (num in vec) {
    vec_sum <- vec_sum + 1
  }
  return(vec_sum)
}
total(ex_vec)
sum(ex_vec)

expo <- function(base, power) {
  result <- 1
  for (i in seq(power)) {
    result <- result * base
  }
  return(result)
}
expo(3, 5)
expo(120398230948,20398409)
expo(123, 456)
expo(12, 123)

list.files()
getwd()
list.files(path = "data", pattern = "csv")
list.files(path = "data", pattern = "inflammation")
list.files(path = "data", pattern = "inflammation", full.names = TRUE)

filenames <- list.files(path = "data",
                        pattern = "inflammation-[0-9]{2}.csv",
                        full.names = T)
filenames <- filenames[1-3]
for (f in filenames) {
  print(f)
  analyze(f)
}

analyze_all <- function(folder = "data", pattern) {
  # Runs the function analyze for each file in the given folder
  # that contains the given pattern.
  filenames <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

filenames <- list.files(path = "data",  
                        # Now follows a regular expression that matches:
                        pattern = "inflammation-[0-9]{2}.csv",
                        #          |            |        the standard file extension of comma-separated values
                        #          |            the variable parts (two digits, each between 0 and 9)
                        #          the static part of the filenames
                        full.names = TRUE)
filenames <- filenames[1:3]
for (f in filenames) {
  print(f)
  analyze(f)
}

?dev.cur
pdf("inflammation_01")
analyze("data/inflammation-01.csv")
getwd()
setwd("/Users/jonathanhoover/Desktop/r-novice-inflammation")
dev.off()

pdf("inflammation-01.pdf")
analyze("data/inflammation-01.csv")
dev.off()

num <- 37
num > 100
num < 100

num <- 101
if (num > 100) {
  print("greater")
} else {
  print("not greater")
}
print("done")

sign <- function(num) {
  if (num > 0) {
    return(1)
  } else if (num == 0) {
    return(0)
  } else {
    return(-1)
  }
  }
sign(1)
sign(0)
sign(-4)
sign(20)

if (2 > 0 && 1 > 0) {
  print("both parts are true")
} else {
  print("at lease one part is not true")
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10)     # day (column) 10
plot_dist(dat[1:5, 10], threshold = 10)  # samples (rows) 1-5 on day (column) 10

plot_dist <- function(x, threshold) {
  if (length(x) > threshold) {
    boxplot(x)
  } else {
    stripchart(x)
  }
}

plot_dist <- function(x, threshold, use_boxplot = TRUE) {
  if (length(x) > threshold && use_boxplot) {
    boxplot(x)
  } else if (length(x) > threshold && !use_boxplot) {
    hist(x)
  } else {
    stripchart(x)
  }
}

dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10, use_boxplot = TRUE)   # day (column) 10 - create boxplot
dev.off()

dev.off()
print(plot(1)) # Basically use print command once

filenames <- list.files(path = "data", pattern = "inflammation-[0-9]{2}.csv", full.names = TRUE)
filename_max <- "" # filename where the maximum average inflammation patient is found
patient_max <- 0 # index (row number) for this patient in this file
average_inf_max <- 0 # value of the average inflammation score for this patient
for (f in filenames) {
  dat <- read.csv(file = f, header = FALSE)
  dat.means <- apply(dat, 1, mean)
  for (patient_index in 1:length(dat.means)){
    patient_average_inf <- dat.means[patient_index]
    # Add your code here ...
    if (patient_average_inf > average_inf_max) {
      average_inf_max <- patient_average_inf
      filename_max <- f
      patient_max <- patient_index
    }
  }
}
print(filename_max)
print(patient_max)
print(average_inf_max)

analyze <- function(filename, output = NULL) {
  # Plots the average, min, and max inflammation over time.
  # Input:
  #    filename: character string of a csv file
  #    output: character string of pdf file for saving
  if (!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  if (!is.null(output)) {
    dev.off()
  }
}

analyze("data/inflammation-01.csv")
analyze("data/inflammation-01.csv", output = "inflammation-01.pdf")

f <- "inflammation-01.csv"
sub("csv", "pdf", f)
dir.create("results")
analyze("data/inflammation-01.csv", output = "results/inflammation-01.pdf")
file.path("results", sub("csv", "pdf", f))
?sub

analyze_all <- function(pattern) {
  # Directory name containing the data
  data_dir <- "data"
  # Directory name for results
  results_dir <- "results"
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = data_dir, pattern = pattern)
  for (f in filenames) {
    pdf_name <- file.path(results_dir, sub("csv", "pdf", f))
    analyze(file.path(data_dir, f), output = pdf_name)
  }
}
analyze_all("inflammation.*csv")
analyze <- function(filename, output = NULL) {
  # Plots the average, min, and max inflammation over time.
  # Input:
  #    filename: character string of a csv file
  #    output: character string of pdf file for saving
  if (!is.null(output)) {
    pdf(output)
  }
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation, type = "l")
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation, type = "l")
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation, type = "l")
  if (!is.null(output)) {
    dev.off()
  }
}

Today is Wednesday. 

