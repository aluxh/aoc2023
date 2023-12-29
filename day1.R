# Day 1: Trebuchet?!
input <- readLines("input/day1.txt")

# Part 1
retrieve_num <- function(x) {
    pos <- grep("[0-9]", x)
    num <- paste0(x[pos[1]], x[pos[length(pos)]]) |> 
        as.numeric()
    
    num
}

strsplit(input, "") |> 
    lapply(retrieve_num) |> 
    unlist() |> 
    sum()

# Part 2
# For "two1nine", the text will be split into c("two", "1", "nine")
# For "eightwothree", the text will be split into c("eight", "two", "three")
# For "abcone2threexyz", the text will be split into c("abc", "one", "2", "three", "xyz")
# For "xtwone3four", the text will be split into c("x", "two", "one", "3", "four")
# For "4nineeightseven2", the text will be split into c("4", "nine", "eight", "seven", "2")

x <- c("two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen")

extract_text <- function(text, pattern) {
    
    pos <- regexec(pattern, text)
    len <- attr(pos[[1]], "match.length")
    
    res <- substr(text, pos[[1]][2], pos[[1]][2] + len[2] - 1)
    
    return(res)
}

text_to_num <- function(text) {
    for(i in seq_along(text)) {
        if (text[i] == "one") {
            text[i] <- "1"
        } else if (text[i] == "two") {
            text[i] <- "2"
        } else if (text[i] == "three") {
            text[i] <- "3"
        } else if (text[i] == "four") {
            text[i] <- "4"
        } else if (text[i] == "five") {
            text[i] <- "5"
        } else if (text[i] == "six") {
            text[i] <- "6"
        } else if (text[i] == "seven") {
            text[i] <- "7"
        } else if (text[i] == "eight") {
            text[i] <- "8"
        } else if (text[i] == "nine") {
            text[i] <- "9"
        } else {
            text[i] <- text[i]
        }
    }
    text
}

retrieve_num <- function(text, pattern) {
    
    first_pattern <- paste0("(", pattern, ")")
    last_pattern <- paste0(".*(", pattern, ")")
    
    first <- extract_text(text, first_pattern) |> 
        text_to_num()
    
    last <- extract_text(text, last_pattern) |>
        text_to_num()
    
    res <- paste0(first, last) |> 
        as.numeric()
    
    return(res)
    
}

lapply(input, retrieve_num, pattern) |> 
    unlist() |> 
    sum()













