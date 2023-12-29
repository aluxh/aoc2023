library(dplyr)
library(tidyr)

# Day 2: Cube Conundrum ------------------------------

x <- c("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", 
"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

day2 <- readLines("input/day2.txt")

# Notes -----
# Day 2 becomes easier if we can create a dataframe from the text input. 
# We can do this by splitting the text into a list of games and color cubes.
# Then, using `lapply` and `data.frame` to make each game into a dataframe.
# Finally, `do.call(rbind, games)` to combine all the games into one dataframe.

create_df <- function(games_list) {
    
    games <- strsplit(games_list, ":\\s|;\\s|,\\s")
    
    games <- lapply(games, function(x) {
        
        data.frame(
            game = strsplit(x[1], "\\s")[[1]][2] |> as.numeric(),
            color = lapply(x[-1], function(y) strsplit(y, "\\s")[[1]][2]) |> unlist(),
            number = lapply(x[-1], function(y) strsplit(y, "\\s")[[1]][1]) |> as.numeric() |> unlist()
        )
        
    }) 
    
    do.call(rbind, games)
    
}

# Part 1 --------------------------------------------

total_id <- function(games_list, red, green, blue) {
    
    red_expr <- enquo(red)
    green_expr <- enquo(green)
    blue_expr <- enquo(blue)
    
    games_df <- create_df(games_list)
    
    games_df |> 
        group_by(game, color) |>
        filter(number == max(number)) |> 
        ungroup() |> 
        distinct() |> 
        pivot_wider(names_from = color, values_from = number) |>
        filter(
            red <= !!red_expr,
            green <= !!green_expr,
            blue <= !!blue_expr
        ) |>
        summarise(result = sum(game))
    
}

total_id(day2, 12, 13, 14)

# Part 2 --------------------------------------------

power_id <- function(games_list) {
    
    games_df <- create_df(games_list)
    
    games_df |> 
        group_by(game, color) |> 
        filter(number == max(number)) |> 
        ungroup() |> 
        distinct() |> 
        group_by(game) |> 
        summarise(power = prod(number), .groups = "drop") |>
        summarise(result = sum(power))

}

power_id(day2)











