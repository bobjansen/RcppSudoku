## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ---- echo = TRUE--------------------------------------------------------
sudokuTxt <- "
1 0 0  0 0 0  0 0 6
0 0 6  0 2 0  7 0 0
7 8 9  4 5 0  1 0 3
0 0 0  8 0 7  0 0 4
0 0 0  0 3 0  0 0 0
0 9 0  0 0 4  2 0 1
3 1 2  9 7 0  0 4 0
0 4 0  0 1 2  0 7 8
9 0 8  0 0 0  0 0 0"

## ---- echo = TRUE--------------------------------------------------------
sudoku <- as.matrix(
  read.table(text = sudokuTxt, 
             col.names = letters[1:9]))

## ---- echo = FALSE-------------------------------------------------------
library(zeallot)
library(magrittr)

## ---- echo = TRUE--------------------------------------------------------
solve <- function(partialSolution) {
  # Eliminate impossible values, give some suggestions
  # and flag contradictions.
  c(partialSolution, suggestions, contradiction) %<-% 
    eliminate(partialSolution)
  # If dead end FALSE to trace back, if finshed TRUE.
  if (contradiction) return(list(FALSE, NULL))
  if (all(partialSolution %in% 1:9))
    return(list(TRUE, partialSolution))
  # Branching, exit when the solution is found.
  for (suggestion in suggestions) {
    c(result, solution) %<-% solve(suggestion)
    if (result) return(list(result, solution))
  }
  list(FALSE, NULL)
}

## ---- echo = TRUE--------------------------------------------------------
eliminate <- function(grid) {
  suggestions <- 0:9
  for (i in 1:nrow(grid)) { for (j in 1:ncol(grid)) {
    if (grid[i, j] == 0L) {
      choices <- findChoicesCpp(grid, i, j)
      if (length(choices) == 0L) {
        return(list(NULL, NULL, TRUE))
      } else if (length(choices) == 1L) {
        grid[i, j] <- choices
        return(list(grid, list(grid), FALSE))
      } else
        suggestions <- updateSuggestions(
          choices, grid, i, j, suggestions)
    }
  }}
  list(grid, suggestions, FALSE)
}

## ---- echo = TRUE--------------------------------------------------------
# Find the choices allowed by the rules.
findChoices <- function(grid, i, j)
  1:9 %>% setdiff(grid[i, ]) %>%
    setdiff(grid[ , j]) %>%
    setdiff(grid[i - (i - 1) %% 3L + 0:2, 
                 j - (j - 1) %% 3L + 0:2])
findChoices2 <- function(grid, i, j)
  setdiff(
    setdiff(setdiff(1:9, 
                    grid[i, ]), 
            grid[ , j]),
    grid[i - (i - 1) %% 3L + 0:2, 
         j - (j - 1) %% 3L + 0:2])
# Create a new grid with suggested next moves.
updateSuggestions <- function(choices, grid, i, j,
                              lastBest) {
  if (length(choices) < length(lastBest))
    lapply(choices, function(choice) {
      grid[i, j] <- choice; grid
    })
  else 
    lastBest
}

## ---- echo = TRUE--------------------------------------------------------
# solution <- solve(sudoku)
# if (!solution[[1]]) { cat('Solution not found\n')
# } else { print(as.data.frame(solution[[2]])) }

## ---- echo = TRUE--------------------------------------------------------
sudokuTxt <- "
8 0 0  0 0 0  0 0 0
0 0 3  6 0 0  0 0 0
0 7 0  0 9 0  2 0 0
0 5 0  0 0 7  0 0 0
0 0 0  0 4 5  7 0 0
0 0 0  1 0 0  0 3 0
0 0 1  0 0 0  0 6 8
0 0 8  5 0 0  0 1 0
0 9 0  0 0 0  4 0 0"
sudoku <- as.matrix(
  read.table(text = sudokuTxt, 
             col.names = letters[1:9]))

## ---- echo = TRUE--------------------------------------------------------
# solution <- solve(sudoku)
# if (!solution[[1]]) { cat('Solution not found\n')
# } else { print(as.data.frame(solution[[2]])) }

