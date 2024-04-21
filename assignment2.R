library(WheresCroc)


myFunction <- function(info, readings, tourists, water_holes, mean_sd_readings) {
  info <- get_next_move(info, readings, tourists, water_holes, mean_sd_readings)
  return(info)
}


get_next_move <- function(info, readings, tourists, water_holes, mean_sd) {
  "Gets the next move based on the probabilities."

  emission <- get_emission(readings, mean_sd)
  if (info$mem$status != 2) {
    info$mem$states <- emission
    info$mem$status <- 2
  } else {
    info$mem$states <- update_states(info$mem$states, water_holes,
                                     emission)
  }

  info$mem$states[abs(tourists[which(tourists < 0)])] <- 1
  croc_loc <- which.max(info$mem$states)
  info$mem$croc_loc <- croc_loc
  path <- get_path(tourists[3], croc_loc, water_holes)

  if (length(path) == 2) {  # If one hole away from croc
    info$moves <- c(path[[2]], 0)
    info$mem$searched <- append(info$mem$searched, path[[2]])
  } else if (length(path) == 1) {  # If at same hole as croc
    info$moves <- c(0, 0)
    info$mem$searched <- append(info$mem$searched, path[[1]])
  } else {  # If more than one hole away from croc
    info$moves <- c(path[[2]], path[[3]])
  }
  return(info)
}


update_states <- function(states, water_holes, emissions) {
  "Updates the states' probabilities.
  
  Calculates the probability of being in a state as the sum of the 
  initial state p * transition p * emission p for all surrounding states."

  new_states <- rep(0, 40)
  for (i in 1:40) {
    transitions <- c(i, water_holes[which(i == water_holes[, 1]), 2],
                        water_holes[which(i == water_holes[, 2]), 1])
    emission_p <- emissions[i]
    new_p <- c()
    for (trans in transitions) {
      state_p <- states[trans]
      trans_p <- 1 / length(transitions)
      new_p <- append(new_p, state_p * trans_p * emission_p)
    }
    new_states[i] <- sum(new_p)
  }
  return(new_states)
}


get_path <- function(start, goal, nodes) {
  "Uses breadth-first search to get the shortest path.
  
  Based on this Stackoverflow answer:
  https://stackoverflow.com/questions/8922060/how-to-trace-the-path-in-a-breadth-first-search"

  q <- list(list(start))
  while (length(q) > 0) {
    path <- q[[1]]
    q <- q[-1]
    node <- unlist(path[[length(path)]])

    if (node == goal) {
      return(path)
    }

    for (neighbour in c(nodes[which(node == nodes[, 1]), 2],
                        nodes[which(node == nodes[, 2]), 1])) {
      if (!neighbour %in% path) {
        new_path <- path
        new_path <- append(new_path, neighbour)
        q[[length(q) + 1]] <- new_path
      }
    }
  }
}


get_emission <- function(readings, mean_sd) {
  emissions <- c()
  for (i in 1:40) {
    emissions <- append(emissions, get_emission_prob(i, readings, mean_sd))
  }
  return(emissions)
}


get_emission_prob <- function(hole, x, mean_sd) {
  "Returns the emission probability of a waterhole.
  
  Calculates the emission probability as the product of the max pdf"

  p <- dnorm(x[1], mean_sd$salinity[, 1][hole], mean_sd$salinity[, 2][hole]) *
       dnorm(x[2], mean_sd$phosphate[, 1][hole], mean_sd$phosphate[, 2][hole]) *
       dnorm(x[3], mean_sd$nitrogen[, 1][hole], mean_sd$nitrogen[, 2][hole])
  return(p)
}