myFunction <- function(roads, car, packages) {
  car$nextMove <- aStar(c(car$x, car$y), roads, packages)
  return(car)
}


aStar <- function(carLoc, roads, packages) {
  "Executes the A* algorithm.
  
  Finds the path from the current location to the goal.
  Returns only the immediate next move from found path."

  goal <- getGoal(carLoc, packages)

  frontier <- list()
  currentNode <- list(loc = carLoc, f = NULL, g = 0, history = list())
  examined <- list()
  examined[[1]] <- currentNode$loc

  while (TRUE) {
    frontier <- addNeighbours(currentNode, frontier, examined, goal, roads, iter)

    if (length(frontier) < 1) {
      return(5)
    }

    bestNodeI <- getBestNode(frontier)

    if (identical(frontier[[bestNodeI]]$loc, goal)) {  # If goal has been found.
      if (length(frontier[[bestNodeI]]$history) == 1) {
        nextMove <- getNextMove(carLoc, goal)
      } else {
        nextMove <- getNextMove(carLoc, frontier[[bestNodeI]]$history[[2]])
      }

      if (nextMove[2] == 1) {return(2)} # down
      else if (nextMove[1] == 1) {return(4)} # left
      else if (nextMove[1] == -1) {return(6)} # right
      else if (nextMove[2] == -1) {return(8)} # up
      else {return(5)}
      }

    else {  # If goal has not been found.
      currentNode <- frontier[[bestNodeI]]
      examined[[length(examined) + 1]] <- frontier[[bestNodeI]]$loc
      frontier <- frontier[-bestNodeI]
    }
  }
}


getF <- function(currentLoc, prevG, nextLoc, goal, roads) {
  "Calculates the next node's total cost f as g + h.

  Where h is the manhattan distance from the next node to the goal
  and g is the cost of going from the current node to the 
  next node + the current node's accumulated cost g. 
  
  Returns f and g as vector."

  g <- getCost(currentLoc, nextLoc, roads) + prevG
  h <- manhattan(nextLoc, goal)
  f <- sum(g, h)
  return(c(f, g))
}


getCost <- function(a, b, roads) {
  "Gets the cost of the next part of the road that is going to be traversed."

  if (a[1] - b[1] != 0) {
    return(roads$hroads[min(a[1], b[1]), a[2]])
  } else if (a[2] - b[2] != 0) {
    return(roads$vroads[a[1], min(a[2], b[2])])
  }
}


manhattan <- function(a, b) {
  "Calculates the manhattan distance between nodes a and b."

  return(abs(a[1] - b[1]) + abs(a[2] - b[2]))
}


addNeighbours <- function(currentNode, frontier, examined, goal, roads, iter) {
  "Gets the neighbouring nodes of the current node being expanded."

  # Retrieves all neighbours within the bounds of the matrix.
  neighbours <- vector("list", length = 4)
  neighbours[[1]] <- c(max(1, currentNode$loc[1] - 1), currentNode$loc[2])
  neighbours[[2]] <- c(currentNode$loc[1], max(1, currentNode$loc[2] - 1))
  neighbours[[3]] <- c(min(10, currentNode$loc[1] + 1), currentNode$loc[2])
  neighbours[[4]] <- c(currentNode$loc[1], min(10, currentNode$loc[2] + 1))

  # Filters out any neighbours that are duplicates of current node.
  neighbours <- Filter(function(x) !identical(x, currentNode$loc), neighbours)

  # Filters out any neighbours that have already been examined
  # or that are in frontier.
  neighbours <- neighbours[!(neighbours %in% examined)]
  neighbours <- neighbours[!(neighbours %in% lapply(frontier, \(x) x$loc))]

  # Adds the new neighbours to the frontier, if there are any new neighbours.
  if (length(neighbours) > 0) {
    for (i in 1:length(neighbours)) {
      newValue <- list(loc = neighbours[[i]], f = NULL, g = NULL,
                       history = currentNode$history)
      newValue$history[[length(newValue$history) + 1]] <- currentNode$loc
      fgValues <- getF(currentNode$loc, currentNode$g, newValue$loc, goal, roads)
      newValue$f <- fgValues[1]
      newValue$g <- fgValues[2]
      frontier[[length(frontier) + 1]] <- newValue
    }
  }
  return(frontier)
}


getNextMove <- function(a, b) {
  "Compares the location of the car and the first step of path to get nextMove."
  return(c(a[1] - b[1], a[2] - b[2]))
}


getGoal <- function(start, packages) {
  "Finds and returns the current goal.
  
  First checks in case any package is picked up (col 5 = 1),
  in which case returns that package's drop off location as goal.
  Second checks which packages are not picked up (col 5 = 0),
  in which case returns the closest package as goal."

  if (any(packages[, 5] == 1)) {
    dropoff <- packages[which(packages[, 5] == 1, arr.ind = TRUE), 3:4]
    return(dropoff)
  } else {
    pickups <- packages[which(packages[, 5] == 0), 1:2, drop = FALSE]
    distances <- apply(pickups, 1, manhattan, start)
    goal <- pickups[which.min(distances), ]
    return(goal)
  }
}


getBestNode <- function(frontier) {
  "Returns the index of the node in frontier with the best (lowest) value f."

  fValues  <- sapply(frontier, \(x) x$f)
  bestI <- which.min(fValues)
  return(bestI)
}