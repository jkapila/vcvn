#'Voting Win method for variable selection and importance
#'
#'@param variableWeightageMatrix A matrix of variables and there scores
#'  based on methods
#'@param candidateColumn The column which should be treated as column
#'  for candidates. If NULL first clumn will be treated as candidate column.
#'@param isRanked A logical. Is the matrix supplied with ranks or raw scores?
#'
#'@return A Ranked data frame with average rank scores
#'
#'@examples
#'namedVotes <- read.table(header = T,text = " Candidates Vote_A Vote_B Vote_C Vote_D Vote_E Vote_F Vote_G Vote_H
#'  Albert      1      2      1      3      2      1      3      4
#'  Bruce      2      4      5      4      5      4      1      2
#'  Charles      3      1      3      1      1      2      4      5
#'  David      4      5      2      5      4      5      2      1
#'  Edward      5      3      4      2      3      3      5      3")
#'
#'varVoting(namedVotes,isranked = TRUE)

varVoting <- function(variableWeightageMatrix,candidateColumn = NULL,
                      isRanked = FALSE){

  if (is.null(candidateColumn)){
    colnames(variableWeightageMatrix)[1] <- "Candidates"
  }else {
    variableWeightageMatrix <- variableWeightageMatrix[,
                            c(candidateColumn,colnames(variableWeightageMatrix)[
                              colnames(variableWeightageMatrix) != candidateColumn])]
    colnames(variableWeightageMatrix)[1] <- "Candidates"
    }

  if(!isRanked){
    variableWeightageMatrix <- MakeRankedMatrix(variableWeightageMatrix,
                                                candidateColumn = candidateColumn)
  }

  ### Defining Voting functions

  # Average ranking
  AvgRank <- function(BallotMatrix) {
    Ballots <- as.matrix(BallotMatrix[, -1], mode = "numeric")
    Num_Candidates <- dim(Ballots)[1]
    Names <- BallotMatrix[, 1]
    Ballots[is.na(Ballots)] <- Num_Candidates + 1  #Treat blanks as one worse than min
    MeanRanks <- rowMeans(Ballots)
    Rankings <- data.frame(Names, MeanRanks)
    Rankings <- Rankings[order(rank(Rankings[, 2], ties.method = "random")), ]  #Ties handled through random draw
    Rankings <- data.frame(Rankings, seq_along(Rankings[, 1]))
    names(Rankings) <- c("Names", "Average Rank", "Position")
    return(Rankings)
  }

  # Extra Voting
  VoteExtract <- function(BallotMatrix) {
    Votes <- as.matrix(BallotMatrix[, -1], mode = "numeric")
    Num_Candidates <- dim(Votes)[1]
    Votes[is.na(Votes)] <- Num_Candidates + 1  #Treat blanks as one worse than min
    return(Votes)
  }

  # Pair Counts
  PairCount <- function(Votes) {
    Num_Candidates <- dim(Votes)[1]
    Pairwise <- matrix(nrow = Num_Candidates, ncol = Num_Candidates)
    for (CurCand in 1:Num_Candidates) {
      CandRank <- as.vector(as.matrix(Votes[CurCand, ]))
      Pref_Cur_Cand <- t(Votes) - CandRank
      for (Pairs in 1:Num_Candidates) {
        Pairwise[CurCand, Pairs] <- sum(Pref_Cur_Cand[, Pairs] > 0)
      }
    }
    return(Pairwise)
  }

  # Schulze Algorithm
  Schulze <- function(PairsMatrix) {
    size <- dim(PairsMatrix)[1]
    p <- matrix(nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        if (i != j) {
          if (PairsMatrix[i, j] > PairsMatrix[j, i]) {
            p[i, j] <- PairsMatrix[i, j]
          } else {
            p[i, j] <- 0
          }
        }
      }
    }
    for (i in 1:size) {
      for (j in 1:size) {
        if (i != j) {
          for (k in 1:size) {
            if (i != k && j != k) {
              p[j, k] <- max(p[j, k], min(p[j, i], p[i, k]))
            }
          }
        }
      }
    }
    diag(p) <- 0
    return(p)
  }

  # Condocert Rankings
  CondorcetRank <- function(BallotMatrix) {
    Num_Candidates <- dim(BallotMatrix)[1]
    Rankings <- matrix(nrow = Num_Candidates, ncol = 3)
    CurrentBallot <- BallotMatrix
    CurrentRank <- 1
    while (CurrentRank <= Num_Candidates) {
      CurrentNames <- as.vector(CurrentBallot[, 1])
      CurrentSize <- length(CurrentNames)
      CurrentVotes <- VoteExtract(CurrentBallot)
      Pairwise <- matrix(nrow = CurrentSize, ncol = CurrentSize)
      Pairwise <- PairCount(CurrentVotes)
      Winner <- vector(length = CurrentSize)

      # Check for Condorcet Winner

      for (i in 1:CurrentSize) {
        Winner[i] <- sum(Pairwise[i, ] > Pairwise[, i]) == (CurrentSize -
                                                              1)
      }
      if (sum(Winner == TRUE) == 1) {
        # Condorcet Winner Exists
        CurrentWinner <- which(Winner == TRUE)
        Rankings[CurrentRank, ] <- c(CurrentNames[CurrentWinner], CurrentRank,
                                     "Condorcet")
      } else {

        # Condorcet Winner does not exist, calculate Schulze beatpaths

        Pairwise <- Schulze(Pairwise)
        for (i in 1:CurrentSize) {
          Winner[i] <- sum(Pairwise[i, ] > Pairwise[, i]) == (CurrentSize -
                                                                1)
        }
        if (sum(Winner == TRUE) == 1) {
          # Schwartz set has unique member
          CurrentWinner <- which(Winner == TRUE)
          Rankings[CurrentRank, ] <- c(CurrentNames[CurrentWinner], CurrentRank,
                                       "Schulze")
        }
      }
      CurrentBallot <- CurrentBallot[-CurrentWinner, ]
      CurrentRank = CurrentRank + 1
    }
    Rankings <- data.frame(Rankings)
    names(Rankings) <- c("Name", "Rank", "Method")
    return(Rankings)
  }

  avgRanks <- AvgRank(variableWeightageMatrix)
  rankedVar <- CondorcetRank(variableWeightageMatrix)

  rankedVar <- merge(rankedVar,avgRanks[,c(1,2)],by = "Name")
  return(rankedVar)

}
