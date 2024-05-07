suits <- rep(1:4, each=13)
ranks <- rep(1:13, 4)
ranks_face <- rep(c('2','3','4','5','6','7','8','9','10','jack','queen','king','ace'),4)
suits_face <- rep(c('hearts','diamonds','spades','clubs'),each=13)
# The cards are labeled 1-52, in the order of 2-3-...-10-J-Q-K-A for the 4 suits
# In straights/straight flush, a rank of 13-1-2-3-4 is allowed as the "smallest" straight
is_strFlush <- function(cards){
  # Check if the 7 cards can form a straight flush
  # If so, return the rank of the best straight-flush hand
  # Otherwise, return -1
  for(i in 13:4){ # Iterate over starting rank, in order of 10,9,...,2,A
    straight <- i:(i-4)
    if(i==4) straight[5] <- 13 # Treat A-2-3-4-5 specially
    for(j in 0:3){ # Iterate over suits, no preference
      nums <- straight + j*13
      if(all(nums %in% cards)) return(straight)
    }
  }
  return(-1)
}
is_four <- function(cards){
  # Check if the 7 cards can form a Four-of-a-kind
  # If so, return the best 4-of-a-kind hand
  # Otherwise, return -1
  for(i in 13:1){ 
    # Iterate over rank of the 4-group, preferring higher ones
    nums <- i + 13*(0:3)
    if(all(nums %in% cards)) {
      singletons <- setdiff(cards, nums) # Pick a best card from the remaining 3
      return(c(i, i, i, i, max(ranks[singletons])))
    }
  }
  return(-1)
}
is_fullHouse <- function(cards){
  # Check if the 7 cards can form a Full-House
  # If so, return the best full-house hand
  # Otherwise, return -1
  r <- ranks[cards]
  for(i in 13:1){ # Iterate over the 3-group
    if(sum(r==i)>=3)
      for(j in 13:1){ # Iterate over the pair
        if(j==i) next
        if(sum(r==j)>=2)
          return(c(i,i,i,j,j))
      }
    else next
  }
  return(-1)
}
is_flush <- function(cards){
  # Check if the 7 cards can form a flush
  # If so, return the best hand that forms a flush, in decreasing order
  # Otherwise, return -1
  s <- suits[cards]
  for(i in 1:4){ # Iterate over suits
    # Since 7 cards cannot contain 2 flushes with different suits
    # If a flush is found, we can choose the highest cards in the same suit and return it
    if(sum(s==i)>=5){
      flush_ranks <- ranks[cards[s==i]]
      return(sort(flush_ranks,decreasing=TRUE)[1:5])
    }
  }
  return(-1)
}
is_straight <- function(cards){
  # Check if the 7 cards can form a straight
  # If so, return the best straight hand
  # Otherwise, return -1
  r <- ranks[cards]
  for(i in 13:4){ # Iterate over starting rank, in order of 10,9,...,2,A
    straight <- i:(i-4)
    if(i==0) straight[5] <- 13 # Treat A-2-3-4-5 specially
    if(all(straight %in% r)) return(straight)
  }
  return(-1)
}
is_three <- function(cards){
  # Check if the 7 cards can form a Three-of-a-kind
  # If so, return the best three-of-a-kind hand
  # Otherwise, return -1
  r <- ranks[cards]
  for(i in 13:1){ # Iterate over the 3-group
    if(sum(r==i)==3){
      return(c(i,i,i, sort(r[r!=i], decreasing = TRUE)[1:2]))
    }
  }
  return(-1)
}
is_twoPair <- function(cards){
  # Check if the 7 cards can form a Two-Pair
  # If so, return the best two-pair hand 
  # Otherwise, return -1
  r <- ranks[cards]
  for(i in 13:2){ # Iterate over the larger pair
    if(sum(r==i)==2)
      for(j in (i-1):1){ # Iterate over the smaller pair
        if(sum(r==j)==2){
          singleton <- r[(r!=i)&(r!=j)]
          return(c(i,i,j,j,max(singleton)))
        }
      }
    else next
  }
  return(-1)
}
is_pair <- function(cards){
  # Check if the 7 cards can form a Pair
  # If so, return the best Pair hand 
  # Otherwise, return -1
  r <- ranks[cards]
  for(i in 13:1){ # Iterate over the 3-group
    if(sum(r==i)==2){
      return(c(i,i, sort(r[r!=i], decreasing = TRUE)[1:3]))
    }
  }
  return(-1)
}
is_high_card <- function(cards){
  # Return the best High-Card
  r <- ranks[cards]
  return(sort(r, decreasing=TRUE)[1:5])
}
find_best_hand <- function(cards){
  pattern_list <- list(is_high_card, is_pair, is_twoPair,
                       is_three, is_straight, is_flush,
                       is_fullHouse, is_four, is_strFlush)
  pattern_name <- c("High Card", "Pair", "Two Pairs",
                    "Three-of-a-kind", "Straight", "Flush",
                    "Full House", "Four-of-a-kind", "Straight Flush")
  for(pattern in 9:1){
    match_pattern <- pattern_list[[pattern]](cards)
    if(!identical(match_pattern, -1))
      return(list(type=pattern_name[pattern],pattern=pattern, hand=match_pattern))
  }
}
compare_hands <- function(hand1, hand2){
  # Compare two hand in dictionary order
  # i.e. compare the first card, compare the second one if the first has the same rank, etc.
  # Return TRUE if hand1 is superior than or equal to hand2(in rank only, we do not consider hand patterns)
  for(i in 1:5){
    if(hand1[i]>hand2[i]) return(TRUE)
    if(hand1[i]<hand2[i]) return(FALSE)
  }
  return(TRUE)
}

hand_probs <- function(private=numeric(0),
                       common=numeric(0),
                       MC_size=10000L,
                       seed=NULL){
  known <- c(private, common)
  stopifnot(anyDuplicated(known)==0)
  stopifnot(setequal(intersect(known, 1:52),known))
  stopifnot(length(private)%in% c(0,2)&length(common)<=5)
  remains <- setdiff(1:52, known)
  if(!is.null(seed)) set.seed(seed)
  hand_types <- rep(0, 9)
  names(hand_types) <- c("High Card", "Pair", "Two Pairs",
                         "Three-of-a-kind", "Straight", "Flush",
                         "Full House", "Four-of-a-kind", "Straight Flush")
  if(length(known)==7){
    best <- find_best_hand(h)
    hand_types[best$pattern] <- hand_types[best$pattern] + 1
    return(hand_types)
  }
  for(iter in 1:MC_size){
    h <- c(known,sample(remains, 7-length(known)))
    best <- find_best_hand(h)
    hand_types[best$pattern] <- hand_types[best$pattern] + 1
  }
  prob <- hand_types/MC_size
  return(prob)
}

simulate_game <- function(N=5, # Number of players
                          private=numeric(0), # Known private card for one player
                          common=numeric(0), # Revealed common cards
                          MC_size=1e4){
  known <- c(private, common)
  stopifnot(anyDuplicated(known)==0)
  stopifnot(setequal(intersect(known, 1:52),known))
  stopifnot(length(private)%in% c(0,2)&length(common)<=5)
  remains <- setdiff(1:52, known)
  prob_self <- rep(0, 9)
  prob_rival <- rep(0, 9)
  names(prob_self) <- c("High Card", "Pair", "Two Pairs",
                         "Three-of-a-kind", "Straight", "Flush",
                         "Full House", "Four-of-a-kind", "Straight Flush")
  names(prob_rival) <- names(prob_self)
  champions <- 0
  for(iter in 1:MC_size){
  is_champion <- 1
  dealt <- sample(remains, 2*N+5-length(known), replace=FALSE)
  if(length(private)==0){
    private0 <- dealt[1:2]
    dealt <- dealt[-(1:2)]
  }
  else{
    private0 <- private
  }
  if(length(common)<5){
    common0 <- c(common, dealt[1:(5-length(common))])
    dealt <- dealt[-(1:(5-length(common)))]
  }
  else{
    common0 <- common
  }
  hands <- list()
  hands[[1]] <- find_best_hand(c(private0, common0))
  for(j in 2:N)
    hands[[j]] <- find_best_hand(c(dealt[(2*j-3):(2*j-2)],common0))
  prob_self[hands[[1]]$pattern] <- prob_self[hands[[1]]$pattern] + 1
  prob_rival[hands[[2]]$pattern] <- prob_rival[hands[[2]]$pattern] + 1
  for(j in 2:N){
    is_champion <- is_champion * (
      (hands[[1]]$pattern>hands[[j]]$pattern) |
      ((hands[[1]]$pattern==hands[[j]]$pattern)&
         (compare_hands(hands[[1]]$hand,hands[[j]]$hand)))
      )
    if(!is_champion) break
  }
  champions <- champions + is_champion
  }
  prob_self <- prob_self / MC_size
  prob_rival <- prob_rival / MC_size
  prob_win <- champions / MC_size
  return(list(self=prob_self,rival=prob_rival,prob_win=prob_win))
}
simulate_game2 <- function(N=5, # Number of players
                           cards=rep(NA, 2*N+5), # Revealed common cards
                           MC_size=1e4){
  known <- cards[!is.na(cards)]
  stopifnot(anyDuplicated(known)==0)
  stopifnot(setequal(intersect(known, 1:52),known))
  stopifnot(length(cards)==2*N+5)
  remains <- setdiff(1:52, known)
  prob_hands <- matrix(0, nrow=N, ncol=9)
  colnames(prob_hands) <- c("High Card", "Pair", "Two Pairs",
                            "Three-of-a-kind", "Straight", "Flush",
                            "Full House", "Four-of-a-kind", "Straight Flush")
  
  private0 <- matrix(cards[1:(2*N)],nrow=N,ncol=2,byrow=TRUE)
  common0 <- cards[(2*N+1):(2*N+5)]
  champions <- rep(0, N)
  for(iter in 1:MC_size){
    ranks <- rep(0, N)
    dealt <- sample(remains, 2*N+5-length(known), replace=FALSE)
    private <- private0
    common <- common0
    # Sample the unknown private cards, if any, for each player
    for(j in 1:N){
      if(sum(!is.na(private[j,]))<2){
        private[j,is.na(private[j,])] <- dealt[1:(2-sum(!is.na(private[j,])))]
        dealt <- dealt[-(1:(2-sum(!is.na(private0[j,]))))]
      }
    }
    # Sample the unknown common cards, if any
    if(sum(!is.na(common))<5){
      common[is.na(common)] <- dealt[1:(5-sum(!is.na(common)))]
      dealt <- dealt[-(1:(5-sum(!is.na(common0))))]
    }
    # Find the best hand type of each player
    hands <- list()
    for(j in 1:N){
      hands[[j]] <- find_best_hand(c(private[j,],common))
      prob_hands[j,hands[[j]]$pattern] <- prob_hands[j,hands[[j]]$pattern] + 1
    }
    # Determine the winner
    # We count, for each player, the number of rivals they beat/draw
    for(j in 1:(N-1))
      for(k in (j+1):N){
        ranks[j] <- ranks[j] +
          ((hands[[j]]$pattern>hands[[k]]$pattern) |
             ((hands[[j]]$pattern==hands[[k]]$pattern)&
                (compare_hands(hands[[j]]$hand,hands[[k]]$hand))))
        ranks[k] <- ranks[k] +
          ((hands[[k]]$pattern>hands[[j]]$pattern) |
             ((hands[[k]]$pattern==hands[[j]]$pattern)&
                (compare_hands(hands[[k]]$hand,hands[[j]]$hand))))
      }
    champions[ranks==N-1] <- champions[ranks==N-1] + 1
  }
  prob_hands <- prob_hands / MC_size
  prob_win <- champions / MC_size
  return(list(prob_hands=prob_hands,prob_win=prob_win))
}

