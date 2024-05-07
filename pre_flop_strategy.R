same_suit <- matrix(0, 13, 13)
diff_suit <- matrix(0, 13, 13)
rownames(same_suit) <- colnames(same_suit) <- 
  rownames(diff_suit) <- colnames(diff_suit) <- ranks_face[1:13]
for(i in 1:13){
  for(j in (i):13){
    if(i==j) same_suit[i,j] <- NA
    else{
      same_suit[i,j] <- simulate_game(N=5,private=c(i,j))$prob_win
      same_suit[j,i] <- same_suit[i,j]
    }
  }
}
for(i in 1:13){
  for(j in (i):13){
    diff_suit[i,j] <- simulate_game(N=5,private=c(i,j+13))$prob_win
    diff_suit[j,i] <- diff_suit[i,j]
  }
}
save(same_suit, diff_suit, file='Starting strategy.rda')