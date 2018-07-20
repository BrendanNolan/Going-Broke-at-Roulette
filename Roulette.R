# There is a well-known strategy for winning at roulette. 

# The idea is to put one unit of cash (let's say one dollar) 
# on either red or black and proceed as follows:
# every time you win, your next bet should be one dollar,
# every time you lose, your next bet should be double your previous bet
# plus one dollar. This way, every time you win, your total earnings will be 
# equal to the number of wheel spins that you have bet on. 
# Obviously this strategy has no hope of winning in the long run 
# because you are playing a game where the odds are stacked against you. 
# This program is designed to let you play this game, which you can do with 
# the play() function. This function has a print method which tells you 
# which colour the ball landed on, what you won, and what your next bet should 
# be. If you get sick of playing and keeping track of your dwindling wallet 
# (it may not dwindle in the short term but I guarantee you that it will dwindle
# in the long run), you can just use the play_til_broke() function, which takes 
# the amount of money in your wallet when you walk into the casino, plays roulette 
# according to the above strategy until you are too broke to continue with the 
# strategy, then outputs the number of psins it took for you to go broke. 
# The function average_time_til_broke() takes an integer n and the amount of 
# money in your wallet when you walk into the casino, plays roulette 
# according to the above strategy until you are too broke to continue, repeats
# this n times and tells you the average number of spins it took for you to go 
# broke. 





# Defining a play function and a print method for its output --------------





# the following function spins the roulette wheel and outputs
# the colour that the ball landed on
spin <- function() {
  colour <- c("red", "black", "green")
  sample(colour, size = 1, prob = c(18/37, 18/37, 1/37))
}


# the following function calculates the payout for a given stake,
# bet, and spin
score <- function(stake, colouryoubeton, spin){ 
  poss = c("red" = 2, "black" = 2, "green" = 36)
  if(colouryoubeton == spin){
    payout <- unname(poss[spin])*stake 
  } else {
    payout <- 0
  }
  payout
}


# the following function asks for your bet (for example, you could bet $3 on 
# "black"). It then spins the wheel and outputs the colour which the ball landed
# on, as well as telling you what you won (if anything) and advising the amount 
# that you should bet next time. 
play <-  function(stake, colour){
  payout <- c(0, stake)
  spin <- spin()
  win <- colour == spin
  winnings <- payout[1 + win]
  structure(winnings, 
            nexttimebet = c(2*stake + 1, 1)[1 + win], spin = spin, 
            class = "rouletteclass")
}


# the following is a print method for the output of the play function
print.rouletteclass <- function(x, ...){
  nexttimebet <- attr(x, "nexttimebet")
  spin <- attr(x, "spin")
  string0 <- paste("ball landed on:", spin, sep = " ")
  string1 <- paste(string0, x, sep = "\nyou receive: $")
  string2 <- paste(string1, nexttimebet, sep = "\nnext time you should bet: $")
  cat(string2)
}


# See how long it takes to go broke with this roulette strategy -----------



# Now let's see how long it takes to go broke playing roulette. Supply 
# the amount of money in your wallet to this function and it will tell
# you how long it took for you to inevitably go broke. 

play_til_broke <- function(starting_cash) {
  wallet <- starting_cash
  winnings <- NA
  attr(winnings, "nexttimebet") <- 1
  number_of_plays <- 0
  while(wallet >= attr(winnings, "nexttimebet")){
    stake <- attr(winnings, "nexttimebet")
    winnings <- play(stake, "red")
    wallet <- wallet - stake + winnings
    number_of_plays <- number_of_plays +1
  }
  number_of_plays
}


# The following function asks for your starting cash and an integer n, 
# then tries the roulette strategy n times and 
# tells you the average time that it took to go broke
average_time_til_broke <- function(starting_cash, n) {
  mean(replicate(n, play_til_broke(starting_cash)))        
}