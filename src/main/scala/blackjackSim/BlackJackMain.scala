package blackjackSim

import BlackJackDefs._

object BlackJackMain extends App{
  val dealer = Dealer(
    Seq(Ace, NonAce("10"), NonAce("9"), Ace, NonAce("10"), NonAce("9"))
    , Hand.emptyHand
    , NoCard
  )
  val player = Player(
    100
    , Hand.emptyHand
    , Player.noBustStrategy
    , 0
    , None
  )
  val (d1, p1) = game(dealer, player)
  println(d1)
  println(p1)
}
