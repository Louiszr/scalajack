package blackjackSim

import blackjackSim.Player._

object BlackJackDefs {
  def hitPlayer(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    Dealer gives exactly one card to the player
     */
    val card = d.shoe.head
    val newD = d.copy(shoe = d.shoe.tail)
    val newP = p.getCard(card)
    (newD, newP)
  }
  def deal(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    Dealer gives two cards to the player and two to himself
     */
    val (d0, p0) = hitPlayer(d, p)
    val (d1, p1) = hitPlayer(d0.hitDealer, p0)
    val d2 = d1.hitDealer
    (d2, p1)
  }
  def bet(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    Player placing the bet (stake)
     */
    val newP = p.copy(bankroll = p.bankroll - p.bet, stake = p.bet + p.stake)
    (d, newP)
  }
  def payOut(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    Dealer decides payout after he finishes
     */
    val (d1, p1) =
      if (p.hand.isBlackJack && d.hand.isBlackJack) (d, p.credit(p.stake))
      else if (p.hand.isBlackJack) (d, p.credit(p.stake * 2.5))
      else if (p.hand.isBust) (d, p)
      else if (d.hand.isBust) (d, p.credit(p.stake * 2))
      else if (d.hand.total == p.hand.total) (d, p.credit(p.stake))
      else if (d.hand.total < p.hand.total) (d, p.credit(p.stake * 2))
      else (d, p)
    reset(d1, p1)
  }
  def reset(d: Dealer, p: Player): (Dealer, Player) = (d.reset(), p.reset())
  def playerAct(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    Player should not act if the dealer has blackjack
    Otherwise player acts based on Strategy = Hand => Decision
     */
    if (d.hand.isBlackJack) (d, p)
    else p.action match {
      case Hit =>
        val (d1, p1) = hitPlayer(d, p)
        playerAct(d1, p1)
      case Stand => (d, p)
      case Double =>
        val (d1, p1) = bet(d, p)
        val (d2, p2) = hitPlayer(d1, p1)
        (d2, p2)
      case Split => ???
    }
  }
  def dealerAct(d: Dealer, p: Player): (Dealer, Player) = {
    /*
    after player finishes
    If Player has blackjack, dealer does not draw,
    else
    Dealer draws to soft 17
     */
    if (p.hand.isBlackJack || p.hand.isBust) (d, p)
    else if (d.hand.total < 17) dealerAct(d.hitDealer, p)
    else (d, p)
  }
  def game(d: Dealer, p: Player): (Dealer, Player) = {
    val (d0, p0) = bet(d, p)
    val (d1, p1) = deal(d0, p0)
    val (d2, p2) = playerAct(d1, p1)
    val (d3, p3) = dealerAct(d2, p2)
    payOut(d3, p3)
    // TODO: Implement insurance
  }
}

trait Card
case object NoCard extends Card
case object Ace extends Card
final case class NonAce(number: String) extends Card {
  require(toIntOption(number).isDefined || List("J", "Q", "K").contains(number.toUpperCase))
  def toIntOption(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
  def value(): Int = toIntOption(number).fold(10)(num => num)
}

final case class Hand(nonAceCards: Seq[NonAce], aceCounts: Int) {
  def total(): Int = {
    val nonAceValues = nonAceCards.foldLeft(0)((acc, card) => acc + card.value)
    if (aceCounts == 0) nonAceValues
    else {
      val softTotal = nonAceValues + aceCounts + 10
      if (softTotal > 21) nonAceValues + aceCounts else softTotal
    }
  }
  val isBust: Boolean = total > 21
  def isbetterThan(that: Hand): Boolean = that.isBust || (!this.isBust && this.total > that.total)
  def getCard(card: Card): Hand = card match {
    case c @ Ace => Hand(nonAceCards, aceCounts + 1)
    case c @ NonAce(_) => Hand(nonAceCards :+ c, aceCounts)
  }
  val isBlackJack: Boolean = aceCounts == 1 && nonAceCards.length == 1 && total == 21
}

object Hand {
  def emptyHand(): Hand = Hand(Seq.empty[NonAce], 0)
}

final case class Dealer(shoe: Seq[Card], hand: Hand, firstCard: Card) {
  import Hand._
  def reset(): Dealer = this.copy(hand = emptyHand, firstCard = NoCard)
  def getCard(card: Card): Dealer = firstCard match {
    case NoCard =>
      this.copy(hand = this.hand.getCard(card), firstCard = card)
    case _ =>
      this.copy(hand = this.hand.getCard(card))
  }
  def hitDealer(): Dealer = getCard(shoe.head).copy(shoe = shoe.tail)
}

object Dealer {
  import scala.util.Random
  def shuffle(deckNum: Int): Seq[Card] = {
    val suite: Seq[Card] =
      (2 to 10).map(_.toString).map(NonAce) ++
      Seq("J", "Q", "K").map(NonAce) :+
      Ace
    val deck = suite.flatMap(card => Seq(card, card, card, card))
    def createShoeRec(accShoe: Seq[Card], remainingDeckNum: Int): Seq[Card] = {
      if (remainingDeckNum == 0) accShoe
      else createShoeRec(accShoe ++ deck, remainingDeckNum - 1)
    }
    Random.shuffle(createShoeRec(Seq.empty[Card], deckNum))
  }
}

final case class Player(bankroll: Double, hand: Hand, strategy: Strategy, stake: Double) {
  import Hand._
  import Player._
  val bet: Double = bankroll.min(5d)
  def reset(): Player = this.copy(hand = emptyHand, stake = 0d)
  def getCard(card: Card): Player = this.copy(hand = this.hand.getCard(card))
  def credit(moneyReceived: Double) = this.copy(bankroll = this.bankroll + moneyReceived)
  def action(): PlayerAction = strategy(hand)
}

object Player {
  type Strategy = Hand => PlayerAction
  trait PlayerAction
  case object Hit extends PlayerAction
  case object Stand extends PlayerAction
  case object Double extends PlayerAction
  case object Split extends PlayerAction
  // TODO: More betting logic
  val noBustStrategy: Strategy = hand => if (hand.total <= 11) Hit else Stand
}