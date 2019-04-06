package blackjackSim

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import BlackJackDefs._

@RunWith(classOf[JUnitRunner])
class BlackJackDefsSuite extends FlatSpec with Matchers{
  trait oneDeckNoBustPlayer {
    import Dealer._
    import Hand._
    import Player._
    val d = Dealer(shuffle(1), emptyHand, NoCard)
    val p = Player(100, emptyHand, noBustStrategy, 0, None)
  }

  trait splitHandPlayer extends oneDeckNoBustPlayer {
    val splitStake = 5
    val pSplitted = p.copy(stake = splitStake).getCard(Ace).getCard(Ace).split()
  }

  "Card" should "report correct values for 2-10" in {
    val nums = 2 to 10
    val numberCards = nums.map(_.toString).map(NonAce)
    numberCards.map(_.value).toList shouldBe nums.toList
  }

  "Card" should "report 10s for JQK" in {
    val cards = List(NonAce("J"), NonAce("Q"), NonAce("K"))
    cards.map(_.value) shouldBe List(10, 10, 10)
  }

  "Card" should "throw exception for non-JQK String" in {
    intercept[IllegalArgumentException] {
      NonAce("HelloWorld")
    }
  }

  "Hand" should "report a total of 11 for card 5 and card 6" in {
    val cards = List(NonAce("5"), NonAce("6"))
    Hand(cards, 0).total shouldBe 11
  }

  "Hand" should "have isBust=true for card 10, 10, 5" in {
    val cards = List(NonAce("10"), NonAce("10"), NonAce("5"))
    Hand(cards, 0).isBust shouldBe true
  }

  "Hand" should "have total of 18 for soft 18" in {
    val nonAceCards = List(NonAce("7"))
    val softEighteen = Hand(nonAceCards, 1)
    softEighteen.total shouldBe 18
  }

  "Hand" should "recalculate total for soft 18 when it gets a 9" in {
    val nonAceCards = List(NonAce("7"))
    val softEighteen = Hand(nonAceCards, 1)
    val postHit = softEighteen.getCard(NonAce("9"))
    postHit.isBust shouldBe false
    postHit.total shouldBe 17
  }

  "Hand" should "have total of 12 for card 9, A, A, A" in {
    val hand = Hand(List(NonAce("9")), 0)
    val postHit = hand.getCard(Ace).getCard(Ace).getCard(Ace)
    postHit.isBust shouldBe false
    postHit.total shouldBe 12
  }

  it should "has isBlackJack=false if the hand has been splitted" in {
    new splitHandPlayer {
      pSplitted.getCard(NonAce("10")).hand.isBlackJack shouldBe false
    }
  }

  "Dealer" should "update hand when he gets a card" in {
    new oneDeckNoBustPlayer {
      val newDealer = d.getCard(NonAce("9"))
      newDealer.hand.total shouldBe 9
      newDealer.firstCard shouldBe NonAce("9")
    }
  }

  it should "deal the exact card to himself if hitDealer is called on shoe with 1 card" in {
    val dealer = Dealer(Seq(Ace), Hand.emptyHand, NoCard)
    val newDealer = dealer.hitDealer()
    newDealer.hand.total shouldBe 11
    newDealer.firstCard shouldBe Ace
    newDealer.shoe.isEmpty shouldBe true
  }

  it should "reset after reset() is called" in {
    val dealer = Dealer(Seq(Ace), Hand(Seq(NonAce("2")), 1), Ace)
    val resetDealer = dealer.reset
    resetDealer.hand.total shouldBe 0
    resetDealer.firstCard shouldBe NoCard
  }

  it should "generate 52 cards when 1 deck is shuffled" in {
    Dealer.shuffle(1).length shouldBe 52
  }

  it should "generate 4 * 52 cards when 4 decks are shuffled" in {
    Dealer.shuffle(4).length shouldBe 52 * 4
  }

  "Player" should "update hand when he gets a card" in {
    new oneDeckNoBustPlayer {
      val newPlayer = p.getCard(NonAce("9"))
      newPlayer.hand.total shouldBe 9
    }
  }

  it should "bet the bankroll amount if that is less than what he intends to bet" in {
    new oneDeckNoBustPlayer {
      val player = p.copy(bankroll = 0)
      player.bet shouldBe 0
    }
  }

  it should "update his bankroll when he receives credit" in {
    new oneDeckNoBustPlayer {
      p.credit(10d).bankroll shouldBe 10d + p.bankroll
    }
  }

  it should "populate playerSplit when split() is called" in {
    new splitHandPlayer {
      pSplitted.bankroll shouldBe p.bankroll - splitStake
      pSplitted.hand shouldBe Hand.emptyHand.getCard(Ace)
      pSplitted.strategy shouldBe p.strategy
      pSplitted.stake shouldBe splitStake

      pSplitted.playerSplit.get.bankroll shouldBe p.bankroll - splitStake
      pSplitted.playerSplit.get.hand shouldBe Hand.emptyHand.getCard(Ace)
      pSplitted.playerSplit.get.strategy shouldBe p.strategy
      pSplitted.playerSplit.get.stake shouldBe splitStake
      pSplitted.playerSplit.get.playerSplit shouldBe None
    }
  }

  "hitPlayer" should "give player one card and update dealer shoe" in {
    import Hand._
    new oneDeckNoBustPlayer {
      val (d1, p1) = hitPlayer(d, p)
      d1.shoe shouldEqual d.shoe.tail
      p1.hand shouldBe emptyHand.getCard(d.shoe.head)
    }
  }

  "deal" should "deal cards as intended" in {
    import Hand._
    new oneDeckNoBustPlayer {
      val (d1, p1) = deal(d, p)
      d.shoe match {
        case first :: second :: third :: fourth :: rest =>
          d1.hand shouldBe emptyHand.getCard(second).getCard(fourth)
          p1.hand shouldBe emptyHand.getCard(first).getCard(third)
          d1.shoe shouldBe rest
      }
    }
  }

  "bet(d, p)" should "subtract bet amount from player bankroll and assign it to player stake" in {
    new oneDeckNoBustPlayer {
      val p0 = p.copy(stake = 5d)
      val (d1, p1) = bet(d, p0)
      p1.bankroll shouldBe p0.bankroll - p0.bet
      p1.stake shouldBe p0.bet + p0.stake
    }
  }

  "payOut" should "push when both dealer and player have bj" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(Ace).getCard(NonAce("10"))
      val p1 = p.getCard(Ace).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll + stake
      p2.stake shouldBe 0
    }
  }

  it should "pay player 3 to 2 if only player have bj" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(NonAce("8")).getCard(NonAce("10"))
      val p1 = p.getCard(Ace).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll + stake * 2.5
      p2.stake shouldBe 0
    }
  }

  it should "pay nothing if player busts" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(NonAce("8")).getCard(NonAce("10"))
      val p1 = p.getCard(NonAce("10")).getCard(NonAce("10")).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll
      p2.stake shouldBe 0
    }
  }

  it should "pay even to player if only dealer busts" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(NonAce("10")).getCard(NonAce("10")).getCard(NonAce("10"))
      val p1 = p.getCard(NonAce("10")).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll + stake * 2
      p2.stake shouldBe 0
    }
  }

  it should "push when player and dealer have the same score < 21" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(NonAce("10")).getCard(NonAce("10"))
      val p1 = p.getCard(NonAce("10")).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll + stake
      p2.stake shouldBe 0
    }
  }

  it should "pay even to player if player has higher score than dealer, both < 21" in {
    new oneDeckNoBustPlayer {
      val stake = 10
      val d1 = d.getCard(NonAce("10")).getCard(NonAce("9"))
      val p1 = p.getCard(NonAce("10")).getCard(NonAce("10"))
      val (d2, p2) = payOut(d1, p1.copy(stake = stake))
      p2.bankroll shouldBe p.bankroll + stake * 2
      p2.stake shouldBe 0
    }
  }

  it should "pay Hand(A, 7, 3)(Doubled) = 4 times stake and Hand(A, 10) = 2 times stake when the dealer lost" in {
    new splitHandPlayer {
      val d1 = d.copy(hand = Hand.emptyHand.getCard(NonAce("10")).getCard(NonAce("7")))
      val ps = pSplitted.playerSplit.get
      val ps1 = ps.copy(bankroll = ps.bankroll - ps.stake
        , hand = ps.hand.getCard(NonAce("7")).getCard(NonAce("3"))
        , stake = ps.stake * 2
      )
      val p1 = pSplitted.copy(hand = pSplitted.hand.getCard(NonAce("10")), playerSplit = Option(ps1))
      val (d2, p2) = payOut(d1, p1)
      p2.bankroll shouldBe pSplitted.bankroll + 6 * pSplitted.stake
    }
  }
}
