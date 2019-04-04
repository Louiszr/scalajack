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
    val p = Player(100, emptyHand, noBustStrategy, 0)
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
}
