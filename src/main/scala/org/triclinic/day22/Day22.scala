package org.triclinic.day22

import scala.annotation.tailrec
import scala.collection.mutable

object Turn extends Enumeration {
  type Turn = Value
  val Hero, Boss = Value
}

object Result extends Enumeration {
  type Result = Value
  val Active, HeroWins, BossWins = Value
}

case class Hero(totalHp: Int, totalMana: Int, baseArmor: Int)
case class Boss(totalHp: Int, damage: Int)

case class State(hero: Hero,
                 boss: Boss,
                 turn: Turn.Turn,
                 initHeroHp: Int,
                 initHeroMana: Int,
                 initHeroArmor: Int,
                 initBossHp: Int,
                 initShield: Int,
                 initRecharge: Int,
                 initPoison: Int,
                 last: String,
                 manaSpent: Int,
                 prev: Option[State],
                 hard: Int = 0) {

  // apply all effects
  val heroHp = initHeroHp - hard
  val (heroMana, recharge) =
    if (initRecharge > 0)
      (initHeroMana+101, initRecharge-1)
    else
      (initHeroMana, 0)
  val (heroArmor, shield) =
    if (initShield > 0)
      (hero.baseArmor+7, initShield-1)
    else
      (hero.baseArmor, 0)
  val (bossHp, poison) =
    if (initPoison > 0)
      (initBossHp-3, initPoison-1)
    else
      (initBossHp, 0)

  // figure out if we won or lost or we have more moves
  lazy val (status, next) = {
    if (heroHp <= 0)
      (Result.BossWins, Nil)
    else if (bossHp <= 0)
      (Result.HeroWins, Nil)
    else {
      turn match {
        case Turn.Boss =>
          (Result.Active, List(bossAttack()))
        case Turn.Hero =>
          castAll() match {
            case Nil =>
              (Result.BossWins, Nil)
            case x =>
              (Result.Active, x)
          }
      }
    }
  }

  def castMissile(): Option[State] = {
    val cost = 53
    if (heroMana >= cost)
      Some(this.copy(
        turn=Turn.Boss,
        initHeroHp=heroHp,
        initHeroMana=heroMana-cost,
        initHeroArmor=heroArmor,
        initBossHp=bossHp-4,
        initShield=shield,
        initRecharge=recharge,
        initPoison=poison,
        last="missile",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castDrain(): Option[State] = {
    val cost = 73
    if (heroMana >= cost)
      Some(this.copy(
        turn=Turn.Boss,
        initHeroHp=heroHp+2,
        initHeroMana=heroMana-cost,
        initHeroArmor=heroArmor,
        initBossHp=bossHp-2,
        initShield=shield,
        initRecharge=recharge,
        initPoison=poison,
        last="drain",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castShield(): Option[State] = {
    val cost = 113
    if (heroMana >= cost && shield == 0)
      Some(this.copy(
        turn=Turn.Boss,
        initHeroHp=heroHp,
        initHeroMana=heroMana-cost,
        initHeroArmor=heroArmor,
        initBossHp=bossHp,
        initShield=6,
        initRecharge=recharge,
        initPoison=poison,
        last="shield",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castPoison(): Option[State] = {
    val cost = 173
    if (heroMana >= cost && poison == 0)
      Some(this.copy(
        turn=Turn.Boss,
        initHeroHp=heroHp,
        initHeroMana=heroMana-cost,
        initHeroArmor=heroArmor,
        initBossHp=bossHp,
        initShield=shield,
        initRecharge=recharge,
        initPoison=6,
        last="poison",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castRecharge(): Option[State] = {
    val cost = 229
    if (heroMana >= cost && recharge == 0)
      Some(this.copy(
        turn=Turn.Boss,
        initHeroHp=heroHp,
        initHeroMana=heroMana-cost,
        initHeroArmor=heroArmor,
        initBossHp=bossHp,
        initShield=shield,
        initRecharge=5,
        initPoison=poison,
        last="recharge",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castAll(): List[State] =
    List(castMissile, castDrain(), castShield(), castPoison(), castRecharge()).flatten


  def bossAttack(): State = {
    val dmg = List(boss.damage-heroArmor, 1).max
    this.copy(
      turn=Turn.Hero,
      initHeroHp=heroHp-dmg,
      initHeroMana=heroMana,
      initHeroArmor=heroArmor,
      initBossHp=bossHp,
      initShield=shield,
      initRecharge=recharge,
      initPoison=poison,
      last="boss_attack",
      manaSpent=manaSpent,
      prev=Some(this))
  }

  def getGame(): List[State] = {
    @tailrec
    def walk(state: State, seen: List[State]): List[State] = {
      state.prev match {
        case None =>
          (state :: seen)
        case Some(prev) =>
          walk(prev, state :: seen)
      }
    }
    walk(this, List())
  }

  override def toString(): String =
    s"State(turn=$turn " +
      s"heroHp=$initHeroHp heroMana=$initHeroMana heroArmor=$initHeroArmor " +
      s"recharge=$initRecharge shield=$initShield poison=$initPoison " +
      s"bossHp=$initBossHp last=$last status=$status)"
}

object State {
  val queueOrdering = Ordering.by{ x: State => x.manaSpent }.reverse

  def apply(hero: Hero, boss: Boss, hard: Int): State =
    State(hero, boss, Turn.Hero,
      hero.totalHp, hero.totalMana, hero.baseArmor,
      boss.totalHp,
      0, 0, 0,
      "init", 0, None, hard)

  def dfs(start: State) = {
    def recurse(current: State, depth: Int): Unit = {
      println(s"${"  " * depth}$current (${current.status})")
      if (depth <= 100)
        for (next <- current.next)
          recurse(next, depth+1)
    }
    recurse(start, 0)
  }

  def dijkstra(start: State): Int = {
    val queue = mutable.PriorityQueue.empty[State](State.queueOrdering)
    queue.enqueue(start)

    while (queue.nonEmpty) {
      val curr = queue.dequeue()
      if (curr.status == Result.HeroWins) {
        for (s <- curr.getGame())
          println(s)
        return curr.manaSpent
      }
      for (next <- curr.next)
        queue.enqueue(next)
    }
    -1
  }
}


object Day22 extends App {
  def test1(): Unit = {
    val hero = Hero(10, 250, 0)
    val boss = Boss(13, 8)

    val initstate = State(hero, boss, 0)
    //State.dfs(initstate)
    println(State.dijkstra(initstate))
  }

  def test2(): Unit = {
    val hero = Hero(10, 250, 0)
    val boss = Boss(14, 8)
    val initstate = State(hero, boss, 0)
    println(State.dijkstra(initstate))
  }

  def input(): Unit = {
    val hero = Hero(50, 500, 0)
    val boss = Boss(58, 9)
    println(State.dijkstra(State(hero, boss, 0)))
    println(State.dijkstra(State(hero, boss, 1)))
  }
  println("--- test 1 ---")
  test1()
  println("--- test 2 ---")
  test2()
  println("--- input ---")
  input()
}
