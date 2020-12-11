package org.triclinic.day22

import scala.annotation.tailrec
import scala.collection.mutable

object Turn extends Enumeration {
  type Turn = Value
  val HeroEffect, Hero, BossEffect, Boss = Value

  def next(turn: Turn): Turn = {
    turn match {
      case HeroEffect => Hero
      case Hero => BossEffect
      case BossEffect => Boss
      case Boss => HeroEffect
    }
  }
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
                 heroHp: Int,
                 heroMana: Int,
                 heroArmor: Int,
                 bossHp: Int,
                 shield: Int,
                 recharge: Int,
                 poison: Int,
                 last: String,
                 manaSpent: Int,
                 prev: Option[State],
                 hard: Int = 0) {

  // figure out if we won or lost or we have more moves
  lazy val (status, next) = {
    if (heroHp <= 0)
      (Result.BossWins, Nil)
    else if (bossHp <= 0)
      (Result.HeroWins, Nil)
    else {
      turn match {
        case Turn.HeroEffect | Turn.BossEffect =>
          (Result.Active, List(applyEffects))
        case Turn.Boss =>
          (Result.Active, List(bossAttack))
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

  def applyEffects(): State = {
    this.copy(
      turn=Turn.next(turn),
      heroHp=heroHp-hard,
      heroMana=if (recharge > 0) heroMana+101 else heroMana,
      heroArmor=if (shield > 0) hero.baseArmor+7 else hero.baseArmor,
      bossHp=if (poison > 0) bossHp-3 else bossHp,
      shield=if (shield > 0) shield-1 else 0,
      recharge=if (recharge > 0) recharge-1 else 0,
      poison=if (poison > 0) poison-1 else 0,
      last="effect",
      prev=Some(this))
  }

  def castMissile(): Option[State] = {
    val cost = 53
    if (heroMana >= cost)
      Some(this.copy(
        turn=Turn.next(turn),
        heroMana=heroMana-cost,
        bossHp=bossHp-4,
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
        turn=Turn.next(turn),
        heroHp=heroHp+2,
        heroMana=heroMana-cost,
        bossHp=bossHp-2,
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
        turn=Turn.next(turn),
        heroMana=heroMana-cost,
        shield=6,
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
        turn=Turn.next(turn),
        heroMana=heroMana-cost,
        poison=6,
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
        turn=Turn.next(turn),
        heroMana=heroMana-cost,
        recharge=5,
        last="recharge",
        manaSpent=manaSpent+cost,
        prev=Some(this)))
    else
      None
  }

  def castAll(): List[State] =
    List(castMissile, castDrain, castShield, castPoison, castRecharge).flatten

  def bossAttack(): State = {
    val dmg = List(boss.damage-heroArmor, 1).max
    this.copy(
      turn=Turn.next(turn),
      heroHp=heroHp-dmg,
      last="boss",
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
    f"State(turn=$turn%-10s " +
      f"heroHp=$heroHp%2d heroMana=$heroMana%3d heroArmor=$heroArmor " +
      f"recharge=$recharge shield=$shield poison=$poison " +
      f"bossHp=$bossHp%2d last=$last%8s manaSpent=$manaSpent%4d status=$status)"
}

object State {
  val queueOrdering: Ordering[State] = Ordering.by{ x: State => x.manaSpent }.reverse

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
