package org.triclinic.day21


case class Equip(name: String,
                 cost: Int,
                 damage: Int,
                 armor: Int)

object Equip {
  val Empty = Equip("Empty", 0, 0, 0)

  val weapons = List(
    Equip("Dagger",      8, 4, 0),
    Equip("Shortsword", 10, 5, 0),
    Equip("Warhammer",  25, 6, 0),
    Equip("Longsword",  40, 7, 0),
    Equip("Greataxe",   74, 8, 0))

  val armors = List(
    Equip("Leather",    13, 0, 1),
    Equip("Chainmail",  31, 0, 2),
    Equip("Splintmail", 53, 0, 3),
    Equip("Bandedmail", 75, 0, 4),
    Equip("Platemail", 102, 0, 5))

  val rings = List(
    Equip("Damage +1",  25, 1, 0),
    Equip("Damage +2",  50, 2, 0),
    Equip("Damage +3", 100, 3, 0),
    Equip("Defense +1", 20, 0, 1),
    Equip("Defense +2", 40, 0, 2),
    Equip("Defense +3", 80, 0, 3))

  def genRingSets(): List[List[Equip]] = {
    List() :: rings.map(List(_)) ++ rings.combinations(2)
  }

  def genLoadouts(): List[List[Equip]] = {
    for (
      weapon <- Equip.weapons.map(Option(_));
      armor <- None :: Equip.armors.map(Option(_));
      ringset <- genRingSets())
      yield List(weapon, armor).flatten ++ ringset
  }
}

trait Character {
  val hp: Int
  val damage: Int
  val armor: Int
}

object Character {
  def firstWins(first: Character,
                second: Character): Boolean = {
    val dmgFirst = first.damage - second.armor
    val dmgSecond = second.damage - first.armor
    if (dmgFirst == 0 && dmgSecond == 0) {
      println("both parties have 0 effective damage")
      return true
    }
    var hpFirst = first.hp
    var hpSecond = second.hp
    while (true) {
      // first attacks
      hpSecond -= dmgFirst
      //println(s"first attacks for ${first.damage - second.armor} damage, hp(1)=$hpFirst, hp(2)=$hpSecond")
      if (hpSecond <= 0)
        return true
      // second attacks
      hpFirst -= dmgSecond
      //println(s"second attacks for ${second.damage - first.armor} damage, hp(1)=$hpFirst, hp(2)=$hpSecond")
      if (hpFirst <= 0)
        return false
    }
    throw new Exception("should never get here")
  }
}

case class ManualCharacter(hp: Int,
                           damage: Int,
                           armor: Int) extends Character

case class EquippedCharacter(hp: Int,
                             loadout: List[Equip]) extends Character {
  override val damage: Int = loadout.map(_.damage).sum
  override val armor: Int  = loadout.map(_.armor).sum
  val cost: Int = loadout.map(_.cost).sum
}

object Day21 extends App {
  println(Equip.weapons)
  println(Equip.armors)
  println(Equip.rings)

  val testHero = ManualCharacter(8, 5, 5)
  val testBoss = ManualCharacter(12, 7, 2)
  println(Character.firstWins(testHero, testBoss))

  def part1(hp: Int, boss: Character): EquippedCharacter = {
    val heroes = Equip.genLoadouts.map(EquippedCharacter(hp, _)).sortBy(_.cost)
    for (h <- heroes) {
      //println(s"cost=${h.cost} dmg=${h.damage} armor=${h.armor} $h")
      if (Character.firstWins(h, boss)) {
        return h
      }
    }
    throw new Exception("should never get here")
  }

  def part2(hp: Int, boss: Character): EquippedCharacter = {
    val heroes = Equip.genLoadouts.map(EquippedCharacter(hp, _)).sortBy(_.cost).reverse
    for (h <- heroes) {
      val wins = Character.firstWins(h, boss)
      //println(s"wins=$wins cost=${h.cost} dmg=${h.damage} armor=${h.armor} $h")
      if (!wins)
        return h
    }
    throw new Exception("should never get here")
  }

  val boss = ManualCharacter(100, 8, 2)
  val hero1 = part1(100, boss)
  println(s"cost=${hero1.cost} $hero1")
  val hero2 = part2(100, boss)
  println(s"cost=${hero2.cost} $hero2")
}
