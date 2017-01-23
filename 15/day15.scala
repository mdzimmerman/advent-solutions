case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

case class Recipe(ingredients: List[Ingredient]) {
  def getScore(amounts: List[Int]): Long = {
    val tuples = amounts zip ingredients
    val capacity   = tuples.map( x => x._1 * x._2.capacity ).sum
    val durability = tuples.map( x => x._1 * x._2.durability).sum
    val flavor     = tuples.map( x => x._1 * x._2.flavor ).sum
    val texture    = tuples.map( x => x._1 * x._2.texture ).sum
    if ( capacity < 0 || durability < 0 || flavor < 0 || texture < 0 )
      0
    else
      capacity.toLong * durability * flavor * texture
  }

  def getCalories(amounts: List[Int]): Int = {
    (amounts zip ingredients).map( x => x._1 * x._2.calories).sum
  }
}

val test = Recipe(List(
  Ingredient("butterscotch", -1, -2, 6, 3, 8),
  Ingredient("cinnamon", 2, 3, -2, -1, 3)
))

println(test.getScore(List(44, 56)))

for (i <- 0 to 100 ; j = 100-i) {
  val a = List(i, j)
  val s = test.getScore(a)
  println(s"$a => $s")
}

val input = Recipe(List(
  Ingredient("sugar", 3, 0, 0, -3, 2),
  Ingredient("sprinkles", -3, 3, 0, 0, 9),
  Ingredient("candy", -1, 0, 4, 0, 1),
  Ingredient("chocolate", 0, 0, -2, 2, 8)
))

var max = 0L
for (i <- 0 to 100; j <- 0 to 100-i; k <- 0 to 100-i-j; l = 100-i-j-k) {
  val a = List(i, j, k, l)
  val s = input.getScore(a)
  val c = input.getCalories(a)
  if (c == 500) {
    println(s"$a => $s ($c)")
    if (s > max) max = s
  }
}
println(s"$max")