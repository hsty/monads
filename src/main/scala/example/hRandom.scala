package example

class hRandom {

  trait Generator[+T] {

    hitesh =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(hitesh.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(hitesh.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random()
    def generate = rand.nextInt()
  }

  //val booleans = new Generator[Boolean] {
  //  def generate = integers.generate > 0
  //}

  val booleans = for (x <- integers) yield x>0

  //val pairs = new Generator[(Int, Int)] {
  //  def generate = (integers.generate, integers.generate)
  //}

  def pairs[T,U](t: Generator[T], u: Generator[U]) = for {
    i <- t
    j <- u
  } yield(i,j)


  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for(x <- integers) yield lo + x % (hi-lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if(isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree


  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if(isLeaf) leafs else innerTree
  } yield tree

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield(Leaf(x))

  def innerTree: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield (Inner(left, right))


  def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {

    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "Test failed for"+value)
    }
    println("Test passed")
  }

}
