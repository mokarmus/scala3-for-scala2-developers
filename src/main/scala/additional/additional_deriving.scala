package additional

//Import required for typeclass derivation
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

trait Show[A]:
  def show(a: A): String
  
object Show {
  
  given Show[String] = s => s"$s!" 
  given Show[Int] = _.toString
  
  def showProduct[T](shows: => List[Show[_]]): Show[T] =
    new Show[T]:
      def show(t: T): String = {
        t.asInstanceOf[Product].productIterator.zip(shows.iterator).map {
          case (p, s) => s.asInstanceOf[Show[Any]].show(p)
        }.mkString
      }

  def showSum[T](s: Mirror.SumOf[T], shows: => List[Show[_]]): Show[T] =
    new Show[T]:
      def show(t: T): String = {
        val index = s.ordinal(t)
        (shows(index).asInstanceOf[Show[Any]]).show(t)
      }

  inline def summonAll[A <: Tuple]: List[Show[_]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]


  inline given derived[T](using m: Mirror.Of[T]): Show[T] =
    lazy val shows = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T] => showSum(s, shows)
      case _: Mirror.ProductOf[T] => showProduct(shows)
}


//https://xebia.com/blog/automatically-deriving-typeclass-instances-in-scala-3/
enum BinaryTree[+A] derives Show: 
  case Node(value: A, left: BinaryTree[A], right: BinaryTree[A])
  case Leaf

trait Eq[A]:
  def eq(x: A, y: A): Boolean
  

object BinaryTree{
  
  given binaryTreeEqual[A](using eqA: Eq[A]): Eq[BinaryTree[A]] with {
    override def eq(x: BinaryTree[A], y: BinaryTree[A]): Boolean = (x, y) match
      case (BinaryTree.Leaf, BinaryTree.Leaf) => 
        true
      case (BinaryTree.Node(xValue, xLeft, xRight), BinaryTree.Node(yValue, yLeft, yRight)) =>
        eqA.eq(xValue, yValue) && eq(xLeft, yLeft) && eq(xRight, yRight)
        
      case _ => false  
  }
  
  given Eq[Int] with {
    override def eq(x: Int, y: Int): Boolean = x == y
  }
  


}


object additional_deriving extends App:
  
  import BinaryTree.given_Eq_Int
  
  val binaryTree = BinaryTree.Node(5,
    BinaryTree.Leaf,
    BinaryTree.Node(10,
      BinaryTree.Leaf,
      BinaryTree.Leaf
    )
  )


  val binaryTree2 = BinaryTree.Node(5,
    BinaryTree.Leaf,
    BinaryTree.Node(10,
      BinaryTree.Leaf,
      BinaryTree.Leaf
    )
  )

  val binaryTree3 = BinaryTree.Node("5",
    BinaryTree.Leaf,
    BinaryTree.Node("10",
      BinaryTree.Leaf,
      BinaryTree.Leaf
    )
  )
  
  
  println(BinaryTree.binaryTreeEqual.eq(binaryTree, binaryTree2))
  
  val binaryTreeShow = summon[Show[BinaryTree[String]]]
  
  println(binaryTreeShow.show(binaryTree3))
  
  println(binaryTreeShow.show(BinaryTree.Leaf).isEmpty)