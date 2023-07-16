import typeclass_basics.PrettyPrint
import typeclass_graduation.Data.Primitive

/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */
object typeclass_basics:
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  given PrettyPrint[String] with
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */
  given PrettyPrint[Person] with
    extension (person: Person) def prettyPrint: String = s"${person.name} is ${person.age} years old." 

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given intPrettyPrint1 : PrettyPrint[Int] with 
    extension (int: Int) def prettyPrint: String = int.toString
  

  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]

  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val intPrettyPrint: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  def prettyPrintIt[A](value: A)(using pp: PrettyPrint[A]) =
    println(value.prettyPrint)

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A]: PrettyPrint[List[A]] with
    extension (a: List[A]) def prettyPrint: String = {
      given pp: PrettyPrint[A] = summon[PrettyPrint[A]]
      a.map(_.prettyPrint).mkString(",")
    }

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given vectorPrettyPrint[A: PrettyPrint] : PrettyPrint[Vector[A]] with 
    extension (vector: Vector[A]) def prettyPrint: String =
      given pp: PrettyPrint[A] = summon[PrettyPrint[A]]
      vector.map(_.prettyPrint).mkString(",")

  import scala.CanEqual.*

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  object Hash:
    given Hash[Int] = _.hashCode
    given Hash[Long] = _.hashCode
    given Hash[Float] = _.hashCode
    given Hash[Double] = _.hashCode

  object givens {
    /**
     * EXERCISE 1
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
     import Hash.given_Hash_Int
     12.hash 

    /**
     * EXERCISE 2
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
     import Hash.given_Hash_Double
     12.123.hash   
  }
  
  
  object more_typeclasses:
    /**
     * EXERCISE 1
     * 
     * Define a typeclass called `Enumerable[A]` that can list all of the `A` values of a type with 
     * a finite number of values. One way to do this is to add a single operation called 
     * `enumerate` that returns a `List[A]`.
     */
    trait Enumerable[A] {
      def enumerate: List[A]
    }

    object Enumerable:
      /**
       * EXERCISE 2
       * 
       * Define an instance of the typeclass Enumerable for `Boolean`.
       */
      given Enumerable[Boolean] with
        override def enumerate: List[Boolean] = List(true, false)

      /**
       * EXERCISE 3
       * 
       * Instances for generic data types often depend on instances for their element types.
       * With `using`, this relationship can be captured precisely. Define an `Enumerable` instance
       * for `Option[A]` given an instance for `A`.
       */
      given [A](using enumerableA: Enumerable[A]): Enumerable[Option[A]] with
        override def enumerate: List[Option[A]] =
          None :: enumerableA.enumerate.map(Some(_))

    end Enumerable

    /**
     * EXERCISE 4
     * 
     * By adding a `using`, implement this polymorphic function to return the "first" enumerable
     * value.
     */
    def first[A](using enumerable: Enumerable[A]): A = enumerable.enumerate.head 

    /**
     * EXERCISE 5
     * 
     * By adding a `using`, implement this polymorphic function to return the "last" enumerable
     * value.
     */
    def last[A: Enumerable]: A = summon[Enumerable[A]].enumerate.last

    /**
     * EXERCISE 6
     * 
     * By adding a `using`, implement this polymorphic function to return the "ordinal" of the 
     * specified enumerable value.
     */
    def ordinalOf[A](value: A)(using enumerable: Enumerable[A]): Int =
      enumerable.enumerate.indexOf(value)

    /**
     * EXERCISE 7
     * 
     * Make an extension method called `ordinal` that is added to any data type that has an 
     * `Enumerable` instance. You will have to use `using`.
     */
    extension [A](value: A)(using enumerable: Enumerable[A]) def ordinal = enumerable.enumerate.indexOf(value)

    /**
     * EXERCISE 8
     * 
     * Givens can be introduced with an expression by using a so-called "alias given". The 
     * difference is that instead of using `with` and defining the body of the given, you 
     * instead use `=` and set the instance equal to a specified value.
     * 
     * Define a `Show` for `Email` by using a given alias and the `contramap` function on 
     * the `Show` instance for `String`.
     */
    given Show[Email] = summon[Show[String]].contramap(_.value)

    trait Show[-A]: 
      self =>
        def show(value: A): String

        def contramap[B](f: B => A): Show[B] = 
          (b: B) => self.show(f(b))

    final case class Email(value: String)

    given Show[String] with 
      def show(value: String): String = value 

    /**
     * EXERCISE 9
     * 
     * You can introduce givens inside patterns. These are called "pattern-bound" given instances.
     * Using some example of destructuring assignment, introduce a given with a pattern.
     */
    //Not sure what should be a result here 
    Some(summon[Show[String]]) match 
      case Some(s @ given Show[String]) => s.show("value")

    /**
     * EXERCISE 10
     * 
     * Using `NotGiven`, you can create given instances that will be used only if the specified
     * instance is not available.
     * 
     * Using `NotGiven`, create an instance of `Show` for `List[A]` only when there is not already 
     * a given instance of `Show` for `A`.
     */
    import scala.util.NotGiven 
    given [A](using NotGiven[Show[A]]): Show[List[A]] = (value: List[A]) => value.mkString(",")
    

    /**
     * EXERCISE 11
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashing[T](value: T)(using Hash[T]) = value.hash 

    /**
     * EXERCISE 12
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingDoubles(using Hash[Double]) = 12.123.hash   

    /**
     * EXERCISE 13
     * 
     * Intersperse normal parameters with their `using Hash[X]` for those parameters.
     */
    def intersperse[A: Hash, B: Hash, C: Hash, D: Hash] = ???
  
object typeclass_derives:
  /**
   * EXERCISE 1
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Color`.
   */
  enum Color:
    case Red 
    case Green 
    case Blue

  given CanEqual[Color, Color] = CanEqual.derived
  
  
  /**
   * EXERCISE 2
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Person`.
   */
  final case class Person(name: String, age: Int) derives CanEqual

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  final case class Rational(n: Int, d: Int)

  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  given Conversion[Rational, Double] = (r: Rational) => r.n.toDouble / r.d

  import scala.language.implicitConversions
  
  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  Rational(1, 2) * 2.0

object typeclass_graduation:
  /**
   * EXERCISE 1
   * 
   * Add cases to this enum for every primitive type in Scala.
   */
  enum PrimType[A]:
    case Char extends PrimType[Char]
    case Byte extends PrimType[Byte]
    case Short extends PrimType[Short]
    case Int extends PrimType[Int]
    case Long extends PrimType[Long]
    case Float extends PrimType[Float]
    case Double extends PrimType[Double]
  
  /**
   * EXERCISE 2
   * 
   * Add another case to `Data` to model enumerations, like `Either`.
   */
  enum Data:
    case Record(fields: Map[String, Data])
    case Primitive[A](primitive: A, primType: PrimType[A])
    case Collection(elements: Vector[Data])
    case Enumerations[A](instance: A)
    

  /**
   * EXERCISE 3
   * 
   * Develop a type class called `EncodeData[A]`, that can encode an `A` into `Data`.
   */
  trait EncodeData[A]: 
    extension (value: A) def encode: Data
  
  /**
   * 
   * EXERCISE 4
   * 
   * In the companion object of `Data`, write encoders for different primitive types in Scala,
   * including lists and collections.
   */
  object EncodeData {

    given EncodeData[Char] = (value: Char) => Primitive[Char](value, PrimType.Char)
    given EncodeData[Byte] = (value: Byte) => Primitive[Byte](value, PrimType.Byte)
    given EncodeData[Short] = (value: Short) => Primitive[Short](value, PrimType.Short)
    given EncodeData[Int] = (value: Int) => Primitive[Int](value, PrimType.Int)
    given EncodeData[Long] = (value: Long) => Primitive[Long](value, PrimType.Long)
    given EncodeData[Float] = (value: Float) => Primitive[Float](value, PrimType.Float)
    given EncodeData[Double] = (value: Double) => Primitive[Double](value, PrimType.Double)
    
    given [A](using EncodeData[A]):  EncodeData[Iterable[A]] =  (list: Iterable[A]) =>
      Data.Collection(list.map(_.encode).toVector)
    
    //TODO does not work in the person companion object and I how no idea why 
    given EncodeData[Person] = (p: Person) => Data.Record(Map("name" -> p.name.toVector.encode, "age" -> p.age.encode))
    
  }

  /**
   * EXERCISE 5
   * 
   * Create an instance of `EncodeData` for `Person`.
   */
  final case class Person(name: String, age: Int)
  object Person {
    
  }