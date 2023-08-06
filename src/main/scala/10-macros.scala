package macros

import scala.quoted.*

/**
 * MACROS
 * 
 * Scala 3 introduces statically-typed, hygienic macros. 
 */
object MacroBasics:
  inline def assert(inline expr: Boolean): Unit =
    ${assertImpl('expr)}

  def assertImpl(expr: Expr[Boolean])(using Quotes): Expr[Any] = '{
    val evaluate = $expr

    if (!evaluate) throw new AssertionError(s"Failed assertion: ${${Expr(expr.show)}}")
  }

  // inline def identity1[A](inline expr: A): A = ${'{expr}}
  
  // inline def identity2[A](inline expr: Expr[A])(using Quotes): Expr[A] = '{${expr}}
  
  // inline def identityT1[A](a: A)(using Quotes): ${'[A]} = a

  /**
   * EXERCISE 1
   * 
   * Construct an `Expr[Int]` by using the `Expr.apply` constructor on an int literal.
   */
  inline def exprInt(inline int: Int)(using Quotes): Expr[Int] = Expr.apply(int)

  /**
   * EXERCISE 2
   * 
   * Construct an `Expr[String]` by using the `Expr.apply` constructor on a string literal.
   */
  def exprString(string: String)(using Quotes): Expr[String] = Expr.apply(string)

  /**
   * EXERCISE 3
   * 
   * Turn this function into a macro by adding `inline` and delegating implementation to 
   * the compile-time function `assertEqualsImpl`. You will have to use splice and quote.
   * 
   * Also inline the parameters so they will be fully expanded in the macro.
   */
  inline def assertEquals[A](inline expected: A, inline actual: A): Unit =
    ${assertEqualsImpl('expected, 'actual)}

  /**
   * EXERCISE 4
   * 
   * Implement a compile-time function that generates the logic to check to see if the 
   * actual value is equal to the expected value, and if not, prints out a useful message
   * of the expressions. You will have to use quotation and splicing, as well as 
   * `Expr#show`.
   */
  def assertEqualsImpl(expected: Expr[Any], actual: Expr[Any])(using Quotes): Expr[Unit] = '{
    val expectedValue = $expected
    val actualValue = $actual
    
    
    if (expectedValue != actualValue) {
      println(s"Value of ${${Expr(expected.show)}} is not equal to  ${${Expr(actual.show)}}" )   
    }
    
    
    
  }

  /**
   * EXERCISE 5
   * 
   * Turn the following function into a macro, which delegates to the compile-time 
   * function `inspectTypeImpl`.
   */

  //${assertEqualsImpl('expected, 'actual)}
  
  inline def inspectType[A]: TypeDetails = ${inspectTypeImpl}

  def inspectTypeImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[TypeDetails] =
    import quotes.reflect.* 

    /**
     * EXERCISE 6
     * 
     * Capture some details about the representation of the type into the 
     * `TypeDetails` structure. You may find documentation 
     * [here](https://github.com/lampepfl/dotty/blob/master/library/src/scala/quoted/Quotes.scala).
     */
    def inspectTypeRepr(repr: TypeRepr): Expr[TypeDetails] = 
      '{TypeDetails()}

    inspectTypeRepr(TypeRepr.of[A])

  final case class TypeDetails()


  



