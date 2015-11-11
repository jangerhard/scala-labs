package org.scalalabs.basic.lab01
import scala.language.implicitConversions
/**
 * The goal of this exercise is to get familiar basic OO constructs in scala
 *
 * Fix the code so that the unit test 'CurrencyExerciseTest' passes.
 *
 */

abstract class Currency(val symbol: String)

class Euro(val euro: Int, val cents: Int = 0) extends Currency("EUR") with Ordered[Euro] {
  lazy val inCents: Int = euro * 100 + cents

  def +(other: Euro): Euro = Euro.fromCents(inCents + other.inCents)
  def *(n: Int): Euro = Euro.fromCents(n * inCents)
  def /(divider: Int): Euro = {
    if (divider <= 0) throw new IllegalArgumentException("Divider must be greater than 0!")
    Euro.fromCents(inCents / divider)
  }

  override def toString: String = {
    if (cents == 0)
      symbol + ": " + euro + ",--"
    else if (cents < 10)
      symbol + ": " + euro + ",0" + cents
    else
      symbol + ": " + euro + "," + cents
  }

  override def compare(that: Euro) = this.inCents - that.inCents
}

object Euro {
  def fromCents(cents: Int) = new Euro(cents / 100, cents % 100)

  implicit class EuroInt(val i: Int) {
    def *(euro: Euro) = euro * i
  }

  //implicit def FromDollar(dollar: Dollar): Euro = Euro.fromCents(DefaultCurrencyConverter.toEuroCents(dollar.inCents))
  implicit def implicitFromDollar(dollar: Dollar)(implicit anotherConverter: CurrencyConverter): Euro = Euro.fromCents(anotherConverter.toEuroCents(dollar.inCents))

}

class Dollar(val dollar: Int, val cents: Int = 0) extends Currency("USD") with Ordered[Dollar] {
  lazy val inCents: Int = dollar * 100 + cents

  override def compare(that: Dollar) = this.inCents - that.inCents

}

/*
 * Exercise 5:
 * - Extend the conversion method from Euro to Dollar with an implicit parameter
 *   of type [[org.scalalabs.basic.lab01.CurrencyConverter]]
 * - Use the implicit CurrencyConverter to do the conversion.
 */
