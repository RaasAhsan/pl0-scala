package com.pl0

sealed trait Token

trait Factor {

	def evaluate(): Int

}

trait Operation {

	def op(a: Int, b: Int): Int = this match {
		case Add => a + b
		case Subtract => a - b
		case Multiply => a * b
		case Divide => a / b
	}

}

trait Testable {

	def test(a: Int, b: Int): Boolean = this match {
		case Greater => a > b
		case GreaterEqual => a >= b
		case Lesser => a < b
		case LesserEqual => a <= b
		case Equals => a == b
		case NotEquals => a != b
	}

}

case object Variable extends Token
case object Constant extends Token
case class Identifier(id: String) extends Token
case class Number(number: Int) extends Token with Factor {

	def evaluate(): Int = {
		number
	}

}

case object Procedure extends Token
case object Call extends Token
case object Begin extends Token
case object End extends Token

case object Comma extends Token
case object Semicolon extends Token
case object Period extends Token

case object Add extends Token with Operation
case object Subtract extends Token with Operation
case object Multiply extends Token with Operation
case object Divide extends Token with Operation

case object LParen extends Token
case object RParen extends Token

case object Equals extends Token with Testable
case object NotEquals extends Token with Testable
case object Greater extends Token with Testable
case object Lesser extends Token with Testable
case object GreaterEqual extends Token with Testable
case object LesserEqual extends Token with Testable

case class Term(factor: Factor, terms: List[(Operation, Factor)]) extends Token with Factor {

	def evaluate(): Int = terms.foldLeft(factor.evaluate)((v, n) => n._1.op(v, n._2.evaluate))

}

case class Expression(sign: Operation, term: Term, exps: List[(Operation, Term)]) extends Token with Factor {

	def evaluate(): Int = exps.foldLeft(sign.op(0, term.evaluate))((v, n) => n._1.op(v, n._2.evaluate))

}
