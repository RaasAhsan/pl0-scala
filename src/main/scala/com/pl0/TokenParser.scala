import scala.util.parsing.combinator._
import com.pl0._

object TokenParser extends RegexParsers {

	def add: Parser[Operation] = "+" ^^ { _ =>
		Add
	}

	def subtract: Parser[Operation] = "-" ^^ { _ =>
		Subtract
	}

	def multiply: Parser[Operation] = "*" ^^ { _ =>
		Multiply
	}

	def divide: Parser[Operation] = "/" ^^ { _ =>
		Divide
	}

	def number: Parser[Number] = "\\d+".r ^^ { n =>
		Number(n.toInt)
	}

	def leftParen: Parser[Token] = "(" ^^ { _ =>
		LParen
	}

	def rightParen: Parser[Token] = ")" ^^ { _ =>
		RParen
	}

	def termPart: Parser[(Operation, Factor)] = (multiply | divide) ~ factor ^^ { 
		case(op ~ fac) => (op, fac)
	}

	def exprPart: Parser[(Operation, Term)] = (add | subtract) ~ term ^^ {
		case(op ~ fac) => (op, fac)
	}

	def expression: Parser[Expression] = opt(add | subtract) ~ term ~ rep(exprPart) ^^ {
		case(fop ~ fterm ~ exprs) => Expression(fop.getOrElse(Add), fterm, exprs)
	}

	def closedExpression: Parser[Expression] = leftParen ~ opt(add | subtract) ~ term ~ rep(exprPart) ~ rightParen ^^ {
		case(_ ~ fop ~ fterm ~ exprs ~ _) => Expression(fop.getOrElse(Add), fterm, exprs)
	}

	def term: Parser[Term] = factor ~ rep(termPart) ^^ { 
		case(first ~ factors) => Term(first, factors)
	}

	def factor: Parser[Factor] = number | closedExpression

}
