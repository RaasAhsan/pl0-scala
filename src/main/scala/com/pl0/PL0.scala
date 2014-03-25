package com.pl0

import com.pl0._

object PL0 extends App {

	val t = Term(Number(10), List((Multiply, Number(5)), (Add, Number(5))))
	println(t.evaluate)

}
