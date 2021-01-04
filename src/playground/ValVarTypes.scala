//object ValVarTypes extends App {
//  val x = 42
//  println(x)
//}

//Expression

object ValVarTypes extends App {
  val ifExpression = if (false) 5 else 3
  println("if exxpression", ifExpression)

  //while return Unit
  var i: Int = 0
  var awhile = while (i <= 5) {
    println(i)
    i += 1
  }
  println("awhile side effect that return Unit", awhile)

  var var_ : Int = 130396
  var aReAssign = (var_ = 861962)
  println("side effect reasign return Unit", aReAssign)

  val aPrint = println(var_)
  println("aPrint side effect that return Unit", aPrint)

  //block code
  var aCodeBlock = {
    var nghia: String = "Electrical Engineering - UET"
    var betterNghia = nghia + " to aspiring Data Engineer"

    if (betterNghia.length() > nghia.length()) betterNghia else nghia

  }
  println("Nghia's CodeBlock", aCodeBlock)
  //exercise
  // "Hello World" vs println("Hello World")
  // --> first is object String vs second is expression return UNit

  val falseBlock = {
    2 > 3
  }

  val unknownBlock = {
    if (falseBlock) 156 else 220
    42
  }

  println("may be false block: ", falseBlock)
  println(
    "Can 42 be last expression that return what block will: ",
    unknownBlock
  )
}
