def balance(chars: List[Char]): Boolean = {
  def f(num_open: Int, chars: List[Char]): Boolean = {
    if(chars.isEmpty) return num_open == 0
    val n = num_open + (chars.head match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    })
    if(n < 0) false
    else f(n, chars.tail)
  }
  f(0, chars)
}

balance("asdf()()()()()asdf".toList)

def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0) 1
  else if(money > 0 && !coins.isEmpty)
    countChange(money-coins.head, coins) + countChange(money, coins.tail)
  else 0
}

countChange(6, List(1,2,3))

def pascal(c: Int, r: Int): Int = {
  if(c==0) return 1
  if(r == 0) 0
  else pascal(c, r - 1) + pascal(c - 1, r - 1)
}

pascal(1,2)

  for (row <- 0 to 10) {
    for (col <- 0 to row)
      print(pascal(col, row) + " ")
    println()
  }
