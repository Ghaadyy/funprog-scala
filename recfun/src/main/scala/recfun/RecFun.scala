package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c == 0 || c == r then 1 
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def countBalance(chars: List[Char], count: Int): Boolean = 
      if chars.isEmpty then count == 0
      else if chars.head == '(' then count >= 0 && countBalance(chars.tail, count+1) 
      else if chars.head == ')' then count >= 0 && countBalance(chars.tail, count-1) 
      else countBalance(chars.tail, count)

    countBalance(chars, 0)
   
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if coins.isEmpty || money == 0 || money < coins.head then 0
    else if money == coins.head then 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)