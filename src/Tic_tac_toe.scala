import scala.util.Try

/**
  * Created by Indradeep on 21-Aug-17.
  */

object Tic_tac_toe {
  def main(args:Array[String]) =
  {
    val symbol = ('1' to '9').toArray
    val (player1,player2) = display(symbol)
    run_game(player1, player2, symbol)
  }

  def run_game(player1:String, player2:String, sym:Array[Char]): Unit =
  {
    var position_count = 0
    var player_char = 'X'
    val wincombo = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
    for (i <- 0 to sym.length-1)
    {
      if (sym(i) == 'X' || sym(i) == 'O')
        position_count += 1
    }
    if (position_count == sym.length)
      {
        println("All box filled. It's a Draw!")
      }
    else
      {
        if (position_count % 2 == 0)  //For even count, player_char is 'X'
        {
          val new_postion = player_move(player1, player_char, sym)
          sym.update(new_postion - 1, player_char)
        }
        else
        {
          player_char = 'O'
          val new_postion = player_move(player2, player_char, sym)
          sym(new_postion - 1) = player_char
        }
        print_board(sym)
        if (check_win(wincombo, player_char, sym))
        {
          if (player_char == 'X')
          {
            println("Congratulations " + player1 + " ! You have won.")
          }
          else
            println ( "Congratulations " + player2 + " ! You have won.")
        }
        else
        {
          run_game(player1, player2, sym)
        }
        println("Wish to play again: Y/N")
        val choice = readChar()
        if (choice == 'Y' || choice == 'y')
          {
            val symbol = ('1' to '9').toArray
            //print_board(symbol)
            val (player1,player2) = display(symbol)
            run_game(player1, player2, symbol)
          }
        else
          {
            println("Thanks, Good Bye!")
            sys.exit()
          }

      }
  }

  def check_win(win_combo:List[(Int,Int,Int)], player_symbol:Char, sym:Array[Char])=
  {
    win_combo.exists{case(i,j,k) => sym(i) == player_symbol && sym(j) == player_symbol && sym(k) == player_symbol}
  }

  def player_move (player:String, player_symbol:Char, sym:Array[Char]) : Int= {
    println(player + " , choose a box to place an " + player_symbol + " into:")
    val player_move_position = readLine()
    if ( (Try(player_move_position.toInt).isFailure) || (player_move_position.toInt < 0) || (player_move_position.toInt > sym.length) || (sym(player_move_position.toInt - 1) == 'X') || (sym(player_move_position.toInt - 1) == 'O'))
      {
        println("Invalid Move, please choose another box")
        player_move(player, player_symbol, sym)
      }
    else
      {
        player_move_position.toInt
      }
  }


  def print_board(sym:Array[Char]) = {
    sym.grouped(3).foreach( row => println(row(0) + "\t|\t" + row(1) + "\t|\t" + row(2) + "\n-------------------"))
  }

  def display(sym:Array[Char]): (String,String) = {
    println("Enter name for Player 1")
    val player1 = readLine()
    println("Enter name for Player 2")
    val player2 = readLine()
    print_board(sym)
    (player1,player2)
  }


}
