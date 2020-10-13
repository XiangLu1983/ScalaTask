import scala.util.Try

object Solution extends InputContext {
  def main(args: Array[String]): Unit = {
    val initLine = 0
    val gridInputs = receiveGridInputs(initLine, Seq.empty[String])
    val wordInputs = receieveWordsInput()
    val initBoard = parsedGridInput(gridInputs)
    for (g <- initBoard.grids) {
      println(s"${g.xPos},${g.yPos},${g.character}")
    }
    val resBoard = fillWords(wordInputs,initBoard).maxBy(_.numberOfValidWords)
    outputBoard(resBoard)
  }

  /** Receive the inputs for the grid lines
   *
   * @param validLines
   * @param receivedInputs
   * @return the sequence of grid line inputs
   */
  def receiveGridInputs(validLines: Int, receivedInputs: Seq[String]): Seq[String] = {
    if (validLines == maxGridLine) {
      receivedInputs
    } else {
      println(s"Please enter ${validLines+1} of ${maxGridLine} line with sign:")
      val inputString = scala.io.StdIn.readLine()
      if (inputString == escpaeString)
        System.exit(0)
      if (validGridInput(inputString)) {
        receiveGridInputs(validLines + 1, receivedInputs :+ inputString)
      } else
        receiveGridInputs(validLines, receivedInputs)
    }
  }

  /** To valid whether the input of grid line
   *
   * @param input
   * @return whether the input is valid or not
   */
  def validGridInput(input: String): Boolean = {
    val charSeq = input.split("").toSeq
    if (charSeq.length != gridSignNumber) {
      println(s"Make sure to enter ${gridSignNumber} number of grids")
      false
    } else if (charSeq.filter(c => (!c.equals(validSign) && !c.equals(invalidSign))).length > 0) {
      println(s"Make sure to enter only valid signs with only ${validSign} or ${invalidSign}")
      false
    } else true
  }

  def receieveWordsInput(): Seq[String] = {
    println(s"Please enter the line with words:")
    val inputString = scala.io.StdIn.readLine()
    if (inputString == escpaeString)
      System.exit(0)
    val wordsSeq = inputString.split(wordSeperate).toSeq
    if (validWordInput(wordsSeq))
      wordsSeq
    else
      receieveWordsInput()
  }

  def validWordInput(inputString: Seq[String]): Boolean = {
    if (inputString.length > maxWords) {
      println(s"Make sure to max number of words input is ${maxWords}")
      false
    } else if (inputString.filter(_.matches(".*[^A-Z].*")).length > 0) {
      println(s"Make sure to only enter A-Z characters for each word")
      false
    } else true
  }

  def parsedGridInput(inputGrid: Seq[String]): wholeBoard = {
    val grids = for (i <- 0 until inputGrid.length;
         j <- 0 until inputGrid(i).split("").length)
      yield SingleGrid(j, i, inputGrid(i).split("")(j) == validSign, inputGrid(i).split("")(j) == validSign, inputGrid(i).split("")(j))
    wholeBoard(grids,0)
  }

  def verifyAvailableGrids(receivedGrids: Seq[SingleGrid]): Seq[Seq[SingleGrid]] = {
    (verifyVerticalAvailableGrids(receivedGrids) ++ verifyHorizaontalAvailableGrids(receivedGrids)).distinct
  }

  def verifyVerticalAvailableGrids(receivedGrids: Seq[SingleGrid]): Seq[Seq[SingleGrid]] = {
    receivedGrids.sortBy(f => (f.xPos, f.yPos)).foldLeft(Seq[Seq[SingleGrid]]()) { (ac, i) =>
      if (ac.isEmpty) {
        Seq(Seq(i))
      } else {
        if (ac.head.head.xPos == i.xPos && ac.head.head.yPos + 1 == i.yPos) {
          (i +: ac.head) +: ac.tail
        } else {
          Seq(i) +: ac
        }
      }
    }.map(_.reverse).reverse
  }

  def verifyHorizaontalAvailableGrids(receivedGrids: Seq[SingleGrid]): Seq[Seq[SingleGrid]] = {
    receivedGrids.sortBy(f => (f.yPos, f.xPos)).foldLeft(Seq[Seq[SingleGrid]]()) { (ac, i) =>
      if (ac.isEmpty) {
        Seq(Seq(i))
      } else {
        if (ac.head.head.yPos == i.yPos && ac.head.head.xPos + 1 == i.xPos) {
          (i +: ac.head) +: ac.tail
        } else {
          Seq(i) +: ac
        }
      }
    }.map(_.reverse).reverse
  }

  def fillWords(words: Seq[String],currentBoard: wholeBoard):Seq[wholeBoard] = {
    words.foldLeft(Seq(currentBoard)){ (bo,w) =>
      if (bo.isEmpty) {
        Seq(currentBoard)
      } else {
        bo.flatMap(fillWord(w,_))
      }
    }
  }

  def fillWord(word: String, currentBoard: wholeBoard): Seq[wholeBoard] ={
    val avaialbleGrids = verifyAvailableGrids(currentBoard.grids.filter(g => g.validToFill || g.validToCheck))
    if (avaialbleGrids.map(g => g.length).max < word.length) {
      Seq(currentBoard)
    } else {
      val availableToUpdateGrids = avaialbleGrids.flatMap(g => getReadyGrids(g,word))
      availableToUpdateGrids.map(updateBoard(currentBoard,_,word)) ++ Seq(currentBoard)
    }
  }

  def getReadyGrids(grids:Seq[SingleGrid],word:String):Seq[Seq[SingleGrid]] = {
    val wordLengths = word.length
    val initCheckGrids = for (i <- 0  to grids.length - wordLengths)
      yield grids.filter(g => grids.indexOf(g) >= i && grids.indexOf(g) < i +wordLengths )
    initCheckGrids.filter(validGrids(_,word).getOrElse(false) )
  }

  def validGrids(grids: Seq[SingleGrid],word:String): Try[Boolean] = Try {
    val pattern = grids.map(g => if(g.validToFill) ".*" else g.character) mkString ""
    word.matches(pattern) && word.length == grids.length
  }

  def updateBoard(currentBoard: wholeBoard, grids:Seq[SingleGrid],word:String):wholeBoard={
    val keepGrids = currentBoard.grids.diff(grids)
    val updateGrids = grids.sortBy(g=>(g.xPos,g.yPos)).map( g => SingleGrid(g.xPos,g.yPos,false,true,word.split("")(grids.indexOf(g))))
    wholeBoard(keepGrids ++ updateGrids, currentBoard.numberOfValidWords + 1)
  }

  def outputBoard(currentBoard: wholeBoard):Unit={
    for (i <- 0 to maxGridLine-1) {
      println(currentBoard.outputByLine(i))
    }
  }
}
