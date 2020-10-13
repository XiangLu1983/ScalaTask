case class SingleGrid(
                       val xPos: Int,
                       val yPos: Int,
                       val validToFill: Boolean,
                       val validToCheck: Boolean,
                       val character: String
                     )

case class wholeBoard(
                       val grids: Seq[SingleGrid],
                       val numberOfValidWords: Int
                     ) {
  def outputByLine(yPos: Int): String = {
    grids.filter(_.yPos == yPos).sortBy(_.xPos).map(_.character) mkString ""
  }
}