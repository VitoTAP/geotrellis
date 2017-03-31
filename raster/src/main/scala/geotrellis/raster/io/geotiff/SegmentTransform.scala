package geotrellis.raster.io.geotiff

private [geotiff] trait SegmentTransform {
  def segmentIndex: Int
  def segmentLayout: GeoTiffSegmentLayout

  def layoutCols: Int = segmentLayout.tileLayout.layoutCols
  def layoutRows: Int = segmentLayout.tileLayout.layoutRows

  def tileCols: Int = segmentLayout.tileLayout.tileCols
  def tileRows: Int = segmentLayout.tileLayout.tileRows

  def layoutCol: Int = segmentIndex % layoutCols
  def layoutRow: Int = segmentIndex / layoutCols

  val (segmentCols, segmentRows) =
    segmentLayout.getSegmentDimensions(segmentIndex)

  /** The col of the source raster that this index represents. Can produce invalid cols */
  def indexToCol(i: Int) = {
    def tileCol = i % tileCols
    (layoutCol * tileCols) + tileCol
  }

  /** The row of the source raster that this index represents. Can produce invalid rows */
  def indexToRow(i: Int) = {
    def tileRow = i / tileCols
    (layoutRow * tileRows) + tileRow
  }

  /** For single band or band interleave */
  def gridToIndex(col: Int, row: Int): Int

  /** For pixel interleave multiband */
  def gridToIndex(col: Int, row: Int, bandOffset: Int): Int
}


private [geotiff] case class StripedSegmentTransform(segmentIndex: Int, bandCount: Int, segmentLayout: GeoTiffSegmentLayout) extends SegmentTransform {
  def gridToIndex(col: Int, row: Int): Int = {
    val tileCol = col - (layoutCol * tileCols)
    val tileRow = row - (layoutRow * tileRows)
    tileRow * segmentCols + tileCol
  }

  def gridToIndex(col: Int, row: Int, bandOffset: Int): Int = {
    val tileCol = col - (layoutCol * tileCols)
    val tileRow = row - (layoutRow * tileRows)
    (tileRow * segmentCols * bandCount) + (tileCol * bandCount) + bandOffset
  }
}

private [geotiff] case class TiledSegmentTransform(segmentIndex: Int, bandCount: Int, segmentLayout: GeoTiffSegmentLayout) extends SegmentTransform {
  def gridToIndex(col: Int, row: Int): Int = {
    val tileCol = col - (layoutCol * tileCols)
    val tileRow = row - (layoutRow * tileRows)
    tileRow * tileCols + tileCol
  }

  def gridToIndex(col: Int, row: Int, bandOffset: Int): Int = {
    val tileCol = col - (layoutCol * tileCols)
    val tileRow = row - (layoutRow * tileRows)
    (tileRow * tileCols * bandCount) + (tileCol * bandCount) + bandOffset
  }
}