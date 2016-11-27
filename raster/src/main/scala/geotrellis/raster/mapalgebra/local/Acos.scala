/*
 * Copyright 2016 Azavea
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.raster.mapalgebra.local

import geotrellis.raster._

/**
 * Operation to get the arc cosine of values.
 * Always returns a double tiled raster.
 * If the absolute value of the cell value is > 1, it will be NaN.
 */
object Acos extends Serializable {
  def apply(r: Tile): Tile =
    (if(r.cellType.isFloatingPoint) r
     else r.convert(DoubleConstantNoDataCellType))
     .mapDouble(z => math.acos(z))
}