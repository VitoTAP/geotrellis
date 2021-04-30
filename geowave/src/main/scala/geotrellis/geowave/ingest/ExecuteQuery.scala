/*
 * Copyright 2020 Azavea
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

package geotrellis.geowave.ingest

import cats.effect.Sync
import geotrellis.geowave.dsl._
import org.locationtech.geowave.core.store.api.Query

object ExecuteQuery {
  def delete(queryConfiguration: QueryConfiguration): Query[_] = {
    val query = queryConfiguration.query
    queryConfiguration.dataStore.delete(query)
    query
  }

  def syncDelete[F[_]: Sync](queryConfiguration: QueryConfiguration): F[Query[_]] = Sync[F].delay(delete(queryConfiguration))
}