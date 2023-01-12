{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kernel.Prelude.OrphanInstances where

import EulerHS.Language ()
import GHC.Generics
import qualified Data.OpenApi as DS
import Control.Lens (at, ix, Index, IxValue, At, Ixed, coerced, Lens')
import Data.Text (Text)
import EulerHS.Prelude ((.))

-- deriving instance Generic (a,b,c,d,e,f,g,h) -- uncomment this when remove EulerHS
-- deriving instance Generic (a, b, c, d, e, f, g, h, i)

{-

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j)

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k)

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l)

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

-- deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

-}

-- Index and IxValue Type Family instances for SecurityDefinitions
type instance Index DS.SecurityDefinitions = Text
type instance IxValue DS.SecurityDefinitions = DS.SecurityScheme

-- Ixed and At Type-Class instances for SecurityDefinitions
instance Ixed DS.SecurityDefinitions where ix n = (coerced :: Lens' DS.SecurityDefinitions (DS.Definitions DS.SecurityScheme)). ix n
instance At   DS.SecurityDefinitions where at n = (coerced :: Lens' DS.SecurityDefinitions (DS.Definitions DS.SecurityScheme)). at n
