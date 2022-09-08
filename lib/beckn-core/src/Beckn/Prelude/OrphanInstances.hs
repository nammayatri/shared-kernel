{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Prelude.OrphanInstances where

import EulerHS.Language ()
import GHC.Generics

-- deriving instance Generic (a,b,c,d,e,f,g,h) -- uncomment this when remove EulerHS
deriving instance Generic (a, b, c, d, e, f, g, h, i)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

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
