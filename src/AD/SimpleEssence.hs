module AD.SimpleEssence where

{--

Multiple notions of derivatives, each has a chain rule:
A derivative is a:
- Number
- Vector
- Covector
- Matrix
- Higher derivatives

--}


-- What is a derivative? It's a linear transformation (linear transformation might be a matrix)
D :: (a -> b) -> (a -> (a -o b))

-- (-o) is a linear map
-- consumes a function a to b, produces a function a to linear-map (a to b)

-- where lim eta->0 (||f(a + eta) - (f a + D f a eta)|| / || eta ||) = 0 


-- Each chain rule represents the same composition - all of them are some form of multiplication:
-- composition of linear maps
-- multiplication is really there because it correctly implements composition

-- Sequential Composition (Chain Rule):
(o) :: (b -> c) -> (a -> b) -> (a -> c)
(g `compose` f) a = g (f a)

D (g `compose` f) a = D g (f a) `compose` D f a  -- chain rule


-- Parallel Composition: (Same input) Semantically Parallel, no data dependencies
-- x here is a cross-product? Represents a tuple?
fork :: (a -> c) (a -> d) -> (a -> (c x d))
(f `fork` g) a = (f a, g a)

D (f `fork` g) a = D f a `fork` D g a

-- THE CHAIN RULE IS NON-COMPOSITIONAL
D (g `compose` f) a = D g (f a) `compose` D f a  -- Each term doesn't just require its own derivative, it also requires the value of the function itself
-- Easy fix though, use an extended derivative which combines regular result with derivative (IT'S COMPOSITIONAL)
D` :: (a -> b) -> (a -> (b x (a -o b)))
D` f = f `fork` D f

-- The derivative of a function with respect to itself is the identity function
-- i.e, linear functions are their own perfect linear approximations

D id a = id
D fst a = fst
D snd a = snd

-- for linear functions f:
D` f a = (f a, f)



---------- ABSTRACT ALGEBRA FOR FUNCTIONS --------------
-- Building Blocks for Automatic Differentiation:
-- Linear Functions
-- Mathematical operations (sin, cosine etc)
-- Sequential and Parallel Composition

-- ~> means something is function-like, has a domain and co-domain
class Category (~>) where
  id :: a ~> a
  (o) :: (b ~> c) -> (a ~> b) -> (a ~> c)

-- Laws:
-- Composition is Associative
-- Identity is left and right identity

-- More specialized category: has a notion of products or pairs (x)
class Category (~>) => Cartesian (~>) where
  exl :: (a, b) ~> a -- fst
  exr :: (a, b) ~> b -- snd
  fork :: (a ~> c) -> (a ~> d) -> (a ~> (c, d)) -- domain to pair result
-- Laws:




-- type of differentible functions from a to b
newtype D a b = D (a -> (b, (a -o b)))
D' :: (a -> b) -> D a b
-- this specification is not computable
D' f = D (f `fork` D f)

-- How do we transform the specification to an implementation?

-- SPECIFICATION:
-- We Require D` to preserve Category and Cartesian structure!
-- In the following, right hand side terms are functions, left hand side terms are DIFFERENTIABLE FUNCTIONS)
-- Category structure :
D' id = id
D' (g `compose` f) = D' g `compose` D' f
-- Cartesian structure:
D' exl = exl
D' exr = exr
D' (f `fork` g) = D' f (`fork`) D' g

-- WE HAVE TO SOLVE THESE EQUATIONS FOR THE RHS
-- WHICH IS:
-- ALL THESE TYPES REQUIRE VECTOR-SPACES
newtype D a b = D (a -> (b x (a -o b)))

linearD f = D (\a -> (f a, a))

instance Category D where
  id = linearD id
  D g `compose` D f = D (\a -> let {(b, f') = f a;
                            (c, g') = g b}
                       in (c, g' `compose` f'))

instance Cartesian D where
  exl = linearD exl
  exr = linearD exr
  D f `fork` D g = D (\a ->
                        let {
                          (b, f') = f a
                          (c, g') = g a
                          }
                        in ((b,c), f' `fork` g'))

instance NumCat D where
  negate = linearD negate
  add = linearD add
  mul = D (mul `fork` (
              \(a, b) ->
                \(da, db) ->     -- THIS IS JUST A LINEAR MAP
                  b * da + a * db))


-- EXAMPLES:

sqr :: Num a => a -> a
sqr a = a * a

magSqr :: Num a => a x a -> a
magSqr (a,b) = sqr a + sqr b

cosSinProd :: Floating a => a x a -> a x a
cosSinProd (x, y) = (cos z, sin z) where z = x * y

-- HOW DO WE RE-WRITE IN CATEGORICAL VOCABULARY
-- Computation, Logic and the foundation of mathematics are all the same thing
-- lambda calculus has a category theoretic equivalence to closed-cartesian categories

-- THE COMPILER TRANSFORMS THESE FUNCTIONS TO THIS REPRESENTATION
sqr = mul `compose` (id `fork` id)
magSqr = add `compose` (mul `compose` (exl `fork` exl) `fork` mul `compose` (exr `fork` exr))
cosSinProd = (cos `fork` sin) `compose` mul
