--The "MIT" License

--Permission is hereby granted, free of charge, to any person obtaining
--a copy of this software and associated documentation files (the
--"Software"), to deal in the Software without restriction, including
--without limitation the rights to use, copy, modify, merge, publish,
--distribute, sublicense, and/or sell copies of the Software, and to
--permit persons to whom the Software is furnished to do so, subject to
--the following conditions:

--The above copyright notice and this permission notice shall be
--included in all copies or substantial portions of the Software.

--THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
--NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
--LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
--OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
--WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-- Downloaded from http://svn.openfoundry.org/pugs/src/Data/DeepSeq.hs on December 6 2007

module Data.DeepSeq where

class DeepSeq a where
    deepSeq :: a -> b -> b

infixr 0 `deepSeq`, $!!

($!!) :: (DeepSeq a) => (a -> b) -> a -> b
f $!! x = x `deepSeq` f x

instance  DeepSeq ()  where  deepSeq = seq

instance  DeepSeq Bool  where  deepSeq = seq
instance  DeepSeq Char  where  deepSeq = seq

instance  (DeepSeq a) => DeepSeq (Maybe a)  where
    deepSeq Nothing  y = y
    deepSeq (Just x) y = deepSeq x y

instance  (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)  where
    deepSeq (Left  a) y = deepSeq a y
    deepSeq (Right b) y = deepSeq b y

instance  DeepSeq Ordering  where  deepSeq = seq

instance  DeepSeq Int       where  deepSeq = seq
instance  DeepSeq Integer   where  deepSeq = seq
instance  DeepSeq Float     where  deepSeq = seq
instance  DeepSeq Double    where  deepSeq = seq

instance  DeepSeq (a -> b)  where  deepSeq = seq

instance  DeepSeq (IO a)  where  deepSeq = seq

instance  (DeepSeq a) => DeepSeq [a]  where
    deepSeq []     y = y
    deepSeq (x:xs) y = deepSeq x $ deepSeq xs y

instance  (DeepSeq a,DeepSeq b) => DeepSeq (a,b)  where
    deepSeq (a,b)           y = deepSeq a $ deepSeq b y

instance  (DeepSeq a,DeepSeq b,DeepSeq c) => DeepSeq (a,b,c)  where
    deepSeq (a,b,c)         y = deepSeq a $ deepSeq b $ deepSeq c y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) => DeepSeq (a,b,c,d)  where
    deepSeq (a,b,c,d)       y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e) => DeepSeq (a,b,c,d,e)  where
    deepSeq (a,b,c,d,e)     y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f) => DeepSeq (a,b,c,d,e,f)  where
    deepSeq (a,b,c,d,e,f)   y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f,DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)  where
    deepSeq (a,b,c,d,e,f,g) y = deepSeq a $ deepSeq b $ deepSeq c $ deepSeq d $ deepSeq e $ deepSeq f $ deepSeq g y

