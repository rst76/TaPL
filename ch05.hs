{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (and, or, not, fst, snd, head, tail, sum)
import Unsafe.Coerce

tru  = \t -> \f -> t
fls  = \t -> \f -> f
test = \l -> \m -> \n -> l m n

and = \b -> \c -> b c fls
or  = \b -> \c -> b tru c
not = \b -> \c -> \d -> b d c

pair = \f -> \s -> \b -> b f s
fst  = \p -> p tru
snd  = \p -> p fls

c0 = \s -> \z -> z
c1 = \s -> \z -> s z

scc = \n -> \s -> \z -> n s (s z)

plus  = \m -> \n -> \s -> \z -> m s (n s z)
times = \m -> \n -> \s -> \z -> m (n s) z
power = \m -> \n -> \s -> \z -> n m s z

iszro = \m -> m (\x -> fls) tru

zz = pair c0 c0
ss = \p -> pair (snd p) (scc (snd p))
prd = \m -> fst (m ss zz) -- n steps for prd c_n

sub = \m -> \n -> \s -> \z -> n (unsafeCoerce prd) m s z

equal = \m -> \n -> and (iszro (sub n m)) (iszro (unsafeCoerce sub m n))

churchnat 0 = c0
churchnat n = scc (churchnat (n-1))

-- fold = \t -> \c -> \n -> t c n
nil   = \h -> \t -> t
cons  = \h -> \t -> \c -> \n -> c h (t c n)
head  = \t -> t tru (error "empty list")
isnil = \t -> t (\x -> \y -> fls) tru
tail  = \t -> fst (t (\h -> \t -> pair (snd t) (cons h (snd t))) (pair nil nil))

fix = \f -> (\x -> f (\y -> x (unsafeCoerce x) y)) (\x -> f (\y -> x (unsafeCoerce x) y))

g = \fct -> \n -> test (iszro $ unsafeCoerce  n) c1 (times n (fct (prd $ unsafeCoerce n)))
factorial = fix g

h = \sm -> \t -> test (isnil $ unsafeCoerce t) c0 (plus (head t) (sm (tail $ unsafeCoerce t)))
sum = fix h
