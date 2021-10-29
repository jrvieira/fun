ID = a => a

COMPOSE = f => g => x => f(g(x))

/* Notes on the Y combinator

   these are equivalent:

   Y = λf.(λx.f(x x))(λx.f(x x))
   Y = λf.(λx.  x x )(λx.f(x x))

   solving Y(φ):

   Y(φ) = (λx.φ(x x))(λx.φ(x x))
        = φ((λx.φ(x x))(λx.φ(x x)))
        = φ(φ(φ...

   Y(φ) = (λx.x x)(λx.φ(x x))
        = (λx.φ(x x))(λx.φ(x x))
        = φ((λx.φ(x x))(λx.φ(x x)))
        = φ(φ(φ...
*/

// Y = f => (x => f(     x(x)   ))(x => f(     x(x)   )) // conventional form
// Y = f => (x =>        x(x)   ) (x => f(     x(x)   )) // simplest form

// Z = f => (x => f(y => x(x)(y)))(x => f(y => x(x)(y))) // conventional
// Z = f => (x => f(     x(x)   ))(x => f(y => x(x)(y))) // 1st wrap is unnecessary
// Z = f => (x =>   y => x(x)(y) )(x => f(y => x(x)(y))) // as is the 1st f
   Z = f => (x =>        x(x)    )(x => f(y => x(x)(y))) // simplest form

/* bool */

TRUE = a => b => a
FALS = a => b => b
// FALS = TRUE(ID)
// CONST

NOT = p => a => b => p(b)(a)
// NOT = p => p(FALS)(TRUE)

XOR = p => q => a => b => p(q(b)(a))(q(a)(b))
// OR = p => q => a => b => p(a)(q(a)(b))
// AND = p => q => a => b => p(q(a)(b))(b)
OR = p => q => p(p)(q)
AND = p => q => p(q)(p)
NOR = p => q => NOT(OR(p)(q))
NAND = p => q => NOT(AND(p)(q))
BEQ = p => q => NOT(XOR(p)(q))
// XNOR

_bool = p => p(true)(false)

/* conditional */

// IF = p => a => b => p(a)(b)
IF = p => p
// IF = ID

/* pair */

PAIR = a => b => f => f(a)(b) // this is a closure
FST = t => t(a => b => a)
SND = t => t(a => b => b)

_pair = t => ({ fst: FST(t), snd: SND(t) })

/* option */

NONE = n => s => n
SOME = x => n => s => s(x)
ISNONE = w => w(TRUE)(_ => FALS)

FROMSOME = w => w(NONE)(ID) // PARTIAL

/* list */

CONS = a => t => f => f(a)(t)
// CONS = a => t => f => f(a)(t)
// CONS = a => t => PAIR(a)(t)
// CONS = PAIR
/*
   PAIR = λabf.f a b
   CONS = λab.PAIR a b
        = λab.(λabf.f a b) a b
        = λab.λf.f a b
        = λabf.f a b
        = PAIR
*/

// canonical church encoded lists are boring

// assuming non-empty lists of the form PAIR(0)(SOME(PAIR(1)(NONE)))
HEAD = FST
TAIL = SND

// assuming lists of the form NONE/SOME(list)
/*
SIZE = l =>
   IF(ISNONE(l))(
      ZERO
   )(
      SUCC(SIZE(TAIL(FROMSOME(l)))) // infinite rec
   )
*/
SIZE = Z(z => l => // Z combinator
   IF(ISNONE(l))(
      ZERO
   )(
      x => SUCC(z(TAIL(FROMSOME(l))))(x)
   )
)

INDEX = l => n => FST(n(COMPOSE(FROMSOME)(SND))(FROMSOME(l)))

/*
MAP = f => l =>
   IF(ISNONE(l))(
      NONE
   )(
      SOME(PAIR(f(FST(FROMSOME(l))))(MAP(f)(SND(FROMSOME(l)))))
   )
*/
MAP = Z(z => f => l =>
   IF(ISNONE(l))(
      NONE
   )(
      x => SOME(PAIR(f(HEAD(FROMSOME(l))))(z(f)(TAIL(FROMSOME(l)))))(x)
   )
)

/*
FOLDL = f => k => l =>
   IF(ISNONE(l))(
      k
   )(
      FOLD(f)(f(k)(HEAD(FROMSOME(l))))(TAIL(FROMSOME(l)))
   )
*/
FOLDL = Z(z => f => k => l =>
   IF(ISNONE(l))(
      k
   )(
      x => z(f)(f(k)(HEAD(FROMSOME(l))))(TAIL(FROMSOME(l)))(x)
   )
)
/*
//
*/
FOLDR = Z(z => f => k => l =>
   IF(ISNONE(l))(
      k
   )(
      x => f(HEAD(FROMSOME(l)))(z(f)(k)(TAIL(FROMSOME(l))))(x)
   )
)

_list = function (t) {
   let l = []
   while (!_bool(ISNONE(t))) {
      t = FROMSOME(t)
      l.push(HEAD(t))
      t = TAIL(t)
   }
   return l
}

list_ = function (l) {
   let t = NONE
   while (l.length) {
      t = SOME(PAIR(l.shift())(t))
   }
   return t
}

/* natural numbers */

ZERO = f => x => x
SUCC = n => f => x => f(n(f)(x))

PRED = n => f => x => n(g => h => h(g(f)))(y => x)(y => y) // wtf

_num = n => n(x => x + 1)(0)

// arithmetic

SUM = n => m => n(SUCC)(m)
// MUL = n => m => n(SUM(m))(ZERO)
MUL = n => m => f => n(m(f))
SUB = n => m => m(PRED)(n)
// POW = n => m => m(MUL(n))(SUCC(ZERO))
POW = n => m => m(n)

ISZERO = n => n(_ => FALS)(TRUE)
// ISZERO = n => n(TRUE(FALSE))(TRUE)

LTE = n => m => ISZERO(SUB(n)(m))
GT = n => m => NOT(LTE(n)(m))
EQ = n => m => AND(LTE(n)(m))(LTE(m)(n))

/* problem
MOD = n => m =>
   IF(LTE(m)(n))(
      MOD(SUB(n)(m))(n) // infinite rec
   )(
      m
   )
*/

/* nor enough
MOD = n => m =>
   IF(LTE(m)(n))(
      x => MOD(SUB(n)(m))(m)(x) // still relies on js explicit recursion
   )(
      n
   )
*/

MOD = Z(z => n => m => // Z combinator
   IF(LTE(m)(n))(
      x => z(SUB(n)(m))(m)(x)
   )(
      n
   )
)

/* util */

num_ = function (n) {
   let x = ZERO
   while (n --) {
      x = SUCC(x)
   }
   return x
}

/* test */

const juzt = require('juzt')

juzt.test('NOT', _bool(NOT(FALS)) === true)
juzt.test('NOT', _bool(NOT(TRUE)) === false)

juzt.test('XOR', _bool(XOR(FALS)(FALS)) === false)
juzt.test('XOR', _bool(XOR(TRUE)(FALS)) === true)
juzt.test('XOR', _bool(XOR(FALS)(TRUE)) === true)
juzt.test('XOR', _bool(XOR(TRUE)(TRUE)) === false)

juzt.test('AND', _bool(AND(FALS)(FALS)) === false)
juzt.test('AND', _bool(AND(TRUE)(FALS)) === false)
juzt.test('AND', _bool(AND(FALS)(TRUE)) === false)
juzt.test('AND', _bool(AND(TRUE)(TRUE)) === true)

juzt.test('NOR', _bool(NOR(FALS)(FALS)) === true)
juzt.test('NOR', _bool(NOR(TRUE)(FALS)) === false)
juzt.test('NOR', _bool(NOR(FALS)(TRUE)) === false)
juzt.test('NOR', _bool(NOR(TRUE)(TRUE)) === false)

juzt.test('NAND', _bool(NAND(FALS)(FALS)) === true)
juzt.test('NAND', _bool(NAND(TRUE)(FALS)) === true)
juzt.test('NAND', _bool(NAND(FALS)(TRUE)) === true)
juzt.test('NAND', _bool(NAND(TRUE)(TRUE)) === false)

juzt.test('BEQ', _bool(BEQ(FALS)(FALS)) === true)
juzt.test('BEQ', _bool(BEQ(TRUE)(FALS)) === false)
juzt.test('BEQ', _bool(BEQ(FALS)(TRUE)) === false)
juzt.test('BEQ', _bool(BEQ(TRUE)(TRUE)) === true)

juzt.test('ZERO', _num(ZERO) === 0)
juzt.test('SUCC', _num(SUCC(ZERO)) === 1)
juzt.test('SUCC', _num(SUCC(SUCC(ZERO))) === 2)
juzt.test('num_', _num(num_(72)) === 72)
juzt.test('SUM', _num(SUM(num_(9))(num_(7))) === 9 + 7)
juzt.test('MUL', _num(MUL(num_(9))(num_(7))) === 9 * 7)
juzt.test('PRED', _num(PRED(num_(9))) === 8)
juzt.test('SUB', _num(SUB(num_(9))(num_(7))) === 9 - 7)
juzt.test('SUB', _num(SUB(num_(7))(num_(9))) === 0)
juzt.test('POW', _num(POW(num_(3))(num_(3))) === 3 ** 3)

juzt.test('ISZERO', _bool(ISZERO(ZERO)) === true)
juzt.test('ISZERO', _bool(ISZERO(SUCC(ZERO))) === false)
juzt.test('ISZERO', _bool(ISZERO(num_(7))) === false)
juzt.test('LTE', _bool(LTE(num_(7))(num_(9))) === true)
juzt.test('LTE', _bool(LTE(num_(9))(num_(9))) === true)
juzt.test('LTE', _bool(LTE(num_(9))(num_(7))) === false)
juzt.test('GT', _bool(GT(num_(7))(num_(9))) === false)
juzt.test('GT', _bool(GT(num_(9))(num_(9))) === false)
juzt.test('GT', _bool(GT(num_(9))(num_(7))) === true)
juzt.test('EQ', _bool(EQ(num_(7))(num_(9))) === false)
juzt.test('EQ', _bool(EQ(num_(9))(num_(9))) === true)
juzt.test('EQ', _bool(EQ(num_(9))(num_(7))) === false)
juzt.test('MOD', _num(MOD(num_(9))(num_(7))) === 9 % 7)
juzt.test('MOD', _num(MOD(num_(7))(num_(7))) === 7 % 7)
juzt.test('MOD', _num(MOD(num_(7))(num_(9))) === 7 % 9)

juzt.test('FST', FST(PAIR(7)(9)) === 7)
juzt.test('SND', SND(PAIR(7)(9)) === 9)

juzt.test('ISNONE', _bool(ISNONE(NONE)) === true)
juzt.test('ISNONE', _bool(ISNONE(SOME(7))) === false)
juzt.test('FROMSOME', _num(FROMSOME(SOME(num_(9)))) === 9)

let list = PAIR(num_(9))(SOME(PAIR(num_(8))(SOME(PAIR(num_(7))(NONE))))) // non empty

juzt.test('HEAD', _num(HEAD(list)) === 9)
juzt.test('TAIL', _num(HEAD(FROMSOME(TAIL(list)))) === 8)
juzt.test('SIZE', _num(SIZE(SOME(list))) === 3) // SIZE takes a possibly empty list
juzt.test('SIZE', _num(SIZE(NONE)) === 0) // SIZE takes a possibly empty list
juzt.test('TAIL', _num(SIZE(TAIL(list))) === 2)
juzt.test('INDEX', _num(INDEX(SOME(list))(num_(2))) === 7)

juzt.test('MAP', _num(HEAD(FROMSOME(MAP(x => SUM(x)(x))(SOME(list))))) === 9+9)
juzt.test('FOLDR', _num(FOLDR(SUM)(ZERO)(NONE)) === 0)
juzt.test('FOLDR', _num(FOLDR(SUM)(ZERO)(SOME(list))) === _list(MAP(_num)(SOME(list))).reduce((x,y) => x+y))
juzt.test('FOLDL', _num(FOLDL(SUM)(ZERO)(NONE)) === 0)
juzt.test('FOLDL', _num(FOLDL(SUM)(ZERO)(SOME(list))) === _list(MAP(_num)(SOME(list))).reduce((x,y) => x+y))


juzt.over()

