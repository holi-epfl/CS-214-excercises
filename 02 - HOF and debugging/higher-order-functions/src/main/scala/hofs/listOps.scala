package hofs

def associative(base: Int, fn: (Int, Int) => Int, l: IntList): Int = 
  if l.isEmpty then base
  else fn(l.head, associative(base, fn, l.tail))

def length(l: IntList): Int = associative(0, (x, acc) => 1 + acc, l)

def produce(l: IntList): Int = associative(1, (x, acc) => x * acc, l)

def countPositive(l: IntList): Int = associative(0, (x, acc) => (if (x > 0) then 1 else 0)+ acc, l)

def sum(l: IntList): Int = associative(0, (x, acc) => x + acc, l)

def countEven(l: IntList): Int = associative(0, (x, acc) => (if x % 2 == 0 then 1 else 0) + acc, l)

def multiplyOdd(l: IntList): Int = associative(1, (x, acc) => (if x % 2 != 0 then x else 1) * acc, l)


def map(fn: Int => Int)(l: IntList): IntList = 
  if l.isEmpty then IntNil()
  else IntCons(fn(l.head), map(fn)(l))

def increment(l: IntList) = map(x => x+1)(l)

def decrement(l: IntList) = map(x => x - 1)(l)

def multiplyBy2(l: IntList) = map(x => 2 * x)(l)

def capAtZero(l: IntList) = map(x => if x < 0 then x else 0)(l)


def filter(fn: Int => Boolean)(l: IntList): IntList = 
  if l.isEmpty then IntNil()
  else if fn(l.head) then IntCons(l.head, filter(fn)(l.tail)) else filter(fn)(l.tail)

def removeOdd(l: IntList) = filter(x => x % 2 == 0)(l)

def removeZeroes(l: IntList) = filter(x => x != 0)(l)

def collectEven(l: IntList) = removeOdd(l)

def collectMultiples(d: Int, l: IntList) = filter(x => x % d ==0)(l)

def intersection(l: IntList, ll: IntList) = filter(x => contains(ll, x))(l)

def difference(l: IntList, ll: IntList) = filter(x => !contains(ll, x))(l)


// Can be replaced by associative?
def foldRight(base: Int, fn: (Int, Int) => Int, l: IntList): Int = 
  if l.isEmpty then base
  else fn(l.head, foldRight(base, fn, l.tail)) 

def horner(x: Int, l: IntList) = foldRight(0, (h, n) => h + x * n, l)


def reduceRight(fn: (Int, Int) => Int)(l: IntList): Int = 
  if l.isEmpty then
    throw IllegalArgumentException("Empty list!")
  else
    if l.tail.isEmpty then l.head
    else fn(l.head, reduceRight(fn)(l.tail))

def substract(l: IntList) = reduceRight((x: Int, n: Int) => x - n)(l)

def last(l: IntList) = reduceRight((x, n) => n)(l)

def min(l: IntList) = reduceRight((x, n) => if x < n then x else n)(l)


def search(exists: Boolean, fn: Int => Boolean)(l: IntList): Boolean =
  if l.isEmpty then !exists
  else 
    if fn(l.head) then exists
    else search(exists, fn)(l.tail)

def allEven(l:IntList) = search(false, x => x % 2 == 0)(l)

def allPositiveOrZero(l: IntList) = search(false, x => x >= 0)(l)

def contains(l:IntList, n:Int) = search(true, x => x == n)(l)

def anyOdd(l: IntList) = search(true, x => x % 2 != 0)(l)

def anyNegative(l: IntList) = search(true, x => x < 0)(l)

def isSubset(l: IntList, ll: IntList) = search(false, x => contains(ll, x))
