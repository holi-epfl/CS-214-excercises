package recursion

def length(l: IntList): Int = 
    if l.isEmpty then 0 else length(l.tail) + 1 // Remember +1

// please recheck
def allPositiveOrZero(l: IntList): Boolean =
    if l.isEmpty then true else (if l.head <0 then false else allPositiveOrZero(l.tail)) // can simply use (l.head >= 0 && ...)

def countPositive(l: IntList): Int =
    def isHeadPositive = if l.head > 0 then 1 else 0
    if l.isEmpty then 0 else isHeadPositive + countPositive(l.tail)

def sum(l: IntList): Int =
    if l.isEmpty then 0 else l.head + sum(l.tail)

def product(l: IntList): Int =
    if l.isEmpty then 1 else l.head * product(l.tail)

def anyOdd(l: IntList): Boolean = 
    if l.isEmpty then false else ((l.head % 2 != 0) || anyOdd(l.tail)) // Must treat int and bool differently!!!

def decrement(l: IntList): IntList =
    if l.isEmpty then l else IntCons(l.head-1, decrement(l.tail))

def collectEven(l: IntList): IntList =
    if l.isEmpty then l 
    else
      if l.head % 2 == 0 then IntCons(l.head, collectEven(l.tail))
      else collectEven(l.tail)

def min(l: IntList): Int = 
  if l.tail.isEmpty then l.head
  else
    val tailMin = min(l.tail)
    if tailMin < l.head then tailMin else l.head

def increment(l: IntList): IntList =
  if l.isEmpty then l else IntCons(l.head + 1, increment(l.tail))

def subtract(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else
    if l.tail.isEmpty then l.head else l.head - subtract(l.tail)

def removeOdd(l: IntList): IntList =
  if l.isEmpty then l
  else
    if l.head % 2 != 0 then removeOdd(l.tail)
    else IntCons(l.head, removeOdd(l.tail))

def countEven(l: IntList): Int =
  if l.isEmpty then 0
  else 
    if l.head % 2 == 0 then 1 + countEven(l.tail)
    else countEven(l.tail)

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  val lEven = removeOdd(l)
  length(lEven)

def multiplyBy2(l: IntList): IntList =
  if l.isEmpty then l else IntCons(2 * l.head, l.tail)

def anyNegative(l: IntList): Boolean =
  if l.isEmpty then false else (l.head < 0 || anyNegative(l.tail))

def allEven(l: IntList): Boolean =
  if l.isEmpty then true else (l.head % 2 ==0 && allEven(l.tail))

def multiplyOdd(l: IntList): Int =
  if l.isEmpty then 1
  else
    if l.head % 2 == 0 then multiplyOdd(l.tail)
    else l.head * multiplyOdd(l.tail)

def horner(x: Int, l: IntList): Int =
  if l.isEmpty then 0
  else l.head + x * horner(x, l.tail)


def capAtZero(l: IntList): IntList =
  if l.isEmpty then l else IntCons(if l.head < 0 then l.head else 0, capAtZero(l.tail))

def removeZeroes(l: IntList): IntList =
  ???

def reverseAppend(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else reverseAppend(l1.tail, IntCons(l1.head, l2))

def reverse(l: IntList): IntList =
  reverseAppend(l, IntNil())

def takeWhilePositive(l: IntList): IntList =
  if l.isEmpty then l
  else
    if l.head > 0 then IntCons(l.head, takeWhilePositive(l.tail))
    else IntNil()

def append(l1: IntList, l2: IntList): IntList =
  if l1.isEmpty then l2
  else IntCons(l1.head, append(l1.tail, l2))

def collectMultiples(d: Int, l: IntList): IntList =
  if l.isEmpty then l
  else
    if l.head % d == 0 then IntCons(l.head, collectMultiples(d, l.tail))
    else collectMultiples(d, l.tail)

def last(l: IntList): Int =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else
    if l.tail.isEmpty then l.head
    else last(l.tail)

def init(l: IntList): IntList =
  if l.isEmpty then throw IllegalArgumentException("Empty list!")
  else
    if l.tail.isEmpty then IntNil()
    else IntCons(l.head, init(l.tail))

def contains(l: IntList, n: Int): Boolean =
  ???

def isSubset(l: IntList, L: IntList): Boolean =
  ???

def intersection(l: IntList, L: IntList): IntList =
  ???

def difference(l: IntList, L: IntList): IntList =
  ???

def minMax(l: IntList): (Int, Int) =
  ???

val Add = -1
val Multiply = -2

def polishEval(l: IntList): (Int, IntList) =
  ???
// wed 3 p.m.