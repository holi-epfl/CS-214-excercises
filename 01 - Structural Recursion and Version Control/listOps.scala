package recursion

def length(l: IntList): Int = 
    if l.isEmpty then 0 else length(l.tail)

// please recheck
def allPositiveOrZero(l: IntList): Boolean =
    if l.isEmpty then true else (if l.head <=0 then false else allPositiveOrZero(l.tail))

def countPositive(l: IntList): Int =
    def isHeadPositive = if l.head > 0 then 1 else 0
    if l.isEmpty then 0 else isHeadPositive + countPositive(l.tail)

def sum(l: IntList): Int =
    if l.isEmpty then 0 else l.head + sum(l.tail)

def product(l: IntList): Int =
    if l.isEmpty then 1 else l.head * product(l.tail)

def anyOdd(l: IntList): Boolean = 
    if l.isEmpty then false else ((l.head % 2) || anyOdd(l.tail))

def decrement(l: IntList): IntList =
    if l.isEmpty then l else IntCons(l.head-1, decrement(l.tail))

def collectEven(l: IntList): IntList =
    if l.isEmpty then 0 else (l.head % 2 == 1) + collectEven(l.tail)

def min(l: IntList): Int =
  ???

def increment(l: IntList): IntList =
  ???

def subtract(l: IntList): Int =
  ???

def removeOdd(l: IntList): IntList =
  ???

def countEven(l: IntList): Int =
  ???

/** `countEven` using `collectEven` and `length` */
def countEven2(l: IntList): Int =
  ???

def multiplyBy2(l: IntList): IntList =
  ???

def anyNegative(l: IntList): Boolean =
  ???

def allEven(l: IntList): Boolean =
  ???

def multiplyOdd(l: IntList): Int =
  ???

def horner(x: Int, l: IntList): Int =
  ???

def capAtZero(l: IntList): IntList =
  ???

def removeZeroes(l: IntList): IntList =
  ???

def reverseAppend(l1: IntList, l2: IntList): IntList =
  ???

def reverse(l: IntList): IntList =
  ???

def takeWhilePositive(l: IntList): IntList =
  ???

def append(l1: IntList, l2: IntList): IntList =
  ???

def collectMultiples(d: Int, l: IntList): IntList =
  ???

def last(l: IntList): Int =
  ???

def init(l: IntList): IntList =
  ???

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
