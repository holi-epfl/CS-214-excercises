import recursion.*

// length
def length(l: IntList): Int = 
    if l.isEmpty then 0 else length(l.tail)

//allPositiveOrZero => ReCheck
def allPositiveOrZero(l: IntList): Boolean =
    if l.isEmpty then true else (if l.head <=0 then false else allPositiveOrZero(l.tail))

//countPositive
def countPositive(l: IntList): Int =
    if l.isEmpty then 0 else l.head > 0 + countPositive(l.tail)

//sum
def sum(l: IntList): Int =
    if l.isEmpty then 0 else l.head + sum(l.tail)

//product
def product(l: IntList): Int =
    if 