trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
}

class Nil[T] extends List[T] {
    def isEmpty:Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](n: Int, l: List[T]): T = 
    if (l.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) l.head
    else nth(n-1, l.tail)

object List {
    def apply[T]() = new Nil[T]
    def apply[T](x: T) = singleton(x)
    def apply[T](x: T, y: T) = new Cons(x, singleton(y))
}

val s = new Cons(42, singleton(1))
val t = List(1,2)
println(nth(1, t))
