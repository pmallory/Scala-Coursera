def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => List(y) ::: init(ys)
}

def removeAt[T](n: Int, xs: List[T]):List[T] = xs match {
    case List() => xs
    case y :: ys => { if (n == 0) ys
                      else y :: removeAt(n-1, ys)

    }
}

def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (xs, Nil) => xs
	case (Nil, ys) => ys
	case (x :: xs1, y :: ys1) => if (x < y) x :: merge (xs1, ys)
                                 else y :: merge(xs, ys1)
  }

println(merge(List(1,3,5), List(2,4,6)))


