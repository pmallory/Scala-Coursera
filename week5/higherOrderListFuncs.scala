def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs => {
    val sublists = xs span (n => n == x)
    (x :: sublists._1) :: pack(sublists._2)
  }
}

def encode[T](xs: List[T]): List[Tuple2[T, Int]] = xs match {
  case Nil => Nil
  case x :: xs => {
    val (first, rest)= xs span (n => n == x)
    (x, first.length+1) :: encode(rest)
  }
}

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

println(mapFun(List(1,2,3), (x: Int)=>2*x))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((a, b) => 1 + b)

println(lengthFun(List(1,2,4,5,6,5,5,5)))
