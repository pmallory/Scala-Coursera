def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
}


def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() =>  List(x)
    case y :: ys => if (x <= y) x :: xs
                    else  y :: insert(x, ys)
}

val mylist = List(32, 43, 11, 32)
val sorted = isort(mylist)
println(sorted)



