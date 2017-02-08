import week3.Rational

object scratch {
    def main(args: Array[String]): Unit = {
        val r = new Rational(1, 2)
        println(r)

        def error(msg: String) = throw new Error(msg)

        error("the system is down!")
    }
}
