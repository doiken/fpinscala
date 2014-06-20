def fact_cps(n: Int, cont: Int => Unit): Unit = {
  if(n == 0) cont(1) else fact_cps(n-1, x => cont(n*x))
}

fact_cps(5, x => println(x))

