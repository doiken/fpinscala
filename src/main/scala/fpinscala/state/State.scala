package fpinscala.state

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}
case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =  {
    State(s => {
      val (a:A, s2:S) = run(s)
      val state:State[S, B] = f(a)
      state.run(s2)

      // 引数内ではState[S, B]を返すのではなく、
      // State[S, B]となるような(A, S)を返す
    })
  }
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      aa <- this
      bb <- sb
    } yield f(aa, bb)
  }
  def sequence(a: List[State[A]]): State[List[A]] = a match {

  }
}
