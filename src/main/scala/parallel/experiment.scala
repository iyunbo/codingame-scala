package org.iyunbo.coding
package parallel

class Par[A](a: => A) {
    def get: A = a
}

object Par{
    def unit[A](a: => A): Par[A] = new Par(a)
    def get[A](par: Par[A]): A = par.get
    def map2[A](a: Par[A], b: Par[A])(combine: (A, A) => A) : Par[A] = unit(combine(a.get, b.get))
}


object experiment {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
        if (ints.size <= 1)
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l,r) = ints.splitAt(ints.length / 2)
            Par.map2(sum(l), sum(r))(_ + _)
        }

}
