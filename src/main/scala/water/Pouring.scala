package org.iyunbo.coding
package water

class Pouring(capacity: Vector[Int]) {

  type State = Vector[Int]

  val initialState: State = capacity.map(_ => 0)

  trait Move {
    def next(current: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def next(current: State): State = current updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def next(current: State): State = current updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def next(current: State): State = {
      val amount = current(from) min capacity(to) - current(to)
      current updated(from, current(from) - amount) updated(to, current(to) + amount)
    }
  }

  val glasses: Seq[Int] = Seq.range(0, capacity.size)

  val moves: Seq[Move] = (for (glass <- glasses) yield Empty(glass)) ++
    (for (glass <- glasses) yield Fill(glass)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  case class Path(history: List[Move], endState: State) {

    def extend(move: Move): Path = Path(move :: history, move next endState)

    override def toString: String = (history.reverse mkString " ") + " -> " + endState
  }


  val initialPath: Path = Path(Nil, initialState)

  val pathSets: LazyList[Set[Path]] = from(Set(initialPath), Set(initialState))

  def from(paths: Set[Path], visited: Set[State]): LazyList[Set[Path]] = {
    if (paths.isEmpty) LazyList.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(visited contains next.endState)
      } yield next
      LazyList.cons(paths, from(more, visited ++ (more map (_.endState))))
    }
  }

  def solution(target: Int): LazyList[Path] = for {
    pathSet <- pathSets
    path <- pathSet
    if path.endState contains target
  } yield path

}
