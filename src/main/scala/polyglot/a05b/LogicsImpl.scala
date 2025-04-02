package polyglot.a05b

import polyglot.a05b.Logics
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private var tickCount: Int = 0
  private val random: Random = Random()
  private val initial: (Int, Int) = (random.nextInt(size-2)+1,random.nextInt(size-2)+1)

  override def tick(): Unit =
    tickCount = tickCount + 1

  override def isOver: Boolean = initial match
    case (ix, iy) if iy - tickCount < 0 || iy + tickCount >= size => true
    case (ix, iy) if ix - tickCount < 0 || ix + tickCount >= size => true
    case _ => false

  override def hasElement(x: Int, y: Int): Boolean = initial match
    case (ix,iy) if x == ix && Math.abs(y - iy) <= tickCount => true
    case (ix, iy) if y == iy && Math.abs(x - ix) <= tickCount => true
    case (ix, iy) if x - y == ix - iy && Math.abs(x - ix) <= tickCount => true
    case (ix, iy) if x + y == ix + iy && Math.abs(x -ix) <= tickCount => true
    case _ => false

