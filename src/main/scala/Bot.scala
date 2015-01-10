/**
 * Contains work from the Scalatron project at http://scalatron.github.com .
 */

import scala.util.Random

object ControlFunction {

}



class ControlFunctionFactory {

    private val pathFinder = new PathFinder()


    private def normalBehaviour(params: Map[String,String], view: View, lastDirection: Option[XY]) = {
        val directionToNearest: Option[XY] = view.offsetToNearest(Food())
        val dir: XY = directionToNearest.getOrElse(randomWalk(lastDirection))

        val moveCommand: String = pathFinder.findPath(view, dir)
        val spawnCommand:String =
        if(params("energy").toInt>200){
            "|Spawn(direction=" + dir + ",energy=200)"
        } else ""
        moveCommand+spawnCommand

    }


    def analyzeView(params: Map[String,String], lastDirection: Option[XY]) = {


        val view = View(params("view"))
        params("generation") match{
            case "0" => normalBehaviour(params, view, lastDirection)

            case _ =>{//bots
                val possibleTarget = view.offsetToNearest(Target()).map(_.distanceTo(XY.Zero) <= 5)
                if(!possibleTarget.isEmpty && possibleTarget.get) {
                    "Explode(size=5)"
                } else {
                    normalBehaviour(params, view, lastDirection)
                }
            }
        }

    }

    private def randomWalk(lastDirection: Option[XY]) = {
        lastDirection.getOrElse(Random.shuffle(XY.directions).head)
    }



    def create = (input: String) => {

        def parseLastDirection(params: Map[String,String]): Option[XY] = {
            params.get("lastDirection").map(XY(_))
        }

        val (opcode, params) = CommandParser(input)
        opcode match {
            case "React" => analyzeView(params,parseLastDirection(params))//+"|Say(text=OMNOMNOM)"
            case _ => "" // OK
        }
    }



}

class PathFinder {

    def findPath(view:View, destination: XY) = {
        var direction = destination.signum
        val orgDirection = direction
        var first = true
        while(view.objAtRelPos(direction).kinds.contains(Damaging())
          && (first || orgDirection == direction)){
            direction = direction.rotateClockwise45
            first = false
        }

        toCommand(direction)
    }


    private def toCommand(xy: XY) = {
        val sig = xy.signum
        val dirString = sig.x+":"+sig.y
        "Move(direction="+dirString+")|Set(lastDirection="+dirString+")"
    }

}

// -------------------------------------------------------------------------------------------------


/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
    /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
    def apply(command: String): (String, Map[String, String]) = {
        /** "key=value" => ("key","value") */
        def splitParameterIntoKeyValue(param: String): (String, String) = {
            val segments = param.split('=')
            (segments(0), if(segments.length>=2) segments(1) else "")
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)
        val opcode = segments(0)
        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
        (opcode, keyValuePairs)
    }
}


// -------------------------------------------------------------------------------------------------


/** Utility class for managing 2D cell coordinates.
  * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
  * The direction (1,-1) points right and up.
  */
case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x != 0 || y != 0
    def isZero = x == 0 && y == 0
    def isNonNegative = x >= 0 && y >= 0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x + dx, y)
    def addToY(dy: Int) = XY(x, y + dy)

    def +(pos: XY) = XY(x + pos.x, y + pos.y)
    def -(pos: XY) = XY(x - pos.x, y - pos.y)
    def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

    def distanceTo(pos: XY): Double = (this -
      pos).length // Phythagorean
    def length: Double = math.sqrt(x * x + y * y) // Phythagorean

    def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
    def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)

    /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
    def toDirection45: Int = {
        val unit = signum
        unit.x match {
            case -1 =>
                unit.y match {
                    case -1 =>
                        if(x < y * 3) Direction45.Left
                        else if(y < x * 3) Direction45.Up
                        else Direction45.UpLeft
                    case 0 =>
                        Direction45.Left
                    case 1 =>
                        if(-x > y * 3) Direction45.Left
                        else if(y > -x * 3) Direction45.Down
                        else Direction45.LeftDown
                }
            case 0 =>
                unit.y match {
                    case 1 => Direction45.Down
                    case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
                    case -1 => Direction45.Up
                }
            case 1 =>
                unit.y match {
                    case -1 =>
                        if(x > -y * 3) Direction45.Right
                        else if(-y > x * 3) Direction45.Up
                        else Direction45.RightUp
                    case 0 =>
                        Direction45.Right
                    case 1 =>
                        if(x > y * 3) Direction45.Right
                        else if(y > x * 3) Direction45.Down
                        else Direction45.DownRight
                }
        }
    }

    def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
    def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
    def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
    def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


    def wrap(boardSize: XY) = {
        val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
        val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
        if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
    }

}


object XY {
    /** Parse an XY value from XY.toString format, e.g. "2:3". */
    def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

    val Zero = XY(0, 0)
    val One = XY(1, 1)

    val Right     = XY( 1,  0)
    val RightUp   = XY( 1, -1)
    val Up        = XY( 0, -1)
    val UpLeft    = XY(-1, -1)
    val Left      = XY(-1,  0)
    val LeftDown  = XY(-1,  1)
    val Down      = XY( 0,  1)
    val DownRight = XY( 1,  1)

    val directions = List(Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight, Zero)

    def fromDirection45(index: Int): XY = index match {
        case Direction45.Right => Right
        case Direction45.RightUp => RightUp
        case Direction45.Up => Up
        case Direction45.UpLeft => UpLeft
        case Direction45.Left => Left
        case Direction45.LeftDown => LeftDown
        case Direction45.Down => Down
        case Direction45.DownRight => DownRight
    }

    def fromDirection90(index: Int): XY = index match {
        case Direction90.Right => Right
        case Direction90.Up => Up
        case Direction90.Left => Left
        case Direction90.Down => Down
    }



    def apply(array: Array[Int]): XY = XY(array(0), array(1))
}


object Direction45 {
    val Right = 0
    val RightUp = 1
    val Up = 2
    val UpLeft = 3
    val Left = 4
    val LeftDown = 5
    val Down = 6
    val DownRight = 7
}


object Direction90 {
    val Right = 0
    val Up = 1
    val Left = 2
    val Down = 3
}


// -------------------------------------------------------------------------------------------------


case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt
    val center = XY(size / 2, size / 2)

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

    def objAtRelPos(relPos: XY) = MapObject(relPos, charToTraits(cellAtRelPos(relPos)))

    def offsetToNearest(k: Kind): Option[XY] = {
        val matchingXY = cells.view.zipWithIndex.filter(c => kindToChars(k).contains(c._1  ))
        if( matchingXY.isEmpty )
            None
        else {
            val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
            Some(nearest)
        }
    }



    def indexToMapObject(index: Int) = {
        val coords = relPosFromIndex(index)
        MapObject(coords, charToTraits(cellAtRelPos(coords)))
    }

    private def charToTraits(c: Char): Set[Kind] = charToKinds(c)

    private val charToKinds = Map[Char, Set[Kind]](
        'b' -> Set[Kind](Damaging(), Impassable(), Mobile()),
        'p' -> Set(Damaging(), Impassable()),
        'W' -> Set(Damaging(), Impassable()),
        'm' -> Set(Impassable(), Target()),
        'M' -> Set(Impassable()),
        's' -> Set(Damaging(), Impassable(), Mobile(), Target()),
        'P' -> Set(Food()),
        'B' -> Set(Food(), Mobile())
    ).withDefaultValue(Set.empty)

    private val kindToChars = charToKinds.toList.flatMap {
        case (char, kinds) => kinds.map((_, char))
    }.groupBy {
        case (kind, char) => kind
    }.map {
        case (kind, pairs) => (kind, pairs.toMap.values.toSet)
    }

}


sealed trait Kind {

}

trait AffectingPower extends Kind {

}

case class Food() extends AffectingPower

case class Damaging() extends AffectingPower

case class Impassable() extends Kind

case class Mobile() extends Kind

case class Target() extends Kind


case class MapObject(coordinates: XY, kinds: Set[Kind]) {


}
