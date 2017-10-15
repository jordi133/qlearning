package connectfour

import java.io._
import java.nio.ByteBuffer

import qlearning.QMatrix

import scala.util.Try

/**
  * Trains to play ConnectFour and then saves the resulting Q Matrix to a file
  */
object FileHandler {

  def writeMatrixToFile(matrix: QMatrix[Long, Int], filename: String): Try[Unit] = Try {
    val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))

    // First write nr of states
    out.writeInt(matrix.size)
    for ((state, actionToQMap) <- matrix) {
      // for every state, write the long representing the state and the size of the map of actions to q values
      out.writeLong(state)
      out.writeInt(actionToQMap.size)
      for ((action, q) <- actionToQMap) {
        out.writeInt(action)
        out.writeDouble(q)
      }
    }
    out.close()
  }

  def readMatrixFromFile(filename: String): Try[QMatrix[Long, Int]] = {
    import java.io.FileInputStream
    val file = new File(filename)
    val fis = new FileInputStream(filename)
    val bytes = new Array[Byte](file.length.toInt)
    fis.read(bytes)

    val bb = ByteBuffer.wrap(bytes)

    def readActionMap(in: ByteBuffer): Map[Int, Double] = {
      val actionCount = in.getInt()
      val actionToQMap: Map[Int, Double] = (for (_ <- 0 until actionCount) yield {
        val action = in.getInt()
        val q = in.getDouble()
        action -> q
      }).toMap
      actionToQMap
    }

    Try {
      val nrOfStates = bb.getInt()
      println(s"Reading trained player from $filename")

      // Use a var for performance reasons
      var result: Map[Long, Map[Int, Double]] = Map.empty

      for (i <- 0 until nrOfStates) yield {
        val state = bb.getLong()
        if (i % (nrOfStates / 100) == 0) {
          println(s"Progress: ${i / (nrOfStates / 100)}%")
        }
        result += state -> readActionMap(bb)
      }

      fis.close()

      result
    }
  }
}