import org.apache.spark._
import org.apache.spark.rdd._
import org.apache.spark.graphx._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import scala.collection.mutable.HashMap
import scala.util.{Try,Success,Failure}

object Main extends App {
  if (args.length != 2) {
    println("Usage: pdd3 input_file output_file_name_base")
    System.exit(1)
  }

  val input_file = args(0)
  val output_file = args(1)

  val spark = SparkSession
    .builder()
    .appName("PDD3")
    .getOrCreate()

  import spark.implicits._ 

  var df = spark.read.json(input_file).drop("year")
  df = df.withColumn("dist", size($"authors"))
  df = df.withColumn("authors", explode($"authors")).as("e")
  df = df.join(df.as("e2"), $"e.title" === $"e2.title")
         .select($"e.authors" as "v", $"e2.authors" as "w",
                 when($"e.authors" === $"e2.authors", 0).otherwise($"e.dist") as "d")
         .as("e").groupBy($"v", $"w").agg(min($"d") as "d")

  val vertices = df.select($"v").distinct().withColumn("id", row_number()).as("e2")
  df = df.join(vertices, $"e.v" === $"e2.v").select($"e2.id" as "v", $"e.w" as "w", $"d").as("e")
         .join(vertices, $"e.w" === $"e2.v").select($"e2.id" as "w", $"e.v" as "v", $"d").as("e")

  def isEmpty(dataFrame : DataFrame) : Boolean = {
      Try{dataFrame.first.length != 0} match {
          case Success(_) => false
          case Failure(_) => true
      }
  }

  // Metoda 1
  var res1 = df.as("res1")
  var delta = df.as("delta")

  while (!isEmpty(delta)) {
      val newPaths = delta.join(df.as("e"), $"e.w" === $"delta.v")
                          .select($"e.v" as "v", $"delta.w" as "w", $"e.d" + $"delta.d" as "d")
                          .groupBy($"v", $"w").agg(min($"d") as "d").as("new")

      val notChanged = newPaths.join(res1, Seq("v", "w"))
                               .where($"new.d" >= $"res1.d")
                               .select($"v", $"w", $"new.d" as "d")

      delta = newPaths.except(notChanged)

      res1 = res1.unionAll(delta).groupBy($"v", $"w").agg(min($"d") as "d").as("res1")
  }

  res1.write.option("delimiter", ",").csv(output_file + "res1")

  // Metoda 2
  var res2 = df.as("res2")
  delta = df.as("delta")

  while (!isEmpty(delta)) {
      val newPaths = delta.join(res2.as("e"), $"e.w" === $"delta.v")
                          .select($"e.v" as "v", $"delta.w" as "w", $"e.d" + $"delta.d" as "d")
                          .groupBy($"v", $"w").agg(min($"d") as "d").as("new")

      val notChanged = newPaths.join(res2, Seq("v", "w"))
                               .where($"new.d" >= $"res2.d")
                               .select($"v", $"w", $"new.d" as "d")

      delta = newPaths.except(notChanged)

      res2 = res2.unionAll(delta).groupBy($"v", $"w").agg(min($"d") as "d").as("res2")
  }

  res2.write.option("delimiter", ",").csv(output_file + "res2")

  // Metoda 3
  val graph: Graph[HashMap[Long, Int], Int] =
    Graph(vertices.rdd.map(r => (r.getInt(1).toLong, new HashMap() += (r.getInt(1).toLong -> 0))),
    df.rdd.map(r => Edge(r.getInt(0).toLong, r.getInt(1).toLong, r.getInt(2))))

  val initialMsg: HashMap[Long, Int] = new HashMap()
  def vprog(vertexId: VertexId,
            value: HashMap[Long, Int],
            message: HashMap[Long, Int]): HashMap[Long, Int] = value ++ message

  def sendMsg(triplet: EdgeTriplet[HashMap[Long, Int], Int]): Iterator[(VertexId, HashMap[Long, Int])] = {
    var message: HashMap[Long, Int] = new HashMap()

    for ((k, v) <- triplet.srcAttr)
      if (v + triplet.attr < triplet.dstAttr.getOrElse(k, Int.MaxValue) &&
          v + triplet.attr < message.getOrElse(k, Int.MaxValue))
        message.put(k, v + triplet.attr)

    if (message.size > 0)
      Iterator((triplet.dstId, message))
    else
      Iterator.empty
  }

  def mergeMsg(msg1: HashMap[Long, Int], msg2: HashMap[Long, Int]): HashMap[Long, Int] =
      msg1 ++ msg2.map{ case (k, v) => k -> (v min msg1.getOrElse(k, Int.MaxValue)) }

  val res3 = graph.pregel(initialMsg)(vprog, sendMsg, mergeMsg)
  res3.vertices.flatMap{
    case (vertexId, map) => for ((k, v) <- map) yield (vertexId.toInt, k, v)
  }.toDF.write.option("delimiter", ",").csv(output_file + "res3")
}
