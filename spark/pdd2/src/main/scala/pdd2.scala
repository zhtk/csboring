import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD
import scala.collection.Searching._
import scala.util.Random

object Main extends App {
    def podziel(serwery : Int, lista : RDD[Int]) = {
        val size = lista.count().toInt
        
        // Losowe granice
        val granice = lista.takeSample(false, serwery - 1, System.nanoTime.toInt).sortWith(_ < _)

        // Policzenie rozmiarow kubelkow
        val rozmiary = lista.map(v => {
            val kub = granice.search(v) match {
                case InsertionPoint(x) => x
                case Found(x) => x
            };
            (kub, v)
        }).countByKey.toList.sortWith(_._1 < _._1)
    
        // Wyliczenie nowych granic
        val granice2 = lista.map(v => {
            val kub = granice.search(v) match {
                case InsertionPoint(x) => x
                case Found(x) => x
            };
            (kub, v)
        }).groupByKey().flatMap(tup => {
            val k = tup._1
            val a = tup._2
            val step = size / serwery

            val previous = rozmiary.take(k).map(_._2).sum.toInt
            val offset = (step - ((previous + 1) % step)) % step

            var last = (previous / step).toInt
            a.drop(offset).grouped(step).map(l => {last += 1; (last, l.head)}).toList
        }).collect.sortWith(_._1 < _._1).map(_._2).dropRight(1)

        granice2
    }

    def policzWagi(podzial : Array[Int], rdd : RDD[(Int, Int)]) = {
        rdd.map(v => {
            val kub = podzial.search(v._1) match {
                case InsertionPoint(x) => x
                case Found(x) => x
            };
            (kub, v)
        }).map(v => (v._1, v._2._2)).reduceByKey((n,c) => n + c).collect.sortWith(_._1 < _._1)
    }

    def windowed(rdd : RDD[(Int, Int)], podzial : Array[Int], wagi : Array[Int], okno : Int) = {
        val size = rdd.count().toInt
        val step = size / (podzial.length + 1)
    
        rdd.map(v => {
            val kub = podzial.search(v._1) match {
                case InsertionPoint(x) => x
                case Found(x) => x
            };
            (kub, v)
        }).groupByKey().flatMap(tup => {
            val k = tup._1
            val a = (step * k to step * (k + 1) - 1) zip tup._2.toList.sortWith(_._1 < _._1).map(_._2)
        
            val dodatek =
                if (okno <= step) {
                    a.map(v => (k + 1, v))
                } else {
                    val offset = (okno - 1) / step
                    (a.map(v => (k + offset, v)))++(a.map(v => (k + 1 + offset, v)))
                }
        
            (a.map(v => (v._1 / step, v)) ++ dodatek).filter(_._1 <= podzial.length)
        }).groupByKey().flatMap(tup => {
            val k = tup._1
            val a = tup._2.toList
        
            val map = Map(a map (i => i._1 -> i._2): _*)
        
            for(e <- math.max(okno - 1, k * step) to (k+1)*step - 1) yield {
                val first = (e - okno + 1)
                val alpha = first / step
            
                val w1 = (first to math.min((alpha + 1) * step - 1, first + okno - 1)).map(map.apply).sum
                val w2 = (alpha + 1 to k - 1).map(wagi(_)).sum
                val w3 = if (alpha == k) 0 else (k * step to e).map(map.apply).sum
            
                (e - okno + 1, w1 + w2 + w3)
            }
        }).sortBy(_._1)
    }

    if (args.length != 4) {
        println("Usage: solution input_file window_size output_file no_of_partitions")
        System.exit(1)
    }

    val input_file = args(0)
    val okno = args(1).toInt
    val output_file = args(2)
    val partycje = args(3).toInt

    val sc = SparkContext.getOrCreate
    val rdd = sc.textFile(input_file).map(line => {
        val l = line.split(" ")
        (l(0).toInt, l(1).toInt)
    })

    val podzial = podziel(partycje, rdd.map(v => v._1))
    val wagi = policzWagi(podzial, rdd).map(v => v._2)
    val oknowane = windowed(rdd, podzial, wagi, okno)

    oknowane.map(t => t._1.toString + " " + t._2.toString).saveAsTextFile(output_file)
}

