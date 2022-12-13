package tkasu.aoc22.utils

object string {

  def addThreadPrefix(s: String): String =
    val threadName = Thread.currentThread().getName
    s"[$threadName] $s"

}
