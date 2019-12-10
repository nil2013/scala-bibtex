package me.nsmr.sbibtex

object BibEntry {

  class EntryType(val name: String, val required: Set[String] = Set.empty) {
    override def toString(): String = s"@$name"
  }

  // EntryTypeについてはXMLで用意しておいて、それを読み込む格好にする。

  def sanitize(orig: String): String = orig
}

case class BibEntry(
                     entryType: BibEntry.EntryType,
                     values: Map[String, String],
                     tags: Set[String]
                   ) {

  def verify(): Set[String] = entryType.required.filter(key => !values.keySet.contains(key))

  def source: String = {
    val sb = new StringBuilder
    val nl = System.lineSeparator()

    sb.append(s"@${entryType.name}{")
    if (!tags.isEmpty) sb.append(" ").append(tags.mkString(", "))
    sb.append(nl).append(
      values.map { case (k, v) => s"  $k = {${BibEntry.sanitize(v)}}" }.mkString(nl)
    ).append(nl).append("}")

    sb.toString
  }

}
