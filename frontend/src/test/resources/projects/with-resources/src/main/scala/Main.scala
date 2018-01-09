import java.io.{BufferedReader, InputStreamReader}

object Main {

  def resourceContent(name: String): Option[String] = {
    Option(getClass.getClassLoader.getResourceAsStream(name)).map { stream =>
      val reader = new BufferedReader(new InputStreamReader(stream))
      reader.readLine()
    }
  }

  def main(args: Array[String]): Unit = {
    assert(resourceContent("my-compile-resource.txt") == Some("Content of my-compile-resource.txt"))
    assert(
      resourceContent("generated-compile-resource.txt") == Some(
        "Content of generated-compile-resource.txt"))
    assert(resourceContent("non-existent-resource.txt") == None)
    println("OK")
  }

}
