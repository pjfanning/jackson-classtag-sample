import com.fasterxml.jackson.databind.json.JsonMapper
import com.fasterxml.jackson.module.scala.{ClassTagExtensions, DefaultScalaModule}

//case class GenericTestClass[T](t: T)

object Main extends App {
  //val toplevelArrayJson = """[{"t":42},{"t":31}]"""
  val genericMixedFieldJson = """{"first":"firstVal","second":2}"""
  val builder = JsonMapper.builder().addModule(DefaultScalaModule)
  val mapper = builder.build() :: ClassTagExtensions
  val result1 = mapper.readValue[Map[String, Any]](genericMixedFieldJson)
  println(result1)
}
