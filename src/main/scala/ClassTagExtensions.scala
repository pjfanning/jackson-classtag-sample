import com.fasterxml.jackson.databind.{JavaType, ObjectMapper}
import com.fasterxml.jackson.databind.`type`.TypeFactory

import scala.reflect.ClassTag

object ClassTagExtensions {

  val mapper = new ObjectMapper()

  /*
   **********************************************************
   * Configuration, basic type handling
   **********************************************************
   */

  /**
   * Convenience method for constructing [[com.fasterxml.jackson.databind.JavaType]] out of given
   * type (typically <code>java.lang.Class</code>), but without explicit
   * context.
   */
  def constructType[T: JavaTypeable]: JavaType = {
    implicitly[JavaTypeable[T]].asJavaType(mapper.getTypeFactory)
  }

  /*
   **********************************************************
   * Public API (from ObjectCodec): Tree Model support
   **********************************************************
   */

  /**
   * Convenience method for doing two-step conversion from given value, into
   * instance of given value type. This is functionality equivalent to first
   * serializing given value into JSON, then binding JSON data into value
   * of given type, but may be executed without fully serializing into
   * JSON. Same converters (serializers, deserializers) will be used as for
   * data binding, meaning same object mapper configuration works.
   *
   * @throws IllegalArgumentException If conversion fails due to incompatible type;
   *                                  if so, root cause will contain underlying checked exception data binding
   *                                  functionality threw
   */
  def convertValue[T: JavaTypeable](fromValue: Any): T = {
    convertValue(fromValue, constructType[T])
  }

  private def classFor[T: ClassTag]: Class[T] = {
    implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
  }
}

trait JavaTypeable[T] {
  def asJavaType(typeFactory: TypeFactory): JavaType
}

object JavaTypeable {

  implicit val anyJavaTypeable: JavaTypeable[Any] = {
    new JavaTypeable[Any] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array()
        typeFactory.constructParametricType(classOf[Object], typeArgs: _*)
      }
    }
  }

  implicit def optionJavaTypeable[T : JavaTypeable]: JavaTypeable[Option[T]] = {
    new JavaTypeable[Option[T]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArg0 = implicitly[JavaTypeable[T]].asJavaType(typeFactory)
        typeFactory.constructReferenceType(classOf[Option[_]], typeArg0)
      }
    }
  }

  implicit def arrayJavaTypeable[T : JavaTypeable]: JavaTypeable[Array[T]] = {
    new JavaTypeable[Array[T]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArg0 = implicitly[JavaTypeable[T]].asJavaType(typeFactory)
        typeFactory.constructArrayType(typeArg0)
      }
    }
  }

  implicit def mapJavaTypeable[M[_,_] <: Map[_,_], K : JavaTypeable, V: JavaTypeable](implicit ct: ClassTag[M[K,V]]): JavaTypeable[M[K, V]] = {
    new JavaTypeable[M[K, V]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArg0 = implicitly[JavaTypeable[K]].asJavaType(typeFactory)
        val typeArg1 = implicitly[JavaTypeable[V]].asJavaType(typeFactory)
        typeFactory.constructMapLikeType(ct.runtimeClass, typeArg0, typeArg1)
      }
    }
  }

  implicit def collectionJavaTypeable[I[_] <: Iterable[_], T : JavaTypeable](implicit ct: ClassTag[I[T]]): JavaTypeable[I[T]] = {
    new JavaTypeable[I[T]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArg0 = implicitly[JavaTypeable[T]].asJavaType(typeFactory)
        typeFactory.constructCollectionLikeType(ct.runtimeClass, typeArg0)
      }
    }
  }

  implicit def gen5JavaTypeable[T[_, _, _, _, _], A: JavaTypeable, B: JavaTypeable, C: JavaTypeable, D: JavaTypeable, E: JavaTypeable](implicit ct: ClassTag[T[A, B, C, D, E]]): JavaTypeable[T[A, B, C, D, E]] = {
    new JavaTypeable[T[A, B, C, D, E]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array(
          implicitly[JavaTypeable[A]].asJavaType(typeFactory),
          implicitly[JavaTypeable[B]].asJavaType(typeFactory),
          implicitly[JavaTypeable[C]].asJavaType(typeFactory),
          implicitly[JavaTypeable[D]].asJavaType(typeFactory),
          implicitly[JavaTypeable[E]].asJavaType(typeFactory)
        )
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

  implicit def gen4JavaTypeable[T[_, _, _, _], A: JavaTypeable, B: JavaTypeable, C: JavaTypeable, D: JavaTypeable](implicit ct: ClassTag[T[A, B, C, D]]): JavaTypeable[T[A, B, C, D]] = {
    new JavaTypeable[T[A, B, C, D]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array(
          implicitly[JavaTypeable[A]].asJavaType(typeFactory),
          implicitly[JavaTypeable[B]].asJavaType(typeFactory),
          implicitly[JavaTypeable[C]].asJavaType(typeFactory),
          implicitly[JavaTypeable[D]].asJavaType(typeFactory)
        )
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

  implicit def gen3JavaTypeable[T[_, _, _], A: JavaTypeable, B: JavaTypeable, C: JavaTypeable](implicit ct: ClassTag[T[A, B, C]]): JavaTypeable[T[A, B, C]] = {
    new JavaTypeable[T[A, B, C]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array(
          implicitly[JavaTypeable[A]].asJavaType(typeFactory),
          implicitly[JavaTypeable[B]].asJavaType(typeFactory),
          implicitly[JavaTypeable[C]].asJavaType(typeFactory)
        )
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

  implicit def gen2JavaTypeable[T[_, _], A: JavaTypeable, B: JavaTypeable](implicit ct: ClassTag[T[A, B]]): JavaTypeable[T[A, B]] = {
    new JavaTypeable[T[A, B]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array(
          implicitly[JavaTypeable[A]].asJavaType(typeFactory),
          implicitly[JavaTypeable[B]].asJavaType(typeFactory)
        )
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

  implicit def gen1JavaTypeable[T[_], A: JavaTypeable](implicit ct: ClassTag[T[A]]): JavaTypeable[T[A]] = {
    new JavaTypeable[T[A]] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array(
          implicitly[JavaTypeable[A]].asJavaType(typeFactory)
        )
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

  implicit def gen0JavaTypeable[T](implicit ct: ClassTag[T]): JavaTypeable[T] = {
    new JavaTypeable[T] {
      override def asJavaType(typeFactory: TypeFactory): JavaType = {
        val typeArgs: Array[JavaType] = Array()
        typeFactory.constructParametricType(ct.runtimeClass, typeArgs: _*)
      }
    }
  }

}
