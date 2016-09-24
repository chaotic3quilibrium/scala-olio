package org.scalaolio.io

import java.io.Serializable

//Adapted StackOverflow answer found here:
//  http://stackoverflow.com/a/6002932/501113
object SerializableAdapter {
  implicit def toSerializableAdapterAnyVal[S <: AnyVal]: SerializableAdapter[S] =
    new SerializableAdapter[S] {}
  implicit def toSerializableAdapterJavaIoSerializable[S <: Serializable]: SerializableAdapter[S] =
    new SerializableAdapter[S] {}

  def generate[S: SerializableAdapter](value: S): S =
    value
}

trait SerializableAdapter[S]
