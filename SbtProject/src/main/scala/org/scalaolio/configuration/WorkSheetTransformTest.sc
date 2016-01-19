import org.scalaolio.configuration._
val list1 =
    List(
        "foo" -> "bar"
      , "Foo" -> ""
    )
  val transform1 =
    Transform(list1, false)

  val transform2 =
    ValueTypedMap.tryApply(
        Map(
            "sn" -> "Situation Normal"
          , "aFu" -> "All Fuxxed Up"
          , "tar" -> "Things Are Really"
          , "fu" -> "Fuxxed Up"
          , "bar" -> "Beyond All Recognition"
        )
      , false
    ).flatMap(
      valueTypedMap =>
        Transform(
            valueTypedMap
          , List("Sn", "AfU", "Tar", "Fu", "Bar")
        )
    )
  val list3 =
    List(
        "startup.globalTimeout" -> "10s"
      , "startup.controlServiceInterface" -> "127.0.0.1"
      , "startup.controlServicePort" -> "8081"
      , "startup.serviceSaasInterface" -> "localhost"
      , "startup.serviceSaasPort" -> "8443"
    )
  val transform3 =
    Transform(list3)
  val csPort = transform3.get.valueTypedMap.tryInt("startup.controlServicePort")

  val transform3Rebase =
    transform3.get.rebase("Rebase.")
  val transform3Subset =
    transform3.get.trySubset("startup.")
  val transform3RebaseSubset =
    transform3Rebase.trySubset("Rebase.startup.")
  val isCaseSensitive4to6 = false
  val list4 =
    List(
        "startup.first" -> "first4"
      , "startup.globalTimeout" -> "14s"
      , "startup.controlServiceInterface" -> "127.0.0.14"
      , "startup.controlServicePort" -> "80814"
      , "startup.serviceSaasInterface" -> "localhost4"
      , "startup.serviceSaasPort" -> "84434"
    )
  val transform4 =
    Transform(list4, isCaseSensitive4to6)


  val list5 =
    List(
        "Startup.globalTimeout" -> "15s"
      , "Startup.controlServiceInterface" -> "127.0.0.15"
      , "Startup.controlServicePort" -> "80815"
      , "Startup.serviceSaasInterface" -> "localhost5"
      , "Startup.serviceSaasPort" -> "84435"
      , "Startup.last" -> "last5"
    )
  val transform5 =
    Transform(list5, true)


  val list6 =
    List(
        "startup.first" -> "first6"
      , "startup.globalTimeout" -> "16s"
      , "startup.controlServiceInterface" -> "127.0.0.16"
      , "startup.controlServicePort" -> "80816"
      , "startup.serviceSaasInterface" -> "localhost6"
      , "startup.serviceSaasPort" -> "84436"
      , "startup.last" -> "last6"
    )
  val transform6 =
    Transform(list6, isCaseSensitive4to6)
  val transform4Merge4 =
    transform4.get.tryMerge(transform4.get)
  val transform5Merge5 =
    transform5.get.tryMerge(transform5.get)
  val transform6Merge6 =
    transform6.get.tryMerge(transform6.get)
  val transform4InvertCaseSensitive =
    transform4.get.tryInvertCaseSensitive.get
  val valueTypedMap5 =
    transform5.get.valueTypedMap
  val valueTypedMap5InvertKeyCaseSensitive =
    transform5.get.valueTypedMap.tryInvertKeyCaseSensitive().get

  val transform5InvertCaseSensitive =
    transform5.get.tryInvertCaseSensitive.get
  val transform6InvertCaseSensitive =
    transform6.get.tryInvertCaseSensitive.get

  val transform4Merge5 =
    transform4.get.tryMerge(transform5.get)
  val configuration5Merge4 =
    transform5.get.tryMerge(transform4.get)
  val configuration4Merge6 =
    transform4.get.tryMerge(transform6.get)
  val configuration6Merge4 =
    transform6.get.tryMerge(transform4.get)
  val configuration5Merge6 =
    transform5.get.tryMerge(transform6.get)
  val configuration6Merge5 =
    transform6.get.tryMerge(transform5.get)
