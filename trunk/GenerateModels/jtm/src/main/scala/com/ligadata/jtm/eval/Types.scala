package com.ligadata.jtm.eval

import com.ligadata.jtm.nodes.Root

/**
  *
  */
object Types {

  def upsert(key: String, value: Set[String]) (implicit map: scala.collection.mutable.Map[String, Set[String]])= {
    val c = map.get(key)
    if(c.isDefined) {
      val s = c.get
      val r = s ++ value
      map += (key -> r)
    } else {
      map += (key -> value)
    }
  }

  /** Map with all messages used and their aliases
    *
    * @param root
    * @return
    */
  def CollectMessages(root: Root): Map[String, Set[String]] = {
    val givenAlias = root.aliases.messages
    implicit var result = scala.collection.mutable.Map.empty[String, Set[String]]

    // Go through all transformations
    root.transformations.foreach( t => {
      // Dependency sets test
      t._2.dependsOn.foreach( d => {
        d.foreach( c => {
          val resolved = givenAlias.getOrElse(c, c)
          upsert(resolved, Set(c))
        })
      })
      t._2.outputs.foreach( o => {
        val c = o._1 // Name of the output
        val resolved = givenAlias.getOrElse(c, c)
         upsert(resolved, Set(c))
      })
    })
    result.toMap
  }

/*
  def InvertAliases(m: Map[String, Set[String]], Alias: Map[String, String]): Map[String, String] = {

    def FindTerminal(start: String): String = {
      var cur = start

      f = m.get(cur)
      if(f.get==start)
        start
      else
        FindTerminal()
    }

    m.foldLeft(Array.empty[(String, String)]) ( (r, s) => {
      s._2.map( e => {
          (s._1, FindTerminal(s._1))
      }).toArray
    }).map( e => (e._1 -> e._2)).toMap
  }
*/

  /** Map with all types and the path to it
    *
    * @param root
    * @return
    */
  def CollectTypes(root: Root): Map[String, Set[String]] = {

    implicit var result = scala.collection.mutable.Map.empty[String, Set[String]]

    root.transformations.foreach( t => {
      t._2.computes.foreach( c => {
          if(c._2.typename.length>0) {
            upsert(c._2.typename, Set(t._1 + "/" + c._1))
          }
      })

      t._2.outputs.foreach( o => {
        o._2.computes.foreach(c => {
            if (c._2.typename.length>0) {
              upsert(c._2.typename, Set(t._1 + "/" + c._1))
            }
          })
      })
    })

    result.toMap
  }

  /** Resolve the input dependencies in terms of transformations
    *
    * @param root
    * @return
    *         0 = {Tuple2@17339} "(Set(com.ligadata.kamanja.test.msg1),(1,Set(test1)))"
    *         1 = {Tuple2@17340} "(Set(com.ligadata.kamanja.test.msg3),(2,Set(test2)))"
    */
  def ResolveDependencies(root: Root): Map[Set[String], (Long, Set[String])] = {

    val aliaseMessages: Map[String, String] = root.aliases.messages.toMap

    val dependencyToTransformations = root.transformations.foldLeft( (0, Map.empty[Set[String], (Long, Set[String])]))( (r1, t) => {
      val transformationName = t._1
      val transformation = t._2

      // Normalize the dependencies, target must be a class
      // ToDo: Do we need chains of aliases, or detect chains of aliases

      t._2.dependsOn.foldLeft(r1)( (r, dependencies) => {

        val resolvedDependencies = dependencies.map(alias => {
          // Translate dependencies, if available
          aliaseMessages.getOrElse( alias, alias )
        }).toSet

        val curr = r._2.get(resolvedDependencies)
        if(curr.isDefined) {
          ( r._1,     r._2 ++ Map[Set[String],(Long, Set[String])](resolvedDependencies -> (curr.get._1, curr.get._2 + t._1)) )
        } else {
          ( r._1 + 1, r._2 ++ Map[Set[String],(Long, Set[String])](resolvedDependencies -> (r._1 + 1, Set(t._1))) )
        }
      })
    })._2

    dependencyToTransformations
  }



}
