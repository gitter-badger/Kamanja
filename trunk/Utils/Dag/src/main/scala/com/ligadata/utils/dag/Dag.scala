/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.utils.dag

import scala.collection.mutable.ArrayBuffer
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.utils._
import scala.collection.mutable.Map
import com.ligadata.Exceptions._

object Dag {
  private val log = LogManager.getLogger(getClass);
  
  def Trace(str: String) = if(log.isTraceEnabled())  log.trace(str)
  def Warning(str: String) = if(log.isWarnEnabled()) log.warn(str)
  def Info(str: String) = if(log.isInfoEnabled())    log.info(str)
  def Error(str: String) = if(log.isErrorEnabled())  log.error(str)
  def Debug(str: String) = if(log.isDebugEnabled())  log.debug(str)
}

import Dag._
// ==============================================================================================================
// ====================================== public classes ========================================================
// ==============================================================================================================

// class representing an edge id -
//   edge id consists of two parts - the originator node and edge type id.
//   implies there can be only one msg type coming as output for each edge
//   note: need to handle when a node can output multiple messages of same type
//

case class EdgeId(nodeId: Long, edgeTypeId: Long) {
  def IsMatch(eid: EdgeId) : Boolean = {
    return (eid.edgeTypeId == edgeTypeId) && ((eid.nodeId == nodeId) || (nodeId == 0))
  }
}

// class representing the ready node to be executed - return from dag 
case class ReadyNode(nodeId: Long, iesPos: Int)

// High level public class to create a dag. This should be used in conjunction with a DagRuntime instance to process messages.
// A single Dag instance could be shared across all DagRuntime instances as once created, it should not be updated.
// mdVersion - version of underlying metadata that used to create this dag instance.
//           - there should be only one instance of this dag per version of metadata
class Dag(mdTimeStamp: String) {
  // check if node id already exists - if exist, it is error condition
  // if not, make a new node with the given information and put that node in node map
  // and populate input edge map with the input edges from that node.
  def AddNode(nodeId : Long, inputs: Array[Array[EdgeId]], outputs: Array[Long]): Unit = dagImpl.AddNode(nodeId, inputs, outputs)
  def toJSON: String = "{}"

  private[dag] val dagImpl = DagRT.DagImpl()
}

// DagRunime must be created for each partition as this state must be exclusive to each unit of high level parallel processing
//
class DagRuntime {
  private var drImpl = DagRT.newDagRTImpl
  // once a new dag is ready, set the dag for run time, so it can reinitialize run time structures representing the dag
  def SetDag(dagNew : Dag) = drImpl.SetDag(dagNew)

  // call this for each completion of dag processing
  // Idea is that this runtime information is non reentrant and used by one thread only or in synchronous manner
  // and reset after each transaction.

  def ReInit() = drImpl.ReInit
  // when an edge gets fired,
  //  get edge type id and use it to find list of InputEdges associated with this the given edge type
  //  for each input edge, turn on the flag in the edge set runtime that corresponds to the input edge's position in the set
  //  if all dependencies for edge set are satisfied, put the node id and edge set position corresponding to the edge set into ready list
  //  return the ready node list
  def FireEdge(eid: EdgeId) : Array[ReadyNode] = drImpl.FireEdges(Array(eid))
  def FireEdges(eids: Array[EdgeId]) : Array[ReadyNode] = drImpl.FireEdges(eids)
  def toJSON (dagAlso: Boolean, f: (Long, Int) => String) : String = "{}"
}

// ====================================================================================================
// ========================== private classes and implementation ======================================
// ====================================================================================================

private[dag]
object DagRT {
  def newDagRTImpl = new DagRTImpl
  
  // maintain all edge sets where a given edge type is member of
  case class EdgeType(edgeTypeId: Long, idxET: Int) {
    def AddInputEdge(ie: InputEdge) = inputEdges.append(ie)
    val inputEdges = ArrayBuffer[InputEdge]()
  }
  
  // definition of input edge which is denoted by origination node id and msg type
  case class InputEdge(eid: EdgeId, et: EdgeType, idxInSet: Int) {
    var iesOwner : InputEdgeSet = null
    def IsMatch(eidFrom: EdgeId) = eid.IsMatch(eidFrom)
  }
  
  case class InputEdgeSet(nodeId: Long, idxIESInNode: Int, idxIES: Int, inputEdges: Array[InputEdge]) {
    // once this class is created, call this method to link input edges to input edge type to handle run time matching
    def LinkEdges = { inputEdges.foreach(ie => { ie.iesOwner = this; ie.et.AddInputEdge(ie)}) }
    
    def Contains(eid: EdgeId) : Int = {
      var idx = 0
      inputEdges.foreach{ edge => if (edge.IsMatch(eid)) return idx; idx += 1}
      -1
    }
  }
  
  case class DagNode(nodeId: Long, idxNode: Int, inputs: Array[InputEdgeSet], outputs: Array[Long])
  
  case class DagImpl() {
    // check if node id already exists - if exist, it is error condition
    // if not, make a new node with the given information and put that node in node map
    // and populate input edge map with the input edges from that node.
    def AddNode(nodeId : Long, inputs: Array[Array[EdgeId]], outputs: Array[Long]): Unit = {
      Info("DagImpl:AddNode ->, numNodes:%d, numEdgeSets: %d, numEdgeTypes: %d, nodeid: %d, inputs: %s, outputs: %s\n".format(numNodes, numEdgeSets, numEdgeTypes, nodeId, inputs.map(_.mkString(",")).mkString(":"), outputs.mkString(",")))
      if(nodesMap.contains(nodeId))
        throw AlreadyExistsException("Dag::AddNode, nodeId already exist: %d".format(nodeId), null)
      val node = MakeNode(nodeId, inputs, outputs)
      Info("DagImpl:AddNode <-, numNodes:%d, numEdgeSets: %d, numEdgeTypes: %d, nodeid: %d\n".format(numNodes, numEdgeSets, numEdgeTypes, nodeId))
    }
  
    def numEdgeSets = _numEdgeSets
    def getInputEdgeSet(idx: Int) = iesList(idx)
    def getEdgeType(edgeTypeId: Long) = edgeTypeMap.get(edgeTypeId)
    
    private[this] var numNodes = 0
    private[this] var _numEdgeSets = 0
    private[this] var numEdgeTypes = 0
  
    private[this] val nodesMap = Map[Long, DagNode]()
    private[this] val edgeTypeMap = Map[Long, EdgeType]()
  
    private[this] val nodesList = ArrayBuffer[DagNode]()
    private[this] val iesList = ArrayBuffer[InputEdgeSet]()
    private[this] val edgeTypeList = ArrayBuffer[EdgeType]()
  
    private 
    def AddEdgeType(eti: Long) : EdgeType = {
      if(! edgeTypeMap.contains(eti)) {
        Info("DagImpl:AddEdgeType, adding new edge type; pre numEdgeTypes: %d, eti: %d\n".format(numEdgeTypes, eti))
        val et = EdgeType(eti, numEdgeTypes)
        edgeTypeMap(eti) = et
        edgeTypeList.append(et)
        numEdgeTypes += 1
      }
      // there should not be any exceptions at this point while accessing map
      edgeTypeMap(eti)
    }
    
    private 
    def AddInputEdge(eid : EdgeId, edgeIdxInSet: Int) : InputEdge = {
      val eti = eid.edgeTypeId
      val et = AddEdgeType(eti)
      val ie = InputEdge(eid, et, edgeIdxInSet)
      Info("DagImpl:AddInputEdge, added new input edge type; edgeIdxInSet: %d, eid: %s\n".format(edgeIdxInSet, eid.toString))
      ie
    }
    
    private 
    def AddInputEdgeSet(nodeId: Long, idxSetInNode:Int, edgeIdSet: Array[EdgeId]) : InputEdgeSet = {
      var edgeIdxInSet = -1
      val ies = InputEdgeSet(nodeId, idxSetInNode, numEdgeSets, edgeIdSet.map(eid => { edgeIdxInSet += 1; AddInputEdge(eid, edgeIdxInSet) }))
      ies.LinkEdges
      iesList.append(ies)
      _numEdgeSets += 1
      Info("DagImpl:AddInputEdgeSet, added new input edge set; nodeId: %d, idxSetInNode: %d, _numEdgeSets: %d, edgeIdSet: %s\n".format(nodeId, idxSetInNode, _numEdgeSets, edgeIdSet.mkString(",")))
      ies
    }
  
    // make a node out of inputs from add node -
    //  translate input array of array of edgeid into array of InputEdgeSets
    private 
    def MakeNode(nodeId : Long, inputs: Array[Array[EdgeId]], outputs: Array[Long]) = {
      var idxSetInNode = -1;
      val node = DagNode(nodeId, numNodes, inputs.map(edgeIds => { idxSetInNode += 1; AddInputEdgeSet(nodeId, idxSetInNode, edgeIds)}), outputs)
      nodesMap(nodeId) = node
      nodesList.append(node)
      numNodes = numNodes + 1
    }
  }
  
  case class IesRTElem(ies: InputEdgeSet) {
    def Reset() = { curCnt = 0;  for (i <- 0 until iesCnt) flagsFired(i) = false }
    def IsSatisfied = (curCnt == iesCnt) // flagsFired.foldLeft(false){(result, flag) => (result && flag) }
    
    // Return true if all edges are in fired/satisfied state otherwise false
    def SetEdgeAsFired(idx : Int) : Boolean = {
      if(idx >= iesCnt) {
        // throw exception with appropriate message - must be bug as the idx never exceed iesCnt
      }
      if(flagsFired(idx)) {
        // throw exception with appropriate message - must not be firing same edge more than once, must be bug
      }
      flagsFired(idx) = true
      curCnt += 1
      IsSatisfied
    }
    def toJSON : String = ""
  
    val iesCnt : Int = ies.inputEdges.length
    private var curCnt = 0
    val flagsFired = new Array[Boolean](iesCnt)
    Reset()
  }
  
  class DagRTImpl {
    private var dag : DagImpl = null
    private var iesRT: Array[IesRTElem] = null
  
    // once a new dag is ready, set the dag for run time, so it can reinitialize run time structures representing the dag
    def SetDag(dagNew : Dag) = {
      dag = dagNew.dagImpl
      iesRT = new Array[IesRTElem](dag.numEdgeSets)
      for(idx <- 0 until dag.numEdgeSets) {
        iesRT(idx) = IesRTElem(dag.getInputEdgeSet(idx))
        iesRT(idx).Reset
      }
    }
  
    // call this for each completion of dag processing
    // Idea is that this runtime information is non reentrant and used by one thread only or in synchronous manner
    // and reset after each transaction.
  
    def ReInit() = iesRT.foreach(iesElem => iesElem.Reset())
    // when an edge gets fired,
    //  get edge type id and use it to find list of InputEdges associated with this the given edge type
    //  for each input edge, turn on the flag in the edge set runtime that corresponds to the input edge's position in the set
    //  if all dependencies for edge set are satisfied, put the node id and edge set position corresponding to the edge set into ready list
    //  return the ready node list
    def FireEdges(eids: Array[EdgeId]) : Array[ReadyNode] = {
      val result = new ArrayBuffer[ReadyNode]()

      def ProcessEid(eid: EdgeId) = {
        val eti = eid.edgeTypeId
        val etOpt = dag.getEdgeType(eti)
        if(etOpt.isEmpty)
          throw InvalidArgumentException("DagRuntime::FireEdge - eid.edgeTypeId does not exist in Dag, eid.NodeId: %d, eid.edgeTypeId: %d".format(eid.nodeId, eid.edgeTypeId), null)
        val et = etOpt.get
        et.inputEdges.foreach(ie => if(ie.IsMatch(eid)) ProcessMatch(ie))
      }
      
      def ProcessMatch(ie : InputEdge) = {
        val ies = ie.iesOwner
        val idxIES = ies.idxIES
        val idxInSet = ie.idxInSet
        
        if(iesRT(idxIES).SetEdgeAsFired(idxInSet)) {
          result.append(ReadyNode(ies.nodeId, ies.idxIESInNode))
        }
      }
      eids.foreach(eid => ProcessEid(eid))
      result.toArray
    }
  }
}
