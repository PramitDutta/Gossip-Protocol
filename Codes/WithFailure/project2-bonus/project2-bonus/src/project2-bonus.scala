/*....*/

// Project 2
// Distributed Operating System
// Fall 2015
// Chiranjib Sur, Pramit Dutta
// Gossip Protocol

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.util.Random

// declaration of the global various function along with the arguments
case object Gossip // for message
case class startActorAgent(node:Int, nodesRefered:List[ActorRef], adjacencyList:List[Int], numberOfNodes:Int) // starting the actor models
case class PushSum (s : Double, w : Double) // for the push sum
case class converge(node:Int)  // to check if X% of the nodes have got the message


object Project2 extends App {
  var noOfNodes : Int = args(0).toInt  // number of nodes
  var topology : String = args(1).toString // topology
  var algorithm : String = args(2).toString // algorithm kind
  //print(noOfNodes+" "+topology+" "+algorithm+" ")  // testing the arguments

  print("\n=================================================================\n")
  print("\nStarting the Simulation  ")
  print("\n=================================================================\n")
  print(" with "+noOfNodes+" number of nodes"+" and "+topology+" Topology"+" and "+algorithm+" Algorithm.")
  print("\n=================================================================\n")

  // Various conversion before the topology is being created
  var NodesCbrt : Int = 0 // a temporary variable
  // Converting the number of nodes to their nearest cubic number, for eg any number below 8 is converted to 8
  if ((math.cbrt(noOfNodes.toDouble) % 1 == 0)) { // checking if the number is divible as a cude
    NodesCbrt = math.cbrt(noOfNodes.toDouble).toInt // transfer the number for further process
    // print(NodesCbrt+" ")  // just for testing
  }
  else { // if the number is not divisible as a cube
    var x: Int = noOfNodes // a temporary variable store
    while ((math.cbrt(x.toDouble) % 1 != 0)) { // keep checking for cube rootable number and increment
      x = x + 1 // incrementing the number
    }
    noOfNodes = x  // the number found as cube root is taken
    NodesCbrt = math.cbrt(noOfNodes.toDouble).toInt // the cube root of the number is found out
  }

  // stating the main system to create topology and create the actors for the further processing of the algorithm
  val system=ActorSystem("main") // this is a standard way of declarartion
  val masterActor =system.actorOf(Props(new Master(noOfNodes,topology,algorithm,NodesCbrt)),"master") // this is also a standard way of declarartion
  // Switching the control to master
}

class Master(noOfNodes: Int, topo: String, algo: String,nodesCbrt: Int) extends Actor {
  var no_of_nodes: Int = noOfNodes  // receiver of the arguments for the master
  var topology: String = topo // receiver of the arguments for the master
  var algorithm: String = algo // receiver of the arguments for the master
  var NodesCbrt: Int = nodesCbrt // receiver of the arguments for the master
  var nodesRefered: List[ActorRef] = Nil // reference to nodes or the actor, initialized to nothing
  var nodesReplied: List[Int] = Nil // reference to nodes who have replied, used for convergence criteria
  var tempdata: Int = 0  // used as a variable for holding temporary data
  var checkpoint: Int = 0 // usd for checking whether X % of the nodes have heard about the message for termination of the program
  var START_TIME = 0L  // used for time analysis and calcualtion

  //Creating Nodes as actor variables
  for (i <- 0 to (no_of_nodes) - 1) { // starting from zero and hence minus one
    nodesRefered ::= context.actorOf(Props(new Node))  // each of the reference to nodes are assigned some value
  }
  print("Nodes Created\n") // checkpointing the program // for just testing
  // creating the Line Topology
  if (topology.equalsIgnoreCase("Line")) {  // enter this section if line is
    print("Creating Line Topology\n")  // for just testing
    for(i <- 0 to no_of_nodes-1) {  // for loop for generatong the topology parameters
      var adjacencyList :List[Int] = Nil  // declaring the list of reference
      // checking for side nodes
      if(i>0) {  // checking if the side node is not encountered
        adjacencyList ::= (i-1) // join the node with its previous node
        // print(i+" in 1") // testing
      }
      if(i<no_of_nodes-1) { // checking if the side node encountered
        adjacencyList::= (i+1) // join the node with the next one, just to avoid nullness
        // print(i+" in 2") // testing
      }
      // start that particular actor for processing data, message passing starts when algorithm is chosen
      if(i < no_of_nodes) {  // checking for the validity of the node id
        nodesRefered(i) ! startActorAgent(i, nodesRefered, adjacencyList, no_of_nodes)  // message passing the required arguments
      }
    }
  }
  // creating the Full Topology
  if (topology.equalsIgnoreCase("Full"))  {
    print("Creating Full Topology\n")  // for just testing
    for(i<- 0 to no_of_nodes-1) { // for loop for generatong the topology parameters
      var adjacencyList :List[Int] = Nil // creating list
      for(k<- 0 to no_of_nodes-1)       { // for loop for all the others
        if(k != i) { // check for identity
          adjacencyList ::= k // assign
        }
      }
      // start that particular actor for processing data, message passing starts when algorithm is chosen
      if(i < no_of_nodes) { // checking for the validity of the node id
        nodesRefered(i) ! startActorAgent(i, nodesRefered, adjacencyList, no_of_nodes) // message passing the required arguments
      }
    }
  }

  //Creating the 3D Grid..
  if (topology.equalsIgnoreCase("3D")) {
    print("Creating 3D Topology\n")  // for just testing

    for (j <- 0 to NodesCbrt-1) {
      var nodesPerLayer : Int = (no_of_nodes/NodesCbrt)
      print("Nodes in a layer "+nodesPerLayer+" ")
      var temp : Int = 0
      if(j == 0) temp = 0
      else if(j == 1) temp = (NodesCbrt * NodesCbrt)
      else temp = 2 * (NodesCbrt * NodesCbrt)

      for (i <- 0 to (NodesCbrt*NodesCbrt) - 1) {
        print("\n")
        var tempindex :Int = ((i+1)+(j*(NodesCbrt*NodesCbrt))-1)
        print(tempindex)
        var adjacencyList: List[Int] = Nil

        // check for values for first and second column only
        if (i < (nodesPerLayer - NodesCbrt)) {
          adjacencyList::= i + NodesCbrt + temp // assign values
          //print(" node: " + (i + NodesCbrt + temp)) // testing
        }

        // check for value of second and third column
        if (i >= NodesCbrt) {
          adjacencyList ::= i - NodesCbrt + temp // assign values
          //print(" node: " + (i - NodesCbrt + temp)) // testing
        }

        // check for other than the edge
        if ((i % NodesCbrt) != 0)  {
          adjacencyList ::= i - 1 + temp // assign values
          //print(" node: " + (i - 1 + temp)) // testing
        }

        //check for the edge values
        if (((i + 1) % NodesCbrt) != 0) {
          adjacencyList ::= i + 1 + temp // assign values
          //print(" node: " + (i + 1 + temp)) // testing
        }

        // Creating adjacency of a node across the layers for the 3D structure
        //For 1st Layer (only backward)
        if (tempindex >= 0 && tempindex < (NodesCbrt*NodesCbrt)) {
          adjacencyList ::= tempindex + (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex + (NodesCbrt*NodesCbrt))) // testing
        }
        // For Layers in Between ( backward and forward )
        if (tempindex >= (NodesCbrt*NodesCbrt) && tempindex < (NodesCbrt-1)*(NodesCbrt*NodesCbrt)) {
          adjacencyList ::= tempindex + (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex + (NodesCbrt * NodesCbrt))) // testing

          adjacencyList ::= tempindex - (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex - (NodesCbrt * NodesCbrt))) // testing
        }
        //For the last layer (only forward)
        if (tempindex >= (NodesCbrt-1)*(NodesCbrt * NodesCbrt) && tempindex < (NodesCbrt * NodesCbrt * NodesCbrt)) {
          adjacencyList ::= tempindex - (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex - (NodesCbrt * NodesCbrt))) // testing
        }

        if(tempindex < no_of_nodes){ // checking for the validity of the node id
          // start that particular actor for processing data, message passing starts when algorithm is chosen
          nodesRefered(tempindex) ! startActorAgent(tempindex, nodesRefered, adjacencyList,no_of_nodes) // message passing the required arguments
        }
      }
    }
  }

  // Creating the Imperfect 3D Grid....
  if (topology.equalsIgnoreCase("3Dimp"))  {
    print("Creating 3D improper Topology\n")  // for just testing

    for (j <- 0 to NodesCbrt-1) {
      var nodesPerLayer : Int = (no_of_nodes/NodesCbrt)
      print("Nodes in a layer "+nodesPerLayer+" ")
      var temp : Int = 0
      if(j == 0) temp = 0
      else if(j == 1) temp = (NodesCbrt * NodesCbrt)
      else temp = 2 * (NodesCbrt * NodesCbrt)

      for (i <- 0 to (NodesCbrt*NodesCbrt) - 1) {
        print("\n")
        var tempindex :Int = ((i+1)+(j*(NodesCbrt*NodesCbrt))-1)
        print(tempindex)
        var adjacencyList: List[Int] = Nil

        // check for values for first and second column only
        if (i < (nodesPerLayer - NodesCbrt)) {
          adjacencyList::= i + NodesCbrt + temp // assign values
          //print(" node: " + (i + NodesCbrt + temp)) // testing
        }

        // check for value of second and third column
        if (i >= NodesCbrt) {
          adjacencyList ::= i - NodesCbrt + temp // assign values
          //print(" node: " + (i - NodesCbrt + temp)) // testing
        }

        // check for other than the edge
        if ((i % NodesCbrt) != 0)  {
          adjacencyList ::= i - 1 + temp // assign values
          //print(" node: " + (i - 1 + temp)) // testing
        }

        //check for the edge values
        if (((i + 1) % NodesCbrt) != 0) {
          adjacencyList ::= i + 1 + temp // assign values
          //print(" node: " + (i + 1 + temp)) // testing
        }

        // Creating adjacency of a node across the layers for the 3D structure
        //For 1st Layer (only backward)
        if (tempindex >= 0 && tempindex < (NodesCbrt*NodesCbrt)) {
          adjacencyList ::= tempindex + (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex + (NodesCbrt*NodesCbrt))) // testing
        }
        // For Layers in Between ( backward and forward )
        if (tempindex >= (NodesCbrt*NodesCbrt) && tempindex < (NodesCbrt-1)*(NodesCbrt*NodesCbrt)) {
          adjacencyList ::= tempindex + (NodesCbrt * NodesCbrt) // assign values
         //print(" n =" + (tempindex + (NodesCbrt * NodesCbrt))) // testing

          adjacencyList ::= tempindex - (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex - (NodesCbrt * NodesCbrt))) // testing
        }
        //For the last layer (only forward)
        if (tempindex >= (NodesCbrt-1)*(NodesCbrt * NodesCbrt) && tempindex < (NodesCbrt * NodesCbrt * NodesCbrt)) {
          adjacencyList ::= tempindex - (NodesCbrt * NodesCbrt) // assign values
          //print(" n =" + (tempindex - (NodesCbrt * NodesCbrt))) // testing
        }

        // Adding a random node other than itself for the adjacency list
        var random : Int = Random.nextInt(no_of_nodes)
        while (random == tempindex) {
          random = Random.nextInt(no_of_nodes)
        }
        adjacencyList ::= random

        // calling the actor creation and getting it ready for message
        if(tempindex < no_of_nodes){ // checking for the validity of the node id
          // start that particular actor for processing data, message passing starts when algorithm is chosen
          nodesRefered(tempindex) ! startActorAgent(tempindex, nodesRefered, adjacencyList,no_of_nodes) // message passing the required arguments
        }
      }
    }
  }



  //failure model implementation
  //if (Random.nextInt(no_of_nodes) > Random.nextInt(no_of_nodes)) {
    var random_node=Random.nextInt(no_of_nodes)
    context.stop(nodesRefered(random_node))
  //}
    //END


  // calling the algorithm for the different algorithms
  // Gossip starting
  if(algorithm.equalsIgnoreCase("Gossip")) { // if gossip
    println("\nStarting the Gossip algorithm") // testing flow
    println("\nThe Actor node will start passing the messages") // testing flow

    // Time Calculation
    START_TIME=System.currentTimeMillis // noting the starting time
    println("\nSTART_TIME : " +START_TIME) // display initial time

    // Actor and Message
    var startnode=Random.nextInt(nodesRefered.length) // starting a ramdom node as initial
    //print("\n"+random_node+" ")  // testing flow
    nodesRefered(startnode) ! Gossip // starting the actor with the message
  }
  // PushSum starting
  if(algorithm.equalsIgnoreCase("PushSum"))  { // if pushsum
    println("\nStarting the PushSum algorithm") // testing flow
    println("\nThe Actor node will start passing the messages") // testing flow

    // Time Calculation
    START_TIME=System.currentTimeMillis // noting the starting time
    println("\n--------START_TIME--------" +START_TIME) // display initial time

    // Actor and Message
    var startnode=Random.nextInt(nodesRefered.length)  // starting a ramdom node as initial
    //print("\n"+random_node+" ")  // testing flow
    nodesRefered(startnode) ! PushSum(0,0) // starting the actor with the message
  }

  // receiver for convergence detection
  def receive = {

    // convergence message detector
    case converge(node:Int) =>   { // converence checking point
      checkpoint=node  // id for the convergence
      //print("\nChecking Convergence\n")
      //print(node+" ") // testing

      // check for the presence of the node in the convergence list
      if(!(nodesReplied.contains(checkpoint))) { // if not present then add it
        nodesReplied ::= checkpoint // adding the node to list
      }

      //
      if(tempdata==0) { // ensure this section runs only once
        if(( nodesReplied.length.toDouble/no_of_nodes)>=0.99) // the percentage is kept here for checking
        {
          tempdata=1 // ensure that this section runs once
          println("\n==============================================\n")
          print("ALGORITHM CONVERGED")
          println("\n==============================================\n")
          context.system.shutdown // shutting the system

          // TIME Calculation
          var END_TIME=System.currentTimeMillis // variable for time detection
          println("\nEND_TIME: " +END_TIME)  // printing time

          // printing the diference in time
          println("\nDIFFERENCE in TIME IN MILLISECONDS = "+(END_TIME-START_TIME)+"\n")
        }
      }
    }
  }

  class Node() extends Actor  {
    //properties of node class
    var node:Int=0 // node id detection
    var nodesRefered :List[ActorRef] = Nil  // reference list
    var adjacencyList :List[Int] = Nil // neighbour list

    // variable to process data
    var counter:Int=0 // counter for termination
    var s : Double = 0  // temporary value
    var w : Double = 1 // temporary value
    var oldRatio : Double = 0 // old temporary value
    var newRatio : Double = 0 // old temporary value
    var random_node : Int = 0  // rtandom temporary variable

    // some temporary nodes
    var numberOfNodes1 : Int = 0 //
    var i : Int = 0
    var tempcheck : Int = 0

    // receiver module
    def receive =  {

      // case for receiveing the starting message
      case startActorAgent(nodex:Int, nodesReferedx:List[ActorRef], adjacencyListx:List[Int], numberOfNodesx:Int) =>  {
        // transfering the arguments
        node=nodex // node id
        nodesRefered = nodesReferedx // refernece to nodes
        adjacencyList = adjacencyListx // list of neighbours
        numberOfNodes1 = numberOfNodesx // number of nodes
        s = nodex
      }

      // case for receiveing the gossip message
      case `Gossip` =>  {
        //print("\n"+node_id+" ")

        if(counter<10)  { // terminate if 10 is reached
          // print("\n"+node+" "+counter) // testing
          context.parent ! converge(node)  // check for convergence
          // counter increment
          counter=counter+1
          // for loop to propafgate to others
          for(i <- 0 to 10)  {
            var random_node = Random.nextInt(adjacencyList.length) // random nodfe generated
            if (random_node < numberOfNodes1) { // checking for validity
              nodesRefered(adjacencyList(random_node)) ! Gossip  // transfer the message
            }
          }
        }
      }

      // case for receiveing the pushsum message
      case PushSum(svalue: Double, wvalue : Double) =>  {
        //print("\n"+node_id+" ")
        context.parent ! converge(node) // check for convergence

        // noting the old value doe s/w
        oldRatio = s/w

        // calculating the new value
        s = (s + svalue)/2
        w = (w + wvalue)/2

        // noting the new value doe s/w
        newRatio = s/w

        if (tempcheck == 0)  {
          // checking for difference
          if (newRatio - oldRatio > math.pow(10,(-10)))  { // check for difference
            counter = 0 // resetting the counter

            // for loop to propafgate to others
            for(i <- 0 to 20) {
              // random node number generation for propagation
              random_node = Random.nextInt(adjacencyList.length) // random number
              if (random_node < numberOfNodes1) { // check for validity
                nodesRefered(adjacencyList(random_node)) ! PushSum(s,w) // random propagation
              }
            }

          }

          // checking for difference and count round
          else if (newRatio - oldRatio < math.pow(10,(-10)) && counter>=3) {
            // stopping criteria
            tempcheck = 1
          }

          // checking for difference satisfaction and count round
          else if (newRatio - oldRatio < math.pow(10,(-10)) && counter<3)  {
            counter = counter + 1 // count for rounds

            // for loop to propafgate to others
            for(i <- 0 to 20)  { // counting to 20 for propagation
              random_node = Random.nextInt(adjacencyList.length)
              if (random_node < numberOfNodes1) { // check for validity
                nodesRefered(adjacencyList(random_node)) ! PushSum(s,w) // random propagation
              }
            }

            // print("\n"+node+" "+counter) // testing
          }
        }
      }
    }
  }
}