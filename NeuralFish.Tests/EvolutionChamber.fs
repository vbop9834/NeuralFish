module NeuralFish.Tests.EvolutionChamber

open Xunit
open FsUnit
open FsUnit.Xunit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exporter
open NeuralFish.EvolutionChamber

open NeuralFish.Tests.TestHelper

[<Fact>]
let ``AddBias mutation should add a bias to a neuron that has none`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddBias] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let addedBias =
    let neuron =
      let neuronId = neuron |> fst
      nodeRecords
      |> Map.find neuronId
    neuron.Bias |> should not' (be None)
    neuron.Bias.Value

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal (addedBias + 100.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``RemoveBias mutation should remove a bias to a neuron that has some bias`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 5.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 105.0)

  let activationFunctions : ActivationFunctions =
      Map.empty
      |> Map.add activationFunctionId activationFunction
  let syncFunctions =
      Map.empty
      |> Map.add syncFunctionId syncFunction
  let outputHooks =
      Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [RemoveBias] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    let layer = 1.0
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let neuron =
    let neuronId = neuron |> fst
    nodeRecords
    |> Map.find neuronId
  neuron.Bias |> should equal None

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal (100.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``MutateWeights mutation should mutate the weights of all outbound connections in a neuron`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 20.0)

  let activationFunctions : ActivationFunctions =
      Map.empty
      |> Map.add activationFunctionId activationFunction
  let syncFunctions =
      Map.empty
      |> Map.add syncFunctionId syncFunction
  let outputHooks =
      Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [MutateWeights] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning
 
  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let newWeight =
    let neuronOutboundConnections =
      let neuronId = neuron |> fst
      let neuron =
        nodeRecords
        |> Map.find neuronId
      neuron.OutboundConnections
      |> Map.toSeq

    let sensorId = newSensor |> fst
    let sensor = 
      nodeRecords
      |> Map.find sensorId

    sensor.OutboundConnections
    |> Map.toSeq
    |> Seq.append neuronOutboundConnections
    |> Seq.exists(fun (key,(_,weight)) -> weight <> 20.0)
    |> should equal true

    let _,(_,weight) =
      sensor.OutboundConnections
      |> Map.toSeq
      |> Seq.head
    weight


  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal newWeight)

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``AddOutboundConnection mutation should mutate connect a neuron to a neuron or actuator`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 20.0)

  let activationFunctions : ActivationFunctions =
      Map.empty
      |> Map.add activationFunctionId activationFunction
  let syncFunctions =
      Map.empty
      |> Map.add syncFunctionId syncFunction
  let outputHooks =
      Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddOutboundConnection] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning
 
  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply
 
[<Fact>]
let ``AddNeuron mutation should add a new neuron and connect it randomly in the neural network`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddNeuron] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  nodeRecords
  |> Map.filter (fun key record -> record.NodeType = NodeRecordType.Neuron)
  |> Map.toSeq
  |> Seq.length
  |> should be (greaterThan 1)

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``AddSensor mutation should add a new sensor and connect it randomly in the neural network`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let secondSyncFunctionId = 1
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
    |> Map.add secondSyncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddSensor] [activationFunctionId] [syncFunctionId; secondSyncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  nodeRecords
  |> Map.filter (fun key record -> record.NodeType = NodeRecordType.Sensor)
  |> Map.toSeq
  |> Seq.length
  |> should equal 2

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``AddActuator mutation should add a new actuator and connect it randomly in the neural network`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let secondOutputHookId = 9002
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ]
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook
    |> Map.add secondOutputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddActuator] [activationFunctionId] [syncFunctionId] [outputHookId; secondOutputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  nodeRecords
  |> Map.filter (fun key record -> record.NodeType = NodeRecordType.Actuator)
  |> Map.toSeq
  |> Seq.length
  |> should equal 2 

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply


[<Fact>]
let ``AddSensorLink mutation should add a sensor connection randomly in the neural network`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ]
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddSensorLink] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  let _,sensor = 
    let sensorId = sensor |> fst

    nodeRecords
    |> Map.toSeq
    |> Seq.item sensorId
  let previousWeightLength = weights |> List.length

  sensor.OutboundConnections
  |> Map.toSeq
  |> Seq.length
  |> should be (greaterThan previousWeightLength)

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``AddActuatorLink mutation should add an actuaor connection randomly in the neural network`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ]
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddActuatorLink] [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  //Adding 1 for the neuron to actuator connection
  let previousNumberOfConnections = (weights |> List.length) + 1 

  let sensorConnectionLength = 
    let _,sensor = 
      let sensorId = sensor |> fst
  
      nodeRecords
      |> Map.toSeq
      |> Seq.item sensorId
    sensor.OutboundConnections |> Map.toSeq |> Seq.length
  let neuronConnectionLength = 
    let _,neuron = 
      let neuronId = neuron |> fst

      nodeRecords
      |> Map.toSeq
      |> Seq.item neuronId
    neuron.OutboundConnections |> Map.toSeq |> Seq.length

  (sensorConnectionLength + neuronConnectionLength)
  |> should be (greaterThan previousNumberOfConnections)

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``MutateActivationFunction mutation should mutate the activation function of a neuron randomly`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let secondActivationFunctionId = 1
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add secondActivationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [MutateActivationFunction] [secondActivationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let neuronRecord =
    let neuronId = neuron |> fst
    nodeRecords
    |> Map.find neuronId
  neuronRecord.ActivationFunctionId |> should equal (Some secondActivationFunctionId)

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``MinimalMutationSequence should be capable of mutating records and executing correctly`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0;1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork minimalMutationSequence [activationFunctionId] [syncFunctionId] [outputHookId] NoLearning

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Should be able to evolve x generations`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunctionId = 0
  let syncFunctionSource : SyncFunctionSource =
    (fun nodeRecordsId -> 
      let data =
        [1.0; 1.0; 1.0; 1.0; 1.0]
        |> List.toSeq
      fakeDataGenerator([data])
    )

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let syncFunction = syncFunctionSource 0
  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId 1
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 100.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let outputHookFunctionIds : OutputHookFunctionIds =
    [outputHookId] |> List.toSeq

  let learningAlgorithm = 
    let learningCoefficient = 0.5
    Hebbian learningCoefficient

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddBias] [activationFunctionId] [syncFunctionId] outputHookFunctionIds learningAlgorithm

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

  let random = System.Random()
  let fitnessFunction : FitnessFunction =
    (fun nodeRecordsId neuralOutputs ->
     random.NextDouble() * (10.0 - 1.0) + 1.0
    )
  let syncFunctionSources : SyncFunctionSources =
    Map.empty
    |> Map.add syncFunctionId syncFunctionSource
  let endOfGenerationFunction : EndOfGenerationFunction =
    (fun scoredNodeRecords ->
     ()
    )
  let mutationSequence =
    [
      MutateActivationFunction
      AddBias
      RemoveBias
      MutateWeights
      AddInboundConnection
      AddOutboundConnection
      AddNeuron
      AddSensorLink
      AddActuatorLink
    ] |> List.toSeq
  let evolvedRecords =
    let generationRecords =
      Map.empty
      |> Map.add 0 nodeRecords
    let evolutionProperties =
      { defaultEvolutionProperties with
          MutationSequence = mutationSequence
          FitnessFunction = fitnessFunction
          ActivationFunctions = activationFunctions
          SyncFunctionSources = syncFunctionSources
          OutputHookFunctionIds = outputHookFunctionIds
          EndOfGenerationFunctionOption = Some endOfGenerationFunction
          StartingRecords = generationRecords
          NeuronLearningAlgorithm = learningAlgorithm
      }
    evolutionProperties |> evolveForXGenerations  
  evolvedRecords
  |> Map.toSeq
  |> Seq.length
  |> should be (greaterThan 0)

type PsuedoAnswer =
| RightAnswer
| WrongAnswer

[<Fact>]
let ``Should be able to evolve x generations from training set`` () =
  //Test setup
  let outputHookId = 0
  let activationFunctionId = 0
  let activationFunction = sigmoid

  let interpretActuatorOutputFunction : InterpretActuatorOutputFunction<PsuedoAnswer> =
    (fun outputMap ->
      outputMap
      |> Map.containsKey outputHookId
      |> should equal true

      let numericalResult =
        outputMap
        |> Map.find outputHookId
      if numericalResult > 0.99 then
        WrongAnswer
      else if numericalResult > 0.1 then
        RightAnswer
      else
        WrongAnswer
    )
  let scoreAnswer : ScoreNeuralNetworkAnswerFunction<PsuedoAnswer> =
    (fun correctAnswer guessedAnswer ->
      if correctAnswer = guessedAnswer then
        1.0
      else
        -1.0
    )

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction

  let outputHookFunctionIds : OutputHookFunctionIds =
    [outputHookId] |> List.toSeq

  let trainingSet : TrainingAnswerAndDataSet<PsuedoAnswer> =
    [
      RightAnswer,[0.0;1.5;2.2;3.1;4.5;5.2;6.66] |> List.toSeq
      WrongAnswer,[5.9;2.8;2.2;7.2;6.4;2.1;6.66] |> List.toSeq
      RightAnswer,[9.2;2.5] |> List.toSeq
      WrongAnswer,[] |> List.toSeq
    ] |> List.toArray

  let maximumMinds = 4

  let evolvedRecords =
    let trainingProperties =
      let defaultTrainingProperties = getDefaultTrainingProperties trainingSet interpretActuatorOutputFunction scoreAnswer activationFunctions outputHookFunctionIds
      { defaultTrainingProperties with
          ShuffleDataSet = true
          MaximumMinds = maximumMinds
      }
    trainingProperties |> evolveFromTrainingSet
  evolvedRecords
  |> Map.toSeq
  |> Seq.length
  |> should equal maximumMinds
