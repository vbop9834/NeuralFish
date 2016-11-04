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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddBias] activationFunctions syncFunctions outputHooks

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

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2

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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [RemoveBias] activationFunctions syncFunctions outputHooks

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

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2

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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [MutateWeights] activationFunctions syncFunctions outputHooks
 
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
    let sensorId = newSensor |> fst
    let sensor = 
      nodeRecords
      |> Map.find sensorId
    let _,(_,weight) =
      sensor.OutboundConnections
      |> Map.toSeq
      |> Seq.head
    weight

  // this has a chance of failing but is not very probable
  newWeight |> should not' ((equalWithin 0.01) (weights |> Seq.head))

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal newWeight)

  neuralNetwork |> killNeuralNetwork

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2

[<Fact>]
let ``AddOutboundConnection mutation should mutate connect a sensor or neuron to a neuron or actuator`` () =
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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddOutboundConnection] activationFunctions syncFunctions outputHooks 
 
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
  |> (should not' (equal 20.0))

  neuralNetwork |> killNeuralNetwork

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2
 
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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddNeuron] activationFunctions syncFunctions outputHooks

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

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2

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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddSensor] activationFunctions syncFunctions outputHooks

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
  |> should be (greaterThan 1)

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2

[<Fact>]
let ``AddActuator mutation should add a new actuator and connect it randomly in the neural network`` () =
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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddActuator] activationFunctions syncFunctions outputHooks

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
  |> should be (greaterThan 1)

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply |> ignore


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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddSensorLink] activationFunctions syncFunctions outputHooks

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

  Die |> testHookMailbox.PostAndReply |> ignore

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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [AddActuatorLink] activationFunctions syncFunctions outputHooks

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

  Die |> testHookMailbox.PostAndReply |> ignore

[<Fact>]
let ``MutateActivationFunction mutation should mutate the activation function of a neuron randoly`` () =
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
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
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
    |> mutateNeuralNetwork [MutateActivationFunction] activationFunctions syncFunctions outputHooks

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

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2
