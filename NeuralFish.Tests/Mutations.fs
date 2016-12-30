module NeuralFish.Tests.Mutations

open Xunit
open FsUnit
open FsUnit.Xunit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exporter
open NeuralFish.EvolutionChamber

open NeuralFish.Tests.TestHelper

[<Fact>]
let ``AddNeuronOutSplice mutation should add a neuron in a new layer, increasing the depth of the neural network`` () =
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords
    let mutations = [AddNeuronOutSplice]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuronRecords =
    nodeRecords
    |> Map.filter (fun _ nodeRecord -> nodeRecord.NodeType = NodeRecordType.Neuron)
  neuronRecords
  |> Seq.length
  |> should be (greaterThan 1)

  let originalNeuron = neuronRecords |> Map.find (fst neuron)
  neuronRecords
  |> Map.exists(fun _ record -> record.Layer <> originalNeuron.Layer)
  |> should equal true

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

let ``ResetWeights mutation should mutate the weights of all inbound connections in a neuron`` () =
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
      [1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords

    let neuronConnection =
      let neuronConnections =
        initialNodeRecords
        |> Map.find (neuron |> fst)
        |> (fun x -> x.InboundConnections)
      neuronConnections
      |> Seq.length
      |> should equal 1

      neuronConnections
      |> Seq.head
      |> (fun x -> x.Value)
    neuronConnection.Weight
    |> should (equalWithin 0.1) 20.0

    let mutations = [ResetWeights]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  let sensorId = (fst sensor)

  let neuronId = neuron |> fst
  let neuron =
    nodeRecords
    |> Map.find neuronId

  let neuronConnection =
    neuron.InboundConnections
    |> Seq.length
    |> should equal 1

    neuron.InboundConnections
    |> Seq.head
    |> (fun x -> x.Value)
  
  neuronConnection.Weight
  |> should not' (equal 20.0)

  neuronConnection.Weight
  |> should not' (equal 1.0)

  neuronConnection.Weight
  |> should not' (equal 0.0)

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should not' (equal 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``RemoveSensorLink mutation should remove a sensor connection randomly in the neural network`` () =
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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

  let neuronId = neuron |> fst
  let initialNumberOfNeuronConnections = weights |> List.length
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find neuronId
    |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)
    let mutations = [RemoveSensorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find neuronId
  |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should be (lessThan initialNumberOfNeuronConnections))

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 100.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``RemoveActuatorLink mutation should remove an actuator connection randomly in the neural network`` () =
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 200.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let acuatorId = actuator |> fst
  let initialNumberOfNeuronConnections = 2
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find actuatorId
    |> (fun actuatorRecord -> actuatorRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)
    let mutations = [RemoveActuatorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find actuatorId
  |> (fun actuatorRecord -> actuatorRecord.InboundConnections |> Seq.length |> should equal 1)

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 100.0)

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``RemoveInboundConnection mutation should remove some neuron connection randomly in the neural network`` () =
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
    let layer = 4
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance
  let neuronTwo =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 3
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
  let neuronWeight = 1.0
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuronTwo |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 300.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuronId = neuronTwo |> fst
  let initialNumberOfNeuronConnections = 3
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN neuronTwo
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find neuronId
    |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)
    let mutations = [RemoveInboundConnection]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find neuronId
  |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal 2)

  [|
    sensor
    neuron
    neuronTwo
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 200.0)

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``RemoveOutboundConnection mutation should remove some neuron connection randomly in the neural network`` () =
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
    let layer = 4
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
    createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance
  let neuronTwo =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 3
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
  let neuronWeight = 1.0
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuron |> connectNodeToNeuron neuronTwo neuronWeight
  neuronTwo |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 300.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuronId = neuronTwo |> fst
  let initialNumberOfNeuronConnections = 3
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN neuronTwo
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find neuronId
    |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)
    let mutations = [RemoveOutboundConnection]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find neuronId
  |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal 2)

  [|
    sensor
    neuron
    neuronTwo
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.001) 200.0)

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

let ``RemoveSensorLink mutation should not remove a sensor connection when a node only has one connection`` () =
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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
    ]
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 30.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuronId = neuron |> fst
  let initialNumberOfNeuronConnections = weights |> List.length
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find neuronId
    |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)
    let mutations = [RemoveSensorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find neuronId
  |> (fun neuronRecord -> neuronRecord.InboundConnections |> Seq.length |> should equal initialNumberOfNeuronConnections)

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 100.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

let ``ChangeNeuronLayer mutation should change a neuron's layer randomly`` () =
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
    let layer = 0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 1
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
    ]
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should (equalWithin 0.1) 30.0)

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuronId = neuron |> fst
  let nodeRecords =
    let initialNodeRecords =
      Array.empty
      |> addNeuronToNN actuator
      |> addNeuronToNN neuron
      |> addNeuronToNN sensor
      |> constructNodeRecords
    initialNodeRecords
    |> Map.find neuronId
    |> (fun neuronRecord -> neuronRecord.Layer |> should equal 1)
    let mutations = [RemoveSensorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  nodeRecords
  |> Map.find neuronId
  |> (fun neuronRecord -> neuronRecord.Layer |> should not' (equal 1))

  [|
    sensor
    neuron
    actuator
  |]
  |> killNeuralNetworkArray

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> should equal 100.0

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply
