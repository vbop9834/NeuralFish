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
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddBias]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 5.0
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
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [RemoveBias]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork
  let newSensor =
    let sensorId = (fst sensor)
    let layer = 1
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
let ``MutateWeights mutation should mutate the weights of some inbound connections in a neuron`` () =
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
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
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
    neuronConnection.Weight
    |> should (equalWithin 0.1) 20.0

    let mutations = [MutateWeights]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  let sensorId = (fst sensor)
  let newSensor =
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let newWeight =
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
    
    neuronConnection.Weight
    |> should not' (equal 20.0)

    neuronConnection.Weight
    |> should not' (equal 1.0)

    neuronConnection.Weight
    |> should not' (equal 0.0)

    neuronConnection.Weight

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should not' (equal 0.0))

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
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddOutboundConnection]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork
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
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddNeuron]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork
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
    |> Map.add secondSyncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddSensor]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId; secondSyncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  nodeRecords
  |> Map.filter (fun key record -> match record.NodeType with | NodeRecordType.Sensor _ -> true | _ -> false)
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
    |> Map.add secondOutputHookId testHook

  let nodeRecords =
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddActuator]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId; secondOutputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

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

  let nodeRecords =
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddSensorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  let totalSensorOutboundConnections =
    let sensorId = sensor |> fst
    let sensorToNeuronConnections =
      let neuronId = neuron |> fst
      nodeRecords
      |> Map.find neuronId
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = sensorId)
      |> Seq.length
    let sensorToActuatorConnections =
      let actuatorId = actuator |> fst
      nodeRecords
      |> Map.find actuatorId
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = sensorId)
      |> Seq.length
    sensorToNeuronConnections + sensorToActuatorConnections

  let previousWeightLength = weights |> List.length

  totalSensorOutboundConnections
  |> should be (greaterThan previousWeightLength)

  neuralNetwork |> synchronizeNN
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.0))

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``AddActuatorLink mutation should add an actuator connection randomly in the neural network`` () =
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

  let nodeRecords =
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [AddActuatorLink]
    {
      Mutations = mutations
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork

  //Adding 1 for the neuron to actuator connection
  let previousNumberOfConnections = (weights |> List.length) + 1

  let neuronId = neuron |> fst
  let neuronRecord =
    nodeRecords
    |> Map.find neuronId
  let actuatorRecord =
    let actuatorId = actuator |> fst
    nodeRecords
    |> Map.find actuatorId
  let sensorId = sensor |> fst
  let sensorRecord =
    nodeRecords
    |> Map.find sensorId
  let totalSensorOutboundConnections =
    let sensorToNeuronConnections =
      neuronRecord
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = sensorId)
      |> Seq.length
    let sensorToActuatorConnections =
      actuatorRecord
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = sensorId)
      |> Seq.length
    sensorToNeuronConnections + sensorToActuatorConnections
  let totalNeuronOutboundConnections =
    let neuronToSensorConnections =
      sensorRecord
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = neuronId)
      |> Seq.length
    let neuronToActuatorConnections =
      actuatorRecord
      |> (fun x -> x.InboundConnections)
      |> Seq.filter(fun inboundConnection -> inboundConnection.NodeId = neuronId)
      |> Seq.length
    neuronToSensorConnections + neuronToActuatorConnections

  (totalSensorOutboundConnections + totalNeuronOutboundConnections)
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
    |> Map.add secondActivationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let nodeRecords =
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    let mutations = [MutateActivationFunction]
    {
      Mutations = mutations
      ActivationFunctionIds = [secondActivationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


  let neuralNetwork =
   {
     ActivationFunctions = activationFunctions
     SyncFunctions = syncFunctions
     OutputHooks = outputHooks
     NodeRecords = nodeRecords
     InfoLog = defaultInfoLog
   } |> constructNeuralNetwork
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
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
      |> constructNodeRecords
    {
      Mutations = minimalMutationSequence
      ActivationFunctionIds = [activationFunctionId]
      SyncFunctionIds = [syncFunctionId]
      OutputHookFunctionIds = [outputHookId]
      LearningAlgorithm = NoLearning
      InfoLog = defaultInfoLog
      NodeRecords = initialNodeRecords
    } |> mutateNeuralNetwork

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork


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
      let dataStream = 
        [
          data
          (Seq.append data data)
          data
          (Seq.append data data)
          (Seq.append data data)
          data
        ]
      fakeDataGenerator([data])
    )

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
     |> (fun score -> score, ContinueGeneration)
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
  |> Array.length
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
      let learningAlgorithm = Hebbian 0.9
      let defaultTrainingProperties = getDefaultTrainingProperties trainingSet interpretActuatorOutputFunction scoreAnswer activationFunctions outputHookFunctionIds learningAlgorithm defaultInfoLog
      { defaultTrainingProperties with
          ShuffleDataSet = true
          MaximumMinds = maximumMinds
      }
    trainingProperties |> trainSingleScopeProblem
  evolvedRecords
  |> Array.length
  |> should equal maximumMinds

[<Fact>]
let ``Should be able to end a generation via the fitness function`` () =
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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
     |> (fun score -> score, EndGeneration)
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
  let maximumMinds = 5
  let evolvedRecords =
    let generationRecords =
      Map.empty
      |> Map.add 0 nodeRecords
    let evolutionProperties =
      { defaultEvolutionProperties with
          MaximumMinds = 5
          Generations = 10
          MaximumThinkCycles = 99999
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
  |> Array.length
  |> should equal maximumMinds
