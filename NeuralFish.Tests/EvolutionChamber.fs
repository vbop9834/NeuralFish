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

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [AddBias]

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let activationFunctions : Map<ActivationFunctionId,ActivationFunction> =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    let layer = 1
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 5.0
    let nodeId = getNodeId()
    let layer = 2
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

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [RemoveBias]

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let activationFunctions : Map<ActivationFunctionId,ActivationFunction> =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
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
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2
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

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords
    |> mutateNeuralNetwork [MutateWeights]
 
  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let activationFunctions : Map<ActivationFunctionId,ActivationFunction> =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
  let newSensor =
    let sensorId = (fst sensor)
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  let newWeight =
    let sensorId = sensor |> fst
    let sensor = 
      nodeRecords
      |> Map.find sensorId
    let _,(_,weight) =
      sensor.OutboundConnections
      |> Map.toSeq
      |> Seq.head
    weight

  synchronize newSensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal newWeight)

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2
