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
