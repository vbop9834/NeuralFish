module NeuralFish.Tests.Cortex

open Xunit
open FsUnit
open FsUnit.Xunit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Cortex
open NeuralFish.Tests.TestHelper

[<Fact>]
let ``Cortex should be able to synchronize neural activity`` () =
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let getNeuronConnectionId = getNumberGenerator()
  let syncFunctionId = 9001
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])
  let outputHookId = 9000
  let activationFunctionId = 777

  let actuatorId = getNodeId()
  let actuator =
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance

  let neuronId = getNodeId()
  let neuron =
    let activationFunction = id
    let bias = 10.0
    let layer = 2
    createNeuron neuronId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensorId = getNodeId()
  let sensor =
    createSensor sensorId syncFunction syncFunctionId 1
    |> createNeuronInstance

  neuron |> connectNodeToActuator actuator

  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq
  sensor |> connectSensorToNode neuron weights

  let neuralNetwork = 
    Array.empty
    |> addNeuronToNN actuator
    |> addNeuronToNN neuron
    |> addNeuronToNN sensor

  let cortex = neuralNetwork |> createCortex

  let thinkCycleState = ThinkAndAct |> cortex.PostAndReply
  thinkCycleState |> should equal ThinkCycleFinished
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  KillCortex
  |> cortex.PostAndReply
  |> ignore

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Cortex should shutdown a think cycle if time exceeds timeout value`` () =
  let mutable neuralNetworkProcessedThinkCycle = false
  let testHook = (fun _ -> neuralNetworkProcessedThinkCycle <- true)
  let getNodeId = getNumberGenerator()
  let syncFunctionId = 9001
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])
  let outputHookId = 9000
  let activationFunctionId = 777

  let actuatorId = getNodeId()
  let actuator =
    let layer = 3
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance

  let neuronId = getNodeId()
  let neuron =
    let activationFunction = id
    let bias = 10.0
    let layer = 2
    createNeuron neuronId layer activationFunction activationFunctionId bias NoLearning
    |> createNeuronInstance

  let sensorId = getNodeId()
  let sensor =
    createSensor sensorId syncFunction syncFunctionId 1
    |> createNeuronInstance

  //This will prevent the NN from working
  //neuron |> connectNodeToActuator actuator

  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq
  sensor |> connectSensorToNode neuron weights

  let neuralNetwork = 
    Array.empty
    |> addNeuronToNN actuator
    |> addNeuronToNN neuron
    |> addNeuronToNN sensor

  let cortex = neuralNetwork |> createCortex

  let thinkCycleState = ThinkAndAct |> cortex.PostAndReply
  thinkCycleState |> should equal ThinkCycleIncomplete

  neuralNetworkProcessedThinkCycle
  |> should equal false

  KillCortex
  |> cortex.PostAndReply
  |> ignore

