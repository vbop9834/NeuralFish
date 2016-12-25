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
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor

  let cortex = neuralNetwork |> createCortex

  ThinkAndAct |> cortex.PostAndReply
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  KillCortex
  |> cortex.PostAndReply
  |> ignore

  Die |> testHookMailbox.PostAndReply
