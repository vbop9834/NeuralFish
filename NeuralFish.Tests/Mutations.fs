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
    let initialNodeRecords =
      Map.empty
      |> addNeuronToMap actuator
      |> addNeuronToMap neuron
      |> addNeuronToMap sensor
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

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuronRecords =
    nodeRecords
    |> Map.filter (fun _ nodeRecord -> nodeRecord.NodeType = NodeRecordType.Neuron)
  neuronRecords
  |> Seq.length
  |> should be (greaterThan 1)

  let originalNeuron = neuronRecords |> Map.find (fst neuron)
  neuronRecords
  |> Map.exists(fun _ record -> System.BitConverter.DoubleToInt64Bits(record.Layer) <> System.BitConverter.DoubleToInt64Bits(originalNeuron.Layer))
  |> should equal true

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
