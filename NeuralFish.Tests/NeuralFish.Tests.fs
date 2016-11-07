module NeuralFish.Tests.NeuralNet

open Xunit
open FsUnit
open FsUnit.Xunit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Tests.TestHelper

[<Fact>]
let ``When the Sensor receives the sync message, the neural circuit should activate causing the actuator to output some value`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let activationFunctionId = 0
    let activationFunction = id
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let syncFunctionId = 0
    let syncFunction =
        let data =
          [1.0; 1.0; 1.0; 1.0; 1.0]
          |> List.toSeq
        fakeDataGenerator([data])
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
  |> (should equal 110.0)

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``The NeuralFish should be able to solve the XNOR problem with predefined weights`` () =
  //(class.coursera.org/ml/lecture/48)
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()

  let activationFunction = sigmoid
  let activationFunctionId = 0

  let actuator =
    let id = getNodeId()
    let layer = 4.0
    createNeuronInstance <| createActuator id layer testHook 0
  let neuron_a3_1 =
    let activationFunction = sigmoid
    let bias = -10.0
    let id = getNodeId()
    let layer = 3.0
    createNeuron id layer activationFunction 0 bias
    |> createNeuronInstance
  let neuron_a2_2 =
    let activationFunction = sigmoid
    let bias = 10.0
    let id = getNodeId()
    let layer = 2.0
    createNeuron id layer activationFunction activationFunctionId bias
    |> createNeuronInstance
  let neuron_a2_1 =
    let bias = -30.0
    let id = getNodeId()
    let layer = 2.0
    createNeuron id layer activationFunction activationFunctionId bias
    |> createNeuronInstance
  let sensor_x1 =
    let syncFunction = fakeDataGenerator([[0.0; 0.0]; [0.0; 0.0]; [1.0; 1.0]; [1.0; 1.0]])
    let syncFunctionId = 0
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
    |> createNeuronInstance
  let sensor_x2 =
    let syncFunction = fakeDataGenerator([[0.0; 0.0]; [1.0; 1.0]; [0.0; 0.0]; [1.0; 1.0]])
    let syncFunctionId = 1
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
    |> createNeuronInstance

  let weightOne = 20.0
  let weightTwo = -20.0
  sensor_x1 |> connectNodeToNeuron neuron_a2_1 weightOne
  sensor_x1 |> connectNodeToNeuron neuron_a2_2 weightTwo
  sensor_x2 |> connectNodeToNeuron neuron_a2_1 weightOne
  sensor_x2 |> connectNodeToNeuron neuron_a2_2 weightTwo

  let weight = 20.0
  neuron_a2_1 |> connectNodeToNeuron neuron_a3_1 weight
  neuron_a2_2 |> connectNodeToNeuron neuron_a3_1 weight

  neuron_a3_1 |> connectNodeToActuator actuator

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  [
    sensor_x1
    sensor_x2
    neuron_a2_1
    neuron_a2_2
    neuron_a3_1
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Should be able to handle recurrent neural connections`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let activationFunctionId = 0
    let activationFunction = id
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let syncFunctionId = 0
    let syncFunction =
        let data =
          [1.0; 1.0; 1.0; 1.0; 1.0]
          |> List.toSeq
        fakeDataGenerator([data])
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
  neuron |> connectNodeToNeuron neuron 20.0

  //Synchronize and Assert!
  //Since there is a recurrent connection then the output will
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 2310.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 46310.0)

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Should be able to handle recurrent neural network with three neurons`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let activationFunctionId = 0
  let activationFunction = id

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron_1a =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 1.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let neuron_1b =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 1.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let neuron_2a =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let syncFunctionId = 0
    let syncFunction =
        let data =
          [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
          |> List.toSeq
        fakeDataGenerator([data])
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

  sensor |> connectSensorToNode neuron_1a weights
  neuron_1a |> connectNodeToActuator actuator
  neuron_2a |> connectNodeToActuator actuator
  neuron_1a |> connectNodeToNeuron neuron_1a 20.0
  neuron_1a |> connectNodeToNeuron neuron_1b 20.0
  neuron_1b |> connectNodeToNeuron neuron_1b 20.0
  neuron_1b |> connectNodeToNeuron neuron_2a 20.0

  //Synchronize and Assert!
  //Since there is a recurrent connection then the output will
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 44320.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 1810520.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 54734520.0)

  [
    sensor
    neuron_1a
    neuron_1b
    neuron_2a
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply
