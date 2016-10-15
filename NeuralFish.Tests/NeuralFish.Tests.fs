module NeuralFish.Tests.NeuralNet

open NUnit.Framework
open FsUnit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Tests.TestHelper

[<Test; Timeout(5000)>]
let ``When the Sensor receives the sync message, the neural circuit should activate causing the actuator to output some value`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001

  //Create Neurons
  let actuator =
    createActuator actuatorId testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let activationFunctionId = 0
    let activationFunction = id
    let bias = 10.0
    let nodeId = getNodeId()
    createNeuron nodeId activationFunction activationFunctionId bias
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
  |> (should equal 110)

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 1

[<Test; Timeout(5000)>]
let ``The NeuralFish should be able to solve the XNOR problem with predefined weights`` () =
  //(class.coursera.org/ml/lecture/48)
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()

  let activationFunction = sigmoid
  let activationFunctionId = 0

  let actuator =
    let id = getNodeId()
    createNeuronInstance <| createActuator id testHook 0
  let neuron_a3_1 =
    let activationFunction = sigmoid
    let bias = -10.0
    let id = getNodeId()
    createNeuron id activationFunction 0 bias
    |> createNeuronInstance
  let neuron_a2_2 =
    let activationFunction = sigmoid
    let bias = 10.0
    let id = getNodeId()
    createNeuron id activationFunction activationFunctionId bias
    |> createNeuronInstance
  let neuron_a2_1 =
    let bias = -30.0
    let id = getNodeId()
    createNeuron id activationFunction activationFunctionId bias
    |> createNeuronInstance
  let sensor_x1 =
    let syncFunction = fakeDataGenerator([[0.0]; [0.0]; [1.0]; [1.0]])
    let syncFunctionId = 0
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
    |> createNeuronInstance
  let sensor_x2 =
    let syncFunction = fakeDataGenerator([[0.0]; [1.0]; [0.0]; [1.0]])
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

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 4
