module NeuralFish.Tests.NeuralNet

open NUnit.Framework
open FsUnit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Tests.TestHelper

[<Test>]
let ``When the Sensor receives the sync message, the neural circuit should activate causing the actuator to output some value`` () =
  let (testHook, testHookMailbox) = testHook ()

  let _, actuator = createNeuronInstance <| createActuator testHook 0
  let _, neuron =
    let activationFunction = fun x -> x
    let bias = 10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToActuator actuator
    |> createNeuronInstance
  let _, sensor =
    let syncFunction =
        let data =
          [1.0; 1.0; 1.0; 1.0; 1.0]
          |> List.toSeq
        fakeDataGenerator([data])
    let weights =
      [20.0; 20.0; 20.0; 20.0; 20.0]
      |> List.toSeq
    createSensor syncFunction 0
    |> connectSensorToNode weights neuron
    |> createNeuronInstance

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110)

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 1

[<Test>]
let ``The NeuralFish should be able to solve the XNOR problem with predefined weights`` () =
  //(class.coursera.org/ml/lecture/48)
  let (testHook, testHookMailbox) = testHook ()

  let _, actuator = createNeuronInstance <| createActuator testHook 0
  let _, neuron_a3_1 =
    let activationFunction = sigmoid
    let bias = -10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToActuator actuator
    |> createNeuronInstance
  let _, neuron_a2_2 =
    let activationFunction = sigmoid
    let bias = 10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToNeuron 20.0 neuron_a3_1
    |> createNeuronInstance
  let _, neuron_a2_1 =
    let activationFunction = sigmoid
    let bias = -30.0
    createNeuron activationFunction 0 bias
    |> connectNodeToNeuron 20.0 neuron_a3_1
    |> createNeuronInstance
  let _, sensor_x1 =
    let syncFunction = fakeDataGenerator([[0.0]; [0.0]; [1.0]; [1.0]])
    createSensor syncFunction 0
    |> connectNodeToNeuron 20.0 neuron_a2_1
    |> connectNodeToNeuron -20.0 neuron_a2_2
    |> createNeuronInstance
  let _, sensor_x2 =
    let syncFunction = fakeDataGenerator([[0.0]; [1.0]; [0.0]; [1.0]])
    createSensor syncFunction 0
    |> connectNodeToNeuron 20.0 neuron_a2_1
    |> connectNodeToNeuron -20.0 neuron_a2_2
    |> createNeuronInstance

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
