module NeuralFish.Exporter

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exceptions

let constructNodeRecords (liveNeurons : NeuralNetwork) : NodeRecords =
  liveNeurons
  |> Map.map(fun nodeRecordId (_,neuronInstance) -> GetNodeRecord |> neuronInstance.PostAndReply)

let constructNeuralNetwork (neuralNetProperties : ConstructNeuralNetworkProperties) : NeuralNetwork =
  let activationFunctions = neuralNetProperties.ActivationFunctions
  let syncFunctions = neuralNetProperties.SyncFunctions
  let outputHooks = neuralNetProperties.OutputHooks
  let nodeRecords = neuralNetProperties.NodeRecords
  let infoLog = neuralNetProperties.InfoLog
  let createNeuronFromRecord nodeId (nodeRecord : NodeRecord) =
    let (neuronId, neuronInstance) =
      match nodeRecord.NodeType with
      | NodeRecordType.Neuron ->
        if (nodeRecord.ActivationFunctionId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Neuron with id %A does not have a Activation function id" nodeRecord.NodeId)
        //TODO tryfind goes here with exceptions
        let activationFunction = activationFunctions |> Map.find nodeRecord.ActivationFunctionId.Value
        nodeRecord
        |> createNeuronFromRecord activationFunction
        |> createNeuronInstance infoLog
      | NodeRecordType.Sensor _ ->
        if (nodeRecord.SyncFunctionId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Sensor with id %A does not have a sync function id" nodeRecord.NodeId)
        //TODO tryfind goes here with exceptions
        let syncFunction = syncFunctions |> Map.find nodeRecord.SyncFunctionId.Value
        nodeRecord
        |> createSensorFromRecord syncFunction
        |> createNeuronInstance infoLog
      | NodeRecordType.Actuator ->
        if (nodeRecord.OutputHookId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Actuator with id %A does not have a Output Hook function id" nodeRecord.NodeId)
        //TODO tryfind goes here with exceptions
        let outputHook = outputHooks |> Map.find nodeRecord.OutputHookId.Value
        nodeRecord
        |> createActuatorFromRecord outputHook
        |> createNeuronInstance infoLog
    neuronInstance

  let connectNeurons (liveNeurons : NeuralNetwork) =
    let connectNode targetNodeId (targetLayer : NeuronLayerId, targetNode : NeuronInstance) =
      let processRecordConnections node =
        let findNeuronAndAddToOutboundConnections (inboundConnection : InactiveNeuronConnection) (targetNodeId : NeuronId) =
          if not <| (liveNeurons |> Map.containsKey targetNodeId) then
            raise (NeuronInstanceException <| sprintf "Trying to connect and can't find a neuron with id %A" targetNodeId)
          let fromLayer, fromNode =
            liveNeurons
            |> Map.find inboundConnection.NodeId

          //Set connection in live neuron
          {
            ConnectionOrderOption = inboundConnection.ConnectionOrder
            ToNodeId = targetNodeId
            InitialWeight = inboundConnection.Weight
          }
          |> (fun partialOutbound r -> ((node.NodeType, targetNode, targetLayer, partialOutbound), r) |> NeuronActions.AddOutboundConnection)
          |> fromNode.PostAndReply

        sprintf "%A has inbound connections %A" targetNodeId node.InboundConnections |> infoLog
        node.InboundConnections
        |> Seq.iter (fun inboundConnection -> findNeuronAndAddToOutboundConnections inboundConnection node.NodeId)
      nodeRecords
      |> Map.find targetNodeId
      |> processRecordConnections

    liveNeurons |> Map.iter connectNode
    liveNeurons

  let rec waitOnNeuralNetwork neuralNetworkToWaitOn : NeuralNetwork =
    let checkIfNeuralNetworkIsActive (neuralNetwork : NeuralNetwork) =
      //returns true if active
      neuralNetwork
      |> Map.forall(fun i (_,neuron) -> neuron.CurrentQueueLength <> 0)
    if neuralNetworkToWaitOn |> checkIfNeuralNetworkIsActive then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      System.Threading.Thread.Sleep(200)
      waitOnNeuralNetwork neuralNetworkToWaitOn
    else
      neuralNetworkToWaitOn

  nodeRecords
  |> Map.map createNeuronFromRecord
  |> connectNeurons
  |> waitOnNeuralNetwork

let getDefaultNodeRecords activationFunctions
  outputHookFunctionIds
    syncFunctionId
      learningAlgorithm
        infoLog : NodeRecords =
  let addNeuronToMap (neuronId, neuronInstance) =
    Map.add neuronId neuronInstance
  let actuator =
    let layer = 0
    let fakeOutputHook = (fun x -> ())
    let nodeId = 0
    let outputHookId =
      match outputHookFunctionIds |> Seq.tryHead with
      | Some outputHookId -> outputHookId
      | None -> raise <| ActuatorRecordDoesNotHaveAOutputHookIdException "Attempted to generate default training properties but no output hook ids were passed"
    createActuator nodeId layer fakeOutputHook outputHookId
    |> createNeuronInstance infoLog
  let sensor =
    let nodeId = 1
    let maximumVectorLength = 1
    createSensor nodeId (fun () -> Seq.empty) syncFunctionId maximumVectorLength
    |> createNeuronInstance infoLog
  let neuron =
    let bias = 0.0
    let nodeId = 2
    let layer = 1
    let activationFunctionId, activationFunction =
      match activationFunctions |> Map.toSeq |> Seq.tryHead with
      | Some xTuple -> xTuple
      | None ->
        raise <| NeuronDoesNotHaveAnActivationFunction "Attempted to generate default traing properties but no activation functions were passed"
    createNeuron nodeId layer activationFunction activationFunctionId bias learningAlgorithm
    |> createNeuronInstance infoLog
  let weight = 0.0
  sensor |> connectSensorToNode neuron [weight]
  neuron |> connectNodeToActuator actuator

  let neuralNetwork =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
  let nodeRecords =
    neuralNetwork
    |> constructNodeRecords
  neuralNetwork |> killNeuralNetwork
  nodeRecords
