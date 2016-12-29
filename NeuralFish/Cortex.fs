module NeuralFish.Cortex

open NeuralFish.Types
open NeuralFish.Exceptions
open NeuralFish.Core
open NeuralFish.Exporter

let createCortex thinkTimeout infoLog liveNeurons : CortexInstance =
  let rec waitOnAcutuators (stopwatch : System.Diagnostics.Stopwatch) neuralNetworkToWaitOn =
    let checkIfActuatorsAreReady (neuralNetwork : NeuralNetwork) =
      //returns true if active
      let actuatorIsActive (neuron : NeuronInstance) =
        let maybeActuatorStatus =
          CheckActuatorStatus
          |> neuron.TryPostAndReply
        match maybeActuatorStatus with
        | None -> raise <| NeuronInstanceUnavailableException "Cortex - Neuron instance is not available when trying to check actuators"
        | Some actuatorReady -> actuatorReady
      let checkIfNeuronIsBusy (neuron : NeuronInstance) = neuron.CurrentQueueLength <> 0
      neuralNetwork
      |> Map.exists(fun i (_,neuron) -> neuron |> checkIfNeuronIsBusy || not <| actuatorIsActive neuron  )
    if neuralNetworkToWaitOn |> checkIfActuatorsAreReady then
      let timeIsUp = System.BitConverter.DoubleToInt64Bits(stopwatch.Elapsed.TotalMilliseconds) >= (int64 thinkTimeout)
      if timeIsUp then
        false
      else
      //TODO make this configurable
        System.Threading.Thread.Sleep(50)
        waitOnAcutuators stopwatch neuralNetworkToWaitOn
    else
      true

  let registerCortex (neuralNetwork : NeuralNetwork) cortex =
    let sendCortexToActuatorAsync (neuronInstance : NeuronInstance) =
      (fun r -> RegisterCortex (cortex,r)) |> neuronInstance.PostAndAsyncReply
    neuralNetwork
    |> Seq.map (fun keyValue -> keyValue.Value |> snd |> sendCortexToActuatorAsync)
    |> Seq.toArray
    |> Async.Parallel
    |> ignore
    cortex
  let resetNeuralNetwork (neuralNetwork : NeuralNetwork) cortex =
    let resetNeuron (neuronInstance : NeuronInstance) =
      ResetNeuron |> neuronInstance.PostAndReply
    neuralNetwork
    |> Seq.iter (fun keyValue -> keyValue.Value |> snd |> resetNeuron)
    cortex

  let sendRecurrentSignals neuralNetwork cortex =
    neuralNetwork
    |> Map.iter(fun _ (_, neuronInstance : NeuronInstance) -> SendRecurrentSignals |> neuronInstance.PostAndReply)
    cortex

  let stopwatch = System.Diagnostics.Stopwatch()
  CortexInstance.Start(fun inbox ->
    let rec loop liveNeurons =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          return! loop liveNeurons
        | Some msg ->
          match msg with
          | ThinkAndAct replyChannel ->
            "Cortex - Starting think cycle" |> infoLog
            liveNeurons |> synchronizeNN
            //Sleep to give the NN a chance to process initial messages
            //TODO synchronize should post back after finishing
            System.Threading.Thread.Sleep(100)
            "Cortex - Waiting on Neural Network to finish" |> infoLog
            stopwatch.Restart()
            let shouldActivateActuators = liveNeurons |> waitOnAcutuators stopwatch
            stopwatch.Stop()
            if shouldActivateActuators then
              "Cortex - Think cycle finished. Activating Actuators" |> infoLog
              liveNeurons |> activateActuators
              "Cortex - Actuators Activated" |> infoLog
              ThinkCycleFinished |> replyChannel.Reply
            else
              resetNeuralNetwork liveNeurons inbox |> ignore
              ThinkCycleIncomplete |> replyChannel.Reply
            return! loop liveNeurons
          | KillCortex replyChannel ->
            "Cortex - Gathering Updated Node Records" |> infoLog
            let updatedNodeRecords = liveNeurons |> constructNodeRecords
            "Cortex - Killing Neural Network" |> infoLog
            liveNeurons |> killNeuralNetwork
            "Cortex - Replying with Updated records" |> infoLog
            updatedNodeRecords |> replyChannel.Reply
            ()
      }
    loop liveNeurons
  )
  |> registerCortex liveNeurons
  |> resetNeuralNetwork liveNeurons
  |> sendRecurrentSignals liveNeurons
  |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> infoLog); x)
