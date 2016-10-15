#I __SOURCE_DIRECTORY__

#load "Types.fs"
#load "NeuralFish.fs"
#load "Exporter.fs"

open NeuralFish.Types
open NeuralFish.Exporter
open NeuralFish.Core

let testHook x = printfn "%f" x
