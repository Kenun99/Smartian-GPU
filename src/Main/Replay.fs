module Smartian.Replay

open System
open System.Runtime.InteropServices

open Argu
open Utils
open BytesUtils
open Executor
open Runner

type ReplayerCLI =
  | [<AltCommandLine("-p")>] [<Mandatory>] [<Unique>] Program of path: string
  | [<AltCommandLine("-k")>] [<Unique>] Kernel of path: string
  | [<AltCommandLine("-g")>] [<Unique>] GPU of int
  | [<AltCommandLine("-i")>] [<Mandatory>] [<Unique>] InputDir of path: string
  | [<AltCommandLine("-t")>] [<Unique>] Interval of time: int
  | [<Unique>] NoDDFA
  | [<Unique>] CheckOptionalBugs
  | [<Unique>] UseOthersOracle
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Program _ -> "Target program for test case replaying"
      | Kernel _ -> "Target PTX for test case generation with GPU fuzzing."
      | GPU _ -> "The GPU device ID to bind the fuzzer."
      | InputDir _ -> "Directory of testcases to replay"
      | Interval _ -> "Time interval (in minutes) for coverage report"
      | NoDDFA -> "Disable dynamic data-flow analysis during the fuzzing."
      | CheckOptionalBugs ->
        "Detect optional bugs (e.g. requirement violation) disabled by default."
      | UseOthersOracle ->
        "Report bugs using other tools' oracles as well.\n\
        Currently we support (BD/IB/ME/RE) X (sFuzz/ILF/Mythril/MANTICORE)."

type ReplayOption = {
  Program           : string
  KernelPath        : string
  GPU               : int
  TestcaseDir       : string
  TimeInterval      : int
  DynamicDFA        : bool
  CheckOptionalBugs : bool
  UseOthersOracle   : bool
}

let parseReplayOption args =
  let cmdPrefix = "dotnet Smartian.dll replay"
  let parser = ArgumentParser.Create<ReplayerCLI> (programName = cmdPrefix)
  let r = try parser.Parse(args) with
          :? Argu.ArguParseException -> printLine (parser.PrintUsage()); exit 1
  { Program = System.IO.Path.GetFullPath(r.GetResult (<@ Program @>))
    KernelPath = r.GetResult (<@ Kernel @>, defaultValue = "")
    GPU = r.GetResult (<@ GPU @>, defaultValue = 0)
    TestcaseDir = r.GetResult (<@ InputDir @>)
    TimeInterval = r.GetResult (<@ Interval @>, defaultValue = 0)
    DynamicDFA = not (r.Contains(<@ NoDDFA @>)) // Enabled by default.
    CheckOptionalBugs = r.Contains(<@ CheckOptionalBugs @>)
    UseOthersOracle = r.Contains(<@ UseOthersOracle @>) }

let extractElapsedTime (tcFile: string) =
  let name = System.IO.Path.GetFileName(tcFile)
  let tokens = name.Split([| '_' |])
  float <| tokens.[Array.length tokens - 1]

let sortTCs tcDir =
  let sorter tcFile = try Some (extractElapsedTime tcFile) with _ -> None
  System.IO.Directory.EnumerateFiles(tcDir) |> Seq.toList |> List.sortBy sorter

let getNumBuckets timeInterval elapsedTimes =
  let maxElapsed = elapsedTimes |> List.max
  int (maxElapsed / timeInterval) + 1

let categorizeTC timeInterval (buckets: string list []) tcFile elapsedTime =
  let idx = int (elapsedTime / timeInterval) + 1
  // No need to sort tcs in same bucket
  buckets.[idx] <- tcFile :: buckets.[idx]

let bucketizeTCs timeInterval tcDir =
  let timeIntervalInSec = float (timeInterval * 60)
  let tcFiles = System.IO.Directory.EnumerateFiles(tcDir) |> Seq.toList
  let elapsedTimes = List.map extractElapsedTime tcFiles
  if List.isEmpty tcFiles then printfn "[Warning] No test case generated"; [| |]
  else let nBuckets = getNumBuckets timeIntervalInSec elapsedTimes
       let buckets = Array.create (nBuckets + 1) []
       List.iter2 (categorizeTC timeIntervalInSec buckets) tcFiles elapsedTimes
       buckets

let runReportMode opt =
  let testcaseDir = opt.TestcaseDir
  let timeInterval = opt.TimeInterval
  let traceDU = opt.DynamicDFA
  let checkOptionalBugs = opt.CheckOptionalBugs
  let useOthersOracle = opt.UseOthersOracle
  let buckets = bucketizeTCs timeInterval testcaseDir
  for i = 0 to Array.length buckets - 1 do
    for file in buckets.[i] do
      let tcStr = System.IO.File.ReadAllText file
      let tc = TestCase.fromJson tcStr
      execute tc true traceDU checkOptionalBugs useOthersOracle
      |> ignore
    let elapsed = i * timeInterval
    let edges = accumEdges.Count
    printfn "%02dm: %d Edges, %d Instrs" elapsed edges accumInstrs.Count

let runDefaultMode opt =
  let testcaseDir = opt.TestcaseDir
  let traceDU = opt.DynamicDFA
  let checkOptionalBugs = opt.CheckOptionalBugs
  let useOthersOracle = opt.UseOthersOracle
  log "Start replaying test cases in : %s" testcaseDir
  let mutable totalElapsed = 0.0
  for file in sortTCs testcaseDir do
    let tcStr = System.IO.File.ReadAllText file
    let tc = TestCase.fromJson tcStr
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    log "Replaying test case: %s" file
    let feedback = execute tc true traceDU checkOptionalBugs useOthersOracle
    stopWatch.Stop()
    totalElapsed <- totalElapsed + stopWatch.Elapsed.TotalMilliseconds
    TCManage.printBugInfo feedback.BugSet
  log "Covered Edges : %d" accumEdges.Count
  log "Covered Instructions: %d" accumInstrs.Count
  log "Elapsed time (ms): %.4f" totalElapsed

let runCudaMode opt =
  assertFileExists opt.KernelPath
  Runner.initialize opt.GPU opt.KernelPath
  let testcaseDir = opt.TestcaseDir
  let timeInterval = opt.TimeInterval
  let buckets = bucketizeTCs timeInterval testcaseDir
  for i = 0 to Array.length buckets - 1 do
    for file in buckets.[i] do
      // log "Replaying test case: %s" file
      let tcStr = System.IO.File.ReadAllText file
      let tc = TestCase.fromJson tcStr
      let txs = tc.Txs
      let deployTx = tc.DeployTx
      Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE deployTx.From, 
                       uint64 deployTx.Timestamp, uint64 deployTx.Blocknum) |> ignore
      if Runner.cuDeployTx(Runner.cuModule, uint64 deployTx.Value, deployTx.Data, uint32 deployTx.Data.Length) <> true then
        raise <| Runner.CudaException("DeployFail", Runner.CUresult.CUDA_ERROR_LAUNCH_FAILED)
      let argTypeMap = [|68uy|]
      for tx in txs do
        Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE tx.From, 
                        uint64 tx.Timestamp, uint64 tx.Blocknum) |> ignore
        cuDataCpy(Runner.dSeed, uint64 tx.Value, tx.Data, uint32 tx.Data.Length) |> ignore
        if Runner.cuRunTxs(Runner.cuModule, Runner.dSeed, argTypeMap, 0) = 0UL then
          log "[*] Fail to execute transaction: %s" (TXData.toJson "      " tx)
      Runner.postGainCov(Runner.cuModule) |> ignore

    let elapsed = i * timeInterval
    let edges = Runner.obtainCov Runner.cuModule
    printfn "%02dm: %d Edges, %d Instrs" elapsed edges 0
  Runner.destroy ()

let runCollectMode opt =
  let testcaseDir = opt.TestcaseDir
  let timeInterval = opt.TimeInterval
  let buckets = bucketizeTCs timeInterval testcaseDir
  let mutable edges = 0
  for i = 0 to Array.length buckets - 1 do
    for file in buckets.[i] do
      // log "Replaying test case: %s" file
      let covStr = System.IO.File.ReadAllText file
      let covTuple = covStr.Split [|' '|] // (edges, instructions)
      let vald = covTuple.[0] |> int
      if edges < vald then edges <- vald

    let elapsed = i * timeInterval
    printfn "%02dm: %d Edges, %d Instrs" elapsed edges 0

/// Replay test cases in the given directory on target program.
let run args =
  let opt = parseReplayOption args
  let program = opt.Program
  assertFileExists program
  Executor.initialize program
  // log "file in %s %d" opt.KernelPath opt.KernelPath.Length
  if opt.TimeInterval <> 0 then 
    if opt.KernelPath.Length = 0 then runReportMode opt
    elif opt.KernelPath = "NULL" then runCollectMode opt
    else runCudaMode opt 
  else runDefaultMode opt