module Smartian.TCManage

open System
open System.Runtime.InteropServices
exception BreakException

open Nethermind.Evm
open Executor
open Options
open Utils
open BytesUtils
open Runner


(*** Directory paths ***)

let mutable tcDir = ""
let mutable bugDir = ""

let initialize outDir =
  tcDir <- System.IO.Path.Combine(outDir, "testcase")
  System.IO.Directory.CreateDirectory(tcDir) |> ignore
  bugDir <- System.IO.Path.Combine(outDir, "bug")
  System.IO.Directory.CreateDirectory(bugDir) |> ignore

(*** Statistics ***)

let mutable private totalTC = 0
let mutable private totalBug = 0
let mutable private totalAF = 0
let mutable private totalAW = 0
let mutable private totalBD = 0
let mutable private totalCH = 0
let mutable private totalEL = 0
let mutable private totalIB = 0
let mutable private totalME = 0
let mutable private totalMS = 0
let mutable private totalRE = 0
let mutable private totalSC = 0
let mutable private totalTO = 0
let mutable private totalFE = 0
let mutable private totalRV = 0

let checkFreezingEtherBug () =
  if receivedEther && useDelegateCall && not canSendEther then
    totalFE <- totalFE + 1

let printStatistics () =
  log "Total Executions: %d" totalExecutions
  log "Deployment failures: %d" deployFailCount
  log "Test Cases: %d" totalTC
  log "Covered Edges: %d" accumEdges.Count
  log "Covered Instructions: %d" accumInstrs.Count
  log "Covered Def-Use Chains: %d" accumDUChains.Count
  log "Found Bugs:"
  log "  Assertion Failure: %d" totalAF
  log "  Arbitrary Write: %d" totalAW
  log "  Block state Dependency: %d" totalBD
  log "  Control Hijack: %d" totalCH
  log "  Ether Leak: %d" totalEL
  log "  Integer Bug: %d" totalIB
  log "  Mishandled Exception: %d" totalME
  log "  Multiple Send: %d" totalMS
  log "  Reentrancy: %d" totalRE
  log "  Suicidal Contract: %d" totalSC
  log "  Transaction Origin Use: %d" totalTO
  log "  Freezing Ether: %d" totalFE
  log "  Requirement Violation: %d" totalRV

let getTestCaseCount () =
  totalTC

(*** Record of paths and bugs ***)

let private updateBugCountAux (bugClass, _, _) =
  match bugClass with
  | BugClass.AssertionFailure -> totalAF <- totalAF + 1
  | BugClass.ArbitraryWrite -> totalAW <- totalAW + 1
  | BugClass.BlockstateDependency -> totalBD <- totalBD + 1
  | BugClass.ControlHijack -> totalCH <- totalCH + 1
  | BugClass.EtherLeak -> totalEL <- totalEL + 1
  | BugClass.IntegerBug -> totalIB <- totalIB + 1
  | BugClass.MishandledException -> totalME <- totalME + 1
  | BugClass.MultipleSend -> totalMS <- totalMS + 1
  | BugClass.Reentrancy -> totalRE <- totalRE + 1
  | BugClass.SuicidalContract -> totalSC <- totalSC + 1
  | BugClass.TransactionOriginUse -> totalTO <- totalTO + 1
  | BugClass.RequirementViolation -> totalRV <- totalRV + 1
  | _ -> ()

let private updateBugCount bugSet =
  Set.iter updateBugCountAux bugSet

(*** Test case storing functions ***)

let printBugInfo bugSet =
  let iterator (bugClass, pc, txIdx) =
    log "Tx#%d found %s at %x" txIdx (BugClassHelper.toString bugClass) pc
  Set.iter iterator bugSet

let private decideBugTag bugSet =
  Set.map (fun (bugType, _, _) -> bugType) bugSet
  |> Set.map BugClassHelper.toTag
  |> String.concat "-"

let private dumpBug opt seed bugSet =
  printBugInfo bugSet
  updateBugCount bugSet
  let tag = decideBugTag bugSet
  let tc = Seed.concretize seed
  let tcStr = TestCase.toJson tc
  let tcName = sprintf "id-%05d-%s_%05d" totalBug tag (elapsedSec())
  let tcPath = System.IO.Path.Combine(bugDir, tcName)
  if opt.Verbosity >= 0 then
    log "[*] Save bug seed %s: %s" tcName (Seed.toString seed)
  System.IO.File.WriteAllText(tcPath, tcStr)
  totalBug <- totalBug + 1

let private dumpTestCase opt seed =
  let tc = Seed.concretize seed
  let tcStr = TestCase.toJson tc
  let tcName = sprintf "id-%05d_%05d" totalTC (elapsedSec())
  let tcPath = System.IO.Path.Combine(tcDir, tcName)
  if opt.Verbosity >= 1 then
    log "[*] Save new seed %s: %s" tcName (Seed.toString seed)
  System.IO.File.WriteAllText(tcPath, tcStr)
  totalTC <- totalTC + 1

let evalAndSave opt seed =
  let covGain, duGain, bugSet = Executor.getCoverage opt seed
  if Set.count bugSet > 0 then dumpBug opt seed bugSet
  if covGain then dumpTestCase opt seed
  if not covGain && duGain && opt.Verbosity >= 2 then
    log "[*] Internal new seed: %s" (Seed.toString seed)
  covGain || duGain // Returns whether this seed is meaningful.

let evalAndSaveCuda opt seed = 
  Executor.incrExecutionCount ()
  let tc = Seed.concretize seed
  let deployTx = tc.DeployTx
  let txs = tc.Txs
  Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE deployTx.From, uint64 deployTx.Timestamp, 
            uint64 deployTx.Blocknum) |> ignore

  if Runner.cuDeployTx(Runner.cuModule, uint64 deployTx.Value, deployTx.Data, uint32 deployTx.Data.Length) <> true then
    raise <| Runner.CudaException("DeployFail", Runner.CUresult.CUDA_ERROR_LAUNCH_FAILED)

  for tx in txs do
    Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE tx.From, 
              uint64 tx.Timestamp, uint64 tx.Blocknum) |> ignore
    Runner.cuRunTxsInGroup(Runner.cuModule, Runner.dSeed, uint64 tx.Value, tx.Data, uint32 tx.Data.Length) |> ignore
  
  Runner.cuCaptureBugs(Runner.dSignals, elapsedStr() ) |> ignore
  Runner.postCov(Runner.cuModule)
  let covGain = Runner.gainCov(uint32 0)
  let duGain = Runner.gainBug(uint32 0)
  covGain || duGain

let runInGroup opt seeds = 
  let seedCnt = List.length seeds
  Executor.incrGroupExecutionCount seedCnt
  let TxsLenMax = seeds |> List.map (fun x -> x |> Seed.getTransactionCount) |> List.max 
  // log "txs #%d; max tx len = %d" seeds.Length TxsLenMax

  // deployment transaction
  let tc = Seed.concretize seeds.[0]
  let deployTx = tc.DeployTx
  Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE deployTx.From, uint64 deployTx.Timestamp, 
                    uint64 deployTx.Blocknum) |> ignore
  if Runner.cuDeployTx(Runner.cuModule, uint64 deployTx.Value, deployTx.Data, uint32 deployTx.Data.Length) <> true then
    raise <| Runner.CudaException("DeployFail", Runner.CUresult.CUDA_ERROR_LAUNCH_FAILED)

  // runtime transactions
  // let rec execCuda txid = 
  //   if txid < TxsLenMax then 
  //     for tid in 0 .. seeds.Length - 1 do
  //       let seed = seeds.[tid]
  //       let dev = Runner.dSeed + uint64 tid * uint64 512
  //       if Seed.getTransactionCount seed > txid then
  //         let tx = Transaction.concretize seed.Transactions.[txid]
  //         cuDataCpy(dev, uint64 tx.Value, tx.Data, uint32 tx.Data.Length) |> ignore
  //       else  cuDataCpy(dev, uint64 0, [| |], uint32 0) |> ignore
    
  //     if Runner.cuRunTxs(Runner.cuModule, Runner.dSeed) then
  //       Executor.incrGroupExecutionCount seeds.Length
  //       Runner.cuCaptureBugs(Runner.dSignals, elapsedStr()) |> ignore
  //       Runner.postCov(Runner.cuModule)
  //       execCuda (txid + 1)
  //     else log "Error to exected tx#%d in GPU" txid
    // else log "Executed all in GPU"
  // execCuda 1  

  for txid in 1 .. TxsLenMax - 1 do
    for tid in 0 .. seedCnt - 1 do
      // log "obtained txid%d tid# %d" txid tid
      let seed = seeds.[tid]
      let dev = Runner.dSeed + uint64 tid * uint64 512
      if Seed.getTransactionCount seed > txid then
        let tx = Transaction.concretize seed.Transactions.[txid]
        cuDataCpy(dev, uint64 tx.Value, tx.Data, uint32 tx.Data.Length) |> ignore
        Runner.setEVMEnv(Runner.cuModule, Address.toBytes LE tx.From, 
                         uint64 tx.Timestamp, uint64 tx.Blocknum) |> ignore
      else  cuDataCpy(dev, uint64 0, [| |], uint32 0) |> ignore
    // FIXME: https://stackoverflow.com/questions/56329377/reset-cuda-context-after-exception/56330491#56330491
    if Runner.cuRunTxs(Runner.cuModule, Runner.dSeed) then
      // log "Executed in CUDA Tx#%d : %s" txid (Transaction.toString seeds.[0].Transactions.[txid])
      Runner.cuCaptureBugs(Runner.dSignals, elapsedStr()) |> ignore
      Runner.postCov(Runner.cuModule)
    // else log "Error in GPU at Tx#%d : %s" txid (Transaction.toString seeds.[0].Transactions.[txid])

  // for tid in 0 .. seedCnt - 1 do
  //   let covGain = Runner.gainCov(uint32 sid)
  //   DarwinNotifyFeedback(?, sid)

let postEvalAndSaveCuda opt (idx, seed) =   
  let covGain = Runner.gainCov(uint32 idx)
  let duGain = Runner.gainBug(uint32 idx)  

  if duGain then
    let tc = Seed.concretize seed
    let tcStr = TestCase.toJson tc
    let tcName = sprintf "id-%05d-any_%05d" totalBug (elapsedSec())
    let tcPath = System.IO.Path.Combine(bugDir, tcName)
    if opt.Verbosity >= 0 then
      log "[*] Save bug seed %s: %s" tcName (Seed.toString seed)
    System.IO.File.WriteAllText(tcPath, tcStr)
    totalBug <- totalBug + 1

  if covGain then dumpTestCase opt seed
  if not covGain && duGain && opt.Verbosity >= 2 then
    log "[*] Internal new seed: %s" (Seed.toString seed)
  covGain || duGain // Returns whether this seed is meaningful.