module Smartian.Options

open Argu
open Utils

type FuzzerCLI =
  | [<AltCommandLine("-p")>] [<Mandatory>] [<Unique>] Program of path: string
  | [<AltCommandLine("-k")>] [<Unique>] Kernel of path: string
  | [<AltCommandLine("-v")>] [<Unique>] Verbose of int
  | [<AltCommandLine("-g")>] [<Unique>] GPU of int
  | [<AltCommandLine("-t")>] [<Mandatory>] [<Unique>] Timelimit of sec: int
  | [<AltCommandLine("-o")>] [<Mandatory>] [<Unique>] OutputDir of path: string
  | [<AltCommandLine("-a")>] [<Unique>] ABIFile of path: string
  | [<Unique>] NoSDFA
  | [<Unique>] NoDDFA
  | [<Unique>] CheckOptionalBugs
  | [<Unique>] UseOthersOracle
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Program _ -> "Target program for test case generation with fuzzing."
      | Kernel _ -> "Target PTX for test case generation with GPU fuzzing."
      | Verbose _ -> "Verbosity level to control debug messages."
      | GPU _ -> "The GPU device ID to bind the fuzzer."
      | Timelimit _ -> "Timeout for fuzz testing (in seconds)."
      | OutputDir _ -> "Directory to store testcase outputs."
      | ABIFile _ -> "ABI JSON file."
      | NoSDFA -> "Disable static data-flow analysis to guide fuzzing."
      | NoDDFA -> "Disable dynamic data-flow analysis during the fuzzing."
      | CheckOptionalBugs ->
        "Detect optional bugs (e.g. requirement violation) disabled by default."
      | UseOthersOracle ->
        "Report bugs using other tools' oracles as well.\n\
        Currently we support (BD/IB/ME/RE) X (sFuzz/ILF/Mythril/MANTICORE)."

type FuzzOption = {
  Verbosity         : int
  GPU               : int
  OutDir            : string
  Timelimit         : int
  ProgPath          : string
  ABIPath           : string
  StaticDFA         : bool
  DynamicDFA        : bool
  CheckOptionalBugs : bool
  UseOthersOracle   : bool
  KernelPath        : string
}

let parseFuzzOption (args: string array) =
  let cmdPrefix = "dotnet Smartian.dll fuzz"
  let parser = ArgumentParser.Create<FuzzerCLI> (programName = cmdPrefix)
  let r = try parser.Parse(args) with
          :? Argu.ArguParseException -> printLine (parser.PrintUsage()); exit 1
  { Verbosity = r.GetResult (<@ Verbose @>, defaultValue = 1)
    GPU = r.GetResult (<@ GPU @>, defaultValue = 0)
    OutDir = r.GetResult (<@ OutputDir @>)
    Timelimit = r.GetResult (<@ Timelimit @>)
    ProgPath = r.GetResult (<@ Program @>)
    KernelPath = r.GetResult (<@ Kernel @>, defaultValue = "")
    ABIPath = r.GetResult(<@ ABIFile @>, defaultValue = "")
    StaticDFA = not (r.Contains(<@ NoSDFA @>))  // Enabled by default.
    DynamicDFA = not (r.Contains(<@ NoDDFA @>)) // Enabled by default.
    CheckOptionalBugs = r.Contains(<@ CheckOptionalBugs @>)
    UseOthersOracle = r.Contains(<@ UseOthersOracle @>) }
