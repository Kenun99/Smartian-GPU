module Smartian.Runner

open System
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

open Config
open BytesUtils
open Options
open Utils

type CUcontext = IntPtr

type CUmodule = IntPtr

type CUfunction = IntPtr

[<Flags>]
type CUcontextFlags = 
    | CU_CTX_SCHED_AUTO = 0x00
    | CU_CTX_SCHED_SPIN = 0x01
    | CU_CTX_SCHED_YIELD = 0x02
    | CU_CTX_SCHED_BLOCKING_SYNC = 0x04
    | CU_CTX_BLOCKING_SYNC = 0x04
    | CU_CTX_SCHED_MASK  = 0x07
    | CU_CTX_MAP_HOST = 0x08
    | CU_CTX_LMEM_RESIZE_TO_MAX = 0x10
    | CU_CTX_FLAGS_MASK          = 0x1f

type CUdevice = int

type CUdeviceptr = UInt64

type CUresult = 
    | CUDA_SUCCESS = 0
    | CUDA_ERROR_INVALID_VALUE = 1
    | CUDA_ERROR_OUT_OF_MEMORY = 2
    | CUDA_ERROR_NOT_INITIALIZED = 3
    | CUDA_ERROR_DEINITIALIZED = 4
    | CUDA_ERROR_PROFILER_DISABLED = 5
    | CUDA_ERROR_PROFILER_NOT_INITIALIZED = 6
    | CUDA_ERROR_PROFILER_ALREADY_STARTED = 7
    | CUDA_ERROR_PROFILER_ALREADY_STOPPED = 8
    | CUDA_ERROR_NO_DEVICE = 100
    | CUDA_ERROR_INVALID_DEVICE = 101
    | CUDA_ERROR_INVALID_IMAGE = 200
    | CUDA_ERROR_INVALID_CONTEXT = 201
    | CUDA_ERROR_CONTEXT_ALREADY_CURRENT = 202
    | CUDA_ERROR_MAP_FAILED = 205
    | CUDA_ERROR_UNMAP_FAILED = 206
    | CUDA_ERROR_ARRAY_IS_MAPPED = 207
    | CUDA_ERROR_ALREADY_MAPPED = 208
    | CUDA_ERROR_NO_BINARY_FOR_GPU = 209
    | CUDA_ERROR_ALREADY_ACQUIRED = 210
    | CUDA_ERROR_NOT_MAPPED = 211
    | CUDA_ERROR_NOT_MAPPED_AS_ARRAY = 212
    | CUDA_ERROR_NOT_MAPPED_AS_POINTER = 213
    | CUDA_ERROR_ECC_UNCORRECTABLE = 214
    | CUDA_ERROR_UNSUPPORTED_LIMIT = 215
    | CUDA_ERROR_CONTEXT_ALREADY_IN_USE = 216
    | CUDA_ERROR_PEER_ACCESS_UNSUPPORTED = 217
    | CUDA_ERROR_INVALID_SOURCE = 300
    | CUDA_ERROR_FILE_NOT_FOUND = 301
    | CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND = 302
    | CUDA_ERROR_SHARED_OBJECT_INIT_FAILED = 303
    | CUDA_ERROR_OPERATING_SYSTEM = 304
    | CUDA_ERROR_INVALID_HANDLE = 400
    | CUDA_ERROR_NOT_FOUND = 500
    | CUDA_ERROR_NOT_READY = 600
    | CUDA_ERROR_LAUNCH_FAILED = 700
    | CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES = 701
    | CUDA_ERROR_LAUNCH_TIMEOUT = 702
    | CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING = 703
    | CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED = 704
    | CUDA_ERROR_PEER_ACCESS_NOT_ENABLED = 705
    | CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE = 708
    | CUDA_ERROR_CONTEXT_IS_DESTROYED = 709
    | CUDA_ERROR_ASSERT = 710
    | CUDA_ERROR_TOO_MANY_PEERS = 711
    | CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED = 712
    | CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED = 713
    | CUDA_ERROR_NOT_PERMITTED = 800
    | CUDA_ERROR_NOT_SUPPORTED = 801
    | CUDA_ERROR_UNKNOWN = 999

type Feedback = {
  CovGain : bool
  DUGain : bool
  // PC, Op, Oprnd1, Oprnd2.
  CmpList : (uint64 * string * bigint * bigint) list
  // Bug class, Bug PC, Triggerring TX index.
  //   BugSet : Set<(BugClass * int * int)>
}

/// GA algorithm
let [<Literal>] GaSoPath = "libga.so"

[<DllImport(GaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern void DarwinInit(UInt64 nrSeeds, UInt32 nrMutations)

[<DllImport(GaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern Int32 DarwinSelectOpt(UInt64 seed)

[<DllImport(GaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern Int32 DarwinNotifyFeedback(UInt64 seed, UInt32 numPaths)

let [<Literal>] DllName = "librunner.so"

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern void HelloWorld()

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern void InitCudaCtx(CUcontext& pctx, int Dev, CUmodule& cuModule, [<MarshalAs(UnmanagedType.LPStr)>]string fname)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern void DestroyCuda(CUcontext ctx, CUmodule cuModule) 

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool cuDataCpy(CUdeviceptr dev, UInt64 callvalue, byte[] buff, UInt32 size)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern void cuMallocAll(CUmodule cudaModule, CUdeviceptr& dSeedsPtr, CUdeviceptr& dSignalsPtr)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern void cuFreeAll(CUmodule cudaModule, CUdeviceptr dSeeds)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool setEVMEnv(CUmodule cudaModule, byte[] From, UInt64 Timestamp, UInt64 Blocknum)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool cuDeployTx(CUmodule cudaModule, UInt64 value, byte[] data, UInt32 size)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool cuRunTxsInGroup(CUmodule cudaModule, CUdeviceptr dSeeds, UInt64 value, byte[] data, UInt32 size)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern UInt64 cuRunTxs(CUmodule cudaModule, CUdeviceptr dSeeds, byte[] argTypeMap, Int32 mapSize)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool postGainDu(CUdeviceptr dSignals, [<MarshalAs(UnmanagedType.LPStr)>]string timeStr) 

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool postGainCov(CUmodule cudaModule)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern UInt64 obtainCov(CUmodule cudaModule)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool gainCov(UInt32 tid)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern bool gainBug(UInt32 tid)

[<DllImport(DllName, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuMemcpyHD(CUdeviceptr& dstDev, byte[] srcHost, UInt32 size)

/// CUDA
let [<Literal>] CudaSoPath = "/usr/lib/x86_64-linux-gnu/libcuda.so"

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuCtxCreate(CUcontext& pctx, [<MarshalAs(UnmanagedType.U4)>]CUcontextFlags flags, CUdevice dev)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuInit(UInt32 Flags)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuModuleLoad(CUmodule& cuModule, [<MarshalAs(UnmanagedType.LPStr)>]string fname)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuModuleGetFunction(CUfunction& hfunc, CUmodule hmod, [<MarshalAs(UnmanagedType.LPStr)>]string name)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuGetErrorString(CUresult error, IntPtr& pStr)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuDeviceGetCount(int& count)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuDriverGetVersion(int& driverVersion)

// [<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
// extern CUresult cuMemAlloc(CUdeviceptr& dptr, UInt32 bytesize)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuMemFree(CUdeviceptr dptr)

// [<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
// extern CUresult cuMemcpyHtoD (CUdeviceptr dstDevice, byte[] srcHost, UInt32 ByteCount)

[<DllImport(CudaSoPath, CallingConvention=CallingConvention.Cdecl)>]
extern CUresult cuDeviceGet(CUdevice& device, int ordinal)

type CudaException(cuFunc, code: CUresult) =     
    inherit Exception()
    member x.Code = code
    override x.Message = 
        let mutable msgPtr = IntPtr.Zero
        cuGetErrorString(CUresult.CUDA_ERROR_ASSERT, &msgPtr) |> ignore        
        Marshal.PtrToStringAnsi(msgPtr)
    member x.FunctionName = cuFunc
    override x.ToString() = 
        sprintf "%s %d [%s] %A" x.Message (int x.Code) x.FunctionName x.StackTrace


let mutable  cuModule = CUmodule.Zero
let mutable  cuCtx = CUcontext.Zero    
let mutable  dSeed = CUdeviceptr.MinValue
let mutable  dSignals = CUdeviceptr.MinValue

let initialize gpu kernel =
  log "Initiate CUDA at : %d for %s" gpu kernel 
  InitCudaCtx(&cuCtx, gpu, &cuModule, kernel)
  cuMallocAll(cuModule, &dSeed, &dSignals)
  // DarwinInit(0, 4)

let destroy () = 
  cuFreeAll(cuModule, dSeed)
  DestroyCuda(cuCtx, cuModule)

let private runEVM opt seed covFlag =
//   incrExecutionCount ()
  let tc = Seed.concretize seed
  log "testing"
//   execute tc covFlag opt.DynamicDFA opt.CheckOptionalBugs opt.UseOthersOracle

let getCoverage opt seed =
//   let f = runEVM opt seed true
//   f.CovGain, f.DUGain, f.BugSet
    log "testing"