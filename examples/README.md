1. Compile Hex to PTX

`~/build/sema/src/standalone-ptxsema bc/IB.bin -o ./bytecode.ll --hex --fsanitize=intsan --dump && llvm-as ./bytecode.ll -o main.bc && llvm-link main.bc ~/build/rt.o.bc --only-needed -o kernel.bc && llc -mcpu=sm_86 ./kernel.bc -o ./ptx/IB.ptx`

2. Fuzz
`dotnet ../build/Smartian.dll fuzz -t 30 -p bc/IB.bin -a abi/IB.abi  -k ptx/IB.ptx -v 0 -o output/IB`

3. Replay

`dotnet ../build/Smartian.dll replay -p bc/IB.bin -i output/IB/bug/`