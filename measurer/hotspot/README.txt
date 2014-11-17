1. Running java programs:
/users/dmitchell/hotspot/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma -jar dacapo-9.12-bach.jar -s small avrora

2. Some important files:
a. share/vm/runtime/globals.hpp
b. cpu/x86/vm/templateInterpreter_x86_32.cpp
c. cpu/x86/vm/templateTable_x86_32.cpp
d. share/vm/interpreter/interpreterRuntime.cpp

3. Useful flags:
a. ProfileInterpreter, CountBytecodes, PrintBytecodeHistogram

/users/dmitchell/hotspot/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma  -XX:+PrintCompilation -XX:CompileThreshold=1000 -jar dacapo-9.12-bach.jar -s small avrora

ddd /users/dmitchell/hotspot/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma &

---------------------------------------------------------
Building Hostpot
---------------------------------------------------------
make jvmg1

----------------------------------------------------------
Running Hotspot (gamma)
----------------------------------------------------------
/projects/zephyr/deadzone/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma -jar /projects/zephyr/deadzone/benchmarks/dacapo-9.12-bach.jar -s large avrora

/projects/zephyr/deadzone/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma -XX:+PrintCompilation -jar /projects/zephyr/deadzone/benchmarks/dacapo-9.12-bach.jar -s small avrora

---------------------------------------------------------
Monitor Driver
----------------------------------------------------------
/tools/cluster/6.2/java/jdk1.6.0_30/bin/java -classpath /nfs/projects/zephyr/deadzone/workspace_AS/MonitorDriver/bin MonitorDriver


----------------------------------------------------------
Debugging Hostpost (gamma)
----------------------------------------------------------
gdb /projects/zephyr/deadzone/hotspot/build/linux/linux_i486_compiler1/jvmg/gamma

b /projects/zephyr/deadzone/hotspot/src/share/vm/compiler/compileBroker.cpp:1439

b /projects/zephyr/deadzone/hotspot/src/share/vm/runtime/thread.cpp:3353

b /projects/zephyr/deadzone/hotspot/src/share/vm/compiler/compileBroker.cpp:893

run -jar /projects/zephyr/deadzone/benchmarks/dacapo-9.12-bach.jar -s small avrora
