---
title: "Java Agents"
slug: "java-agents"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Modifying classes with agents
Firstly, make sure that the agent being used has the following attributes in the Manifest.mf:

    Can-Redefine-Classes: true
    Can-Retransform-Classes: true
Starting a java agent will let the agent access the class Instrumentation. With Instrumentation you can call *addTransformer(ClassFileTransformer transformer)*. ClassFileTransformers will let you rewrite the bytes of classes. The class has only a single method which supplies the ClassLoader that loads the class, the class's name, a java.lang.Class instance of it, it's ProtectionDomain, and lastly the bytes of the class itself. 

It looks like this:

    byte[] transform(ClassLoader loader, String className, Class<?> classBeingRedefined, 
              ProtectionDomain protectionDomain, byte[] classfileBuffer)

Modifying a class purely from bytes can take ages. To remedy this there are libraries that can be used to convert the class bytes into something more usable. 

In this example I'll be using ASM, but  other alternatives like Javassist and BCEL have similar features. 

    ClassNode getNode(byte[] bytes) {
        // Create a ClassReader that will parse the byte array into a ClassNode
        ClassReader cr = new ClassReader(bytes);
        ClassNode cn = new ClassNode();
        try {
            // This populates the ClassNode
            cr.accept(cn, ClassReader.EXPAND_FRAMES);
            cr = null;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return cn;
    }

From here changes can be made to the ClassNode object. This makes changing field/method access incredibly easy. Plus with ASM's Tree API modifying the bytecode of methods is a breeze. 

Once the edits are finished you can convert the ClassNode back into bytes with the following method and return them in the *transform* method:

    public static byte[] getNodeBytes(ClassNode cn, boolean useMaxs) {
        ClassWriter cw = new ClassWriter(useMaxs ? ClassWriter.COMPUTE_MAXS : ClassWriter.COMPUTE_FRAMES);
        cn.accept(cw);
        byte[] b = cw.toByteArray();
        return b;
    }

## Adding an agent at runtime
Agents can be added to a JVM at runtime. To load an agent you will need to use the Attach API's *VirtualMachine.attatch(String id)*. You can then load a compiled agent jar with the following method:

    public static void loadAgent(String agentPath) {
        String vmName = ManagementFactory.getRuntimeMXBean().getName();
        int index = vmName.indexOf('@');
        String pid = vmName.substring(0, index);
        try {
            File agentFile = new File(agentPath);
            VirtualMachine vm = VirtualMachine.attach(pid);
            vm.loadAgent(agentFile.getAbsolutePath(), "");
            VirtualMachine.attach(vm.id());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
This will not call *premain((String agentArgs, Instrumentation inst)* in the loaded agent, but instead will call *agentmain(String agentArgs, Instrumentation inst)*. This requires *Agent-Class* to be set in the agent Manifest.mf.

## Setting up a basic agent
The Premain class will contain the method *"premain(String agentArgs Instrumentation inst)"*

Here is an example:

    import java.lang.instrument.Instrumentation;

    public class PremainExample {
        public static void premain(String agentArgs, Instrumentation inst) {
            System.out.println(agentArgs);
        }
    }

When compiled into a jar file open the Manifest and ensure that it has the Premain-Class attribute. 

Here is an example:

    Premain-Class: PremainExample

To use the agent with another java program "myProgram" you must define the agent in the JVM arguments: 

    java -javaagent:PremainAgent.jar -jar myProgram.jar

