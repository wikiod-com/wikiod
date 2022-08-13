---
title: "Bytecode Modification"
slug: "bytecode-modification"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## What is Bytecode?
Bytecode is the set of instructions used by the JVM. To illustrate this let's take this Hello World program. 

    public static void main(String[] args){
        System.out.println("Hello World");
    }
This is what it turns into when compiled into bytecode.

    public static main([Ljava/lang/String; args)V    
        getstatic java/lang/System out Ljava/io/PrintStream;
        ldc "Hello World"
        invokevirtual java/io/PrintStream print(Ljava/lang/String;)V

What's the logic behind this?
=============================

**getstatic** - Retreives the value of a static field of a class. In this case, the *PrintStream "Out"* of *System*.

**ldc** - Push a constant onto the stack. In this case, the String "Hello World"

**invokevirtual** - Invokes a method on a loaded reference on the stack and puts the result on the stack. Parameters of the method are also taken from the stack.

Well, there has to be more right?
=================================

There are 255 opcodes, but not all of them are implemented yet. A table with all of the current opcodes can be found here: [Java bytecode instruction listings][1]. 

How can I write / edit bytecode?
================================

There's multiple ways to write and edit bytecode. You can use a compiler, use a library, or use a program.

For writing:

 - [Jasmin][2]
 - [Krakatau][3]

For editing: 

 - Libraries
   - [ASM][4]
   - [Javassist][5]
   - [BCEL][6] - *Doesn't support Java 8+*
 - Tools
   - [Bytecode-Viewer][7]
   - [JBytedit][8]
   - [reJ][9] - *Doesn't support Java 8+*
   - [JBE][10] - *Doesn't support Java 8+*

I'd like to learn more about bytecode!
======================================

There's probably a specific documentation page specificially for bytecode. This page focuses on the modification of bytecode using different libraries and tools.


  [1]: https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
  [2]: http://jasmin.sourceforge.net/
  [3]: https://github.com/Storyyeller/Krakatau
  [4]: http://asm.ow2.org/
  [5]: http://jboss-javassist.github.io/javassist/
  [6]: https://commons.apache.org/proper/commons-bcel/
  [7]: https://github.com/Konloch/bytecode-viewer
  [8]: https://github.com/QMatt/JBytedit
  [9]: http://rejava.sourceforge.net/features.html
  [10]: http://www.cs.ioc.ee/~ando/jbe/

## How to edit jar files with ASM
Firstly the classes from the jar need to be loaded. We'll use three methods for this process:

 - loadClasses(File)
 - readJar(JarFile, JarEntry, Map)
 - getNode(byte[])

        
    Map<String, ClassNode> loadClasses(File jarFile) throws IOException {
        Map<String, ClassNode> classes = new HashMap<String, ClassNode>();
        JarFile jar = new JarFile(jarFile);
        Stream<JarEntry> str = jar.stream();
        str.forEach(z -> readJar(jar, z, classes));
        jar.close();
        return classes;
    }

    Map<String, ClassNode> readJar(JarFile jar, JarEntry entry, Map<String, ClassNode> classes) {
        String name = entry.getName();
        try (InputStream jis = jar.getInputStream(entry)){
            if (name.endsWith(".class")) {
                byte[] bytes = IOUtils.toByteArray(jis);
                String cafebabe = String.format("%02X%02X%02X%02X", bytes[0], bytes[1], bytes[2], bytes[3]);
                if (!cafebabe.toLowerCase().equals("cafebabe")) {
                    // This class doesn't have a valid magic
                    return classes;
                }
                try {
                    ClassNode cn = getNode(bytes);
                    classes.put(cn.name, cn);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return classes;
    }
    
    ClassNode getNode(byte[] bytes) {
        ClassReader cr = new ClassReader(bytes);
        ClassNode cn = new ClassNode();
        try {
            cr.accept(cn, ClassReader.EXPAND_FRAMES);
        } catch (Exception e) {
            e.printStackTrace();
        }
        cr = null;
        return cn;
    }

With these methods loading and changing a jar file becomes a simple matter of changing ClassNodes in a map. In this example we will replace all Strings in the jar with capitalized ones using the Tree API.
    
    File jarFile = new File("sample.jar");
    Map<String, ClassNode> nodes = loadClasses(jarFile);
    // Iterate ClassNodes
    for (ClassNode cn : nodes.values()){
        // Iterate methods in class
        for (MethodNode mn : cn.methods){
            // Iterate instructions in method
            for (AbstractInsnNode ain : mn.instructions.toArray()){
                // If the instruction is loading a constant value 
                if (ain.getOpcode() == Opcodes.LDC){
                    // Cast current instruction to Ldc
                    // If the constant is a string then capitalize it.
                    LdcInsnNode ldc = (LdcInsnNode) ain;
                    if (ldc.cst instanceof String){
                        ldc.cst = ldc.cst.toString().toUpperCase();
                    }
                }
            }
        }
    }

Now that all of the ClassNode's strings have been modified we need to save the changes. In order to save the changes and have a working output a few things have to be done:

 - Export ClassNodes to bytes
 - Load non-class jar entries *(Ex: Manifest.mf / other binary resources in jar)* as bytes
 - Save all bytes to a new jar

From the last portion above, we'll create three methods. 

 - processNodes(Map<String, ClassNode> nodes)
 - loadNonClasses(File jarFile)
 - saveAsJar(Map<String, byte[]> outBytes, String fileName)

Usage:

    Map<String, byte[]> out = process(nodes, new HashMap<String, MappedClass>());
    out.putAll(loadNonClassEntries(jarFile));
    saveAsJar(out, "sample-edit.jar");

The methods used:

    static Map<String, byte[]> processNodes(Map<String, ClassNode> nodes, Map<String, MappedClass> mappings) {
        Map<String, byte[]> out = new HashMap<String, byte[]>();
        // Iterate nodes and add them to the map of <Class names , Class bytes>
        // Using Compute_Frames ensures that stack-frames will be re-calculated automatically
        for (ClassNode cn : nodes.values()) {
            ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
            out.put(mappings.containsKey(cn.name) ? mappings.get(cn.name).getNewName() : cn.name, cw.toByteArray());
        }
        return out;
    }
    
    static Map<String, byte[]> loadNonClasses(File jarFile) throws IOException {
        Map<String, byte[]> entries = new HashMap<String, byte[]>();
        ZipInputStream jis = new ZipInputStream(new FileInputStream(jarFile));
        ZipEntry entry;
        // Iterate all entries
        while ((entry = jis.getNextEntry()) != null) {
            try {
                String name = entry.getName();
                if (!name.endsWith(".class") && !entry.isDirectory()) {
                    // Apache Commons - byte[] toByteArray(InputStream input)
                    //
                    // Add each entry to the map <Entry name , Entry bytes>
                    byte[] bytes = IOUtils.toByteArray(jis);
                    entries.put(name, bytes);
                }
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                jis.closeEntry();
            }
        }
        jis.close();
        return entries;
    }
    
    static void saveAsJar(Map<String, byte[]> outBytes, String fileName) {
        try {
            // Create jar output stream
            JarOutputStream out = new JarOutputStream(new FileOutputStream(fileName));
            // For each entry in the map, save the bytes
            for (String entry : outBytes.keySet()) {
                // Appent class names to class entries
                String ext = entry.contains(".") ? "" : ".class";
                out.putNextEntry(new ZipEntry(entry + ext));
                out.write(outBytes.get(entry));
                out.closeEntry();
            }
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

That's it. All the changes will be saved to "sample-edit.jar".


## How to load a ClassNode as a Class
    /**
     * Load a class by from a ClassNode
     * 
     * @param cn
     *            ClassNode to load
     * @return
     */
    public static Class<?> load(ClassNode cn) {
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        return new ClassDefiner(ClassLoader.getSystemClassLoader()).get(cn.name.replace("/", "."), cw.toByteArray());
    }

    /**
     * Classloader that loads a class from bytes.
     */
    static class ClassDefiner extends ClassLoader {
        public ClassDefiner(ClassLoader parent) {
            super(parent);
        }

        public Class<?> get(String name, byte[] bytes) {
            Class<?> c = defineClass(name, bytes, 0, bytes.length);
            resolveClass(c);
            return c;
        }
    }

## How to rename classes in a jar file
    public static void main(String[] args) throws Exception {
        File jarFile = new File("Input.jar");
        Map<String, ClassNode> nodes = JarUtils.loadClasses(jarFile);
        
        Map<String, byte[]> out = JarUtils.loadNonClassEntries(jarFile);
        Map<String, String> mappings = new HashMap<String, String>();
        mappings.put("me/example/ExampleClass", "me/example/ExampleRenamed");
        out.putAll(process(nodes, mappings));
        JarUtils.saveAsJar(out, "Input-new.jar");
    }

    static Map<String, byte[]> process(Map<String, ClassNode> nodes, Map<String, String> mappings) {
        Map<String, byte[]> out = new HashMap<String, byte[]>();
        Remapper mapper = new SimpleRemapper(mappings);
        for (ClassNode cn : nodes.values()) {
            ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
            ClassVisitor remapper = new ClassRemapper(cw, mapper);
            cn.accept(remapper);
            out.put(mappings.containsKey(cn.name) ? mappings.get(cn.name) : cn.name, cw.toByteArray());
        }
        return out;
    }

SimpleRemapper is an existing class in the ASM library. However it only allows for class names to be changed. If you wish to rename fields and methods you should create your own implemenation of the Remapper class.

## Javassist Basic


