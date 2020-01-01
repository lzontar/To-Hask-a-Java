# To-Hask-a-Java
## :bulb: What's the idea?
In few words, our main goal is to develop a simple compiler that will take as an input a program written in Java and output the same program written in Haskell programming language. Due to its simplicity to process tree structures, processing will be done in Haskell.

## What was the motivation for this project?
I started learning Haskell earlier this year and this is my first real project in Haskell. I though it would be interesting transforming object-oriented code to code based on functional programming.

## Libraries and toolset
#### Haskell
As mentioned above, compiler is based on [Haskell](https://www.haskell.org/) programming language.

**Haskell** is a general purpose purely functional programming language. It is based on lambda calculus and due to its distinctiveness to other languages it allows you to develop clearer code with fewer errors and higher reliability.

## Project structure
- **Main.hs** contains the main Haskell function which processes program's IO and connects other modules in a single file.

## Using To-Hask-a-Java compiler
1. We have to run the Glasgow Haskell Compiler: ``` $ ghci ```
2. Now we have to tell *GHC* that it should look for modules in the same directory as well, because we have splitted our program into 4 Haskell code files. We do that by executing:
```
Prelude> :set -isrc
```
3. Time to load our modules (all modules are included in Main.hs):
```
Prelude> :l ./src/Main.hs
```
4. Once our modules are loaded we can execute the compiler (<FILENAME> is obviously replaced with an actual name of the file that we want to compile):
```
*Main> thaj "<FILENAME>.java"
```
5. We can now compile and execute our newly generated Haskell version of Java code from file *<FILENAME>.hs*. 
## :sos: Wish to contribute?
### Environment setup
1. Fork repository and pull the content
   **Make changes**
2. Pull the latest version of repository
3. Make changes:
   * Test code manually
4. After testing, commit to your forked repository
5. Create a Pull Request to branch **master**

   **:link: Check out my other projects**

   Check out my other projects at my GitHub Pages website [lzontar.github.io](https://lzontar.github.io):star:.
