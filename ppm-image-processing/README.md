# Negru Mihai 323CD: Tema1 PP

A mini project that reflects the importance and workflow of functional programming paradigm, written in Scala Language.

## **Table of contents**

1. [Getting started](#start)
   1. [Project Structure](#structure)
   2. [Files Hierarchy](#main)
2. [Solution Class](#solution)
   1. [Parsers](#parsers)
   2. [Vertical and Horizontal Concat](#concats)
   3. [Rotation](#rotate)
   4. [Sobel detector](#detector)
   5. [Pascal Triangle](#pascal)


<a name="start"></a>
## **Getting started**

This README covers the structure and some explanations about solving this project.


<a name="structure"></a>
### **Project Structure**

The project is divided in two parts:
* *input/output/correct* - PPM files provided in order to perform test on the project solution.
* *src* - implementation for the solution of the project.


<a name="main"></a>
### **Files Hierarchy**

The **main** implementation for the project can be found under the **Solution.scala** file.

Other important files are:
* *util/Util.scala* - a singleton containing util methods for solving the project.
* *util/Pixel.scala* - case class representing a Pixel with rgb codification.
* *Solution.scala* - base implementation of the project API.
* *test/scala/Tests.scala* - unit tests for the project.

In the following sections we will discuss more about the **Solution.scala** file and about <br>
functions that build the project API.


<a name="solution"></a>
## **Solution**

This File contains the base implementation of the project API.


<a name="parsers"></a>
### **Parsers**

In this section we will discuss parsing function for PPM files.

#### **fromStringPPM**

This function takes a `List[Char]` which represents the codification of the `PPM` file.

First we take the PPM codification and split it over *Space* and *Newline* characters, then we<br>
extract the *columns* and *rows* length of the image and the pixels' matrix.

In order to convert the raw pixels into *Pixel* case class, we will group the numbers into 3
```text
   Ex: '2', '5', '5' -> 
```


<a name="concats"></a>
### **Vertical and Horizontal Concat**



<a name="rotate"></a>
### **Rotation**



<a name="detector"></a>
### **Sobel detector**



<a name="pascal"></a>
### **Pascal Triangle**
