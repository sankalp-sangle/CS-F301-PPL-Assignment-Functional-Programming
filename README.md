# CS F301 Principles of Programming Languages Assignment on Functional Programming

This is Scala code written for the assignment as described in Assignment_Description.pdf as part of my Junior year coursework at BITS.

The assignment was meant to give us an introduction to Functional Programming in Scala. We had to simulate the working of a Convolutional Neural Network using the functional programming paradigm.

Despite it's relatively simple description, this is probably the most enjoyable assignment I have done at BITS. Functional Programming was tedious to understand but an absolute delight eventually.

To install scala on Ubuntu 16.04, run the command:
```sh
sudo apt-get install scala
```

To compile the code (i.e `F2016A7PS0110P.scala` and `driver.scala`), run the command:
```sh
scalac F2016A7PS0110P.scala driver.scala
```

The subfolder `test_cases` contains three test cases: `t0`, `t1` and `t2`. To run your code on test-case `t0` (let's say), run the command:
```sh
scala pplAssignment.Driver test_cases/t0.in > t0.check
```

 To compare the program output with the given output, run the command:
```sh
diff test_cases/t0.out t0.check
