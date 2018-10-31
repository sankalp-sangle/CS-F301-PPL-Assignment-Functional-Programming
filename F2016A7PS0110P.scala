package pplAssignment

object F2016A7PS0110P {
    //Start Coding from here

    def computeRow(row_1 : List[Double], row_2 : List[Double], acc: Double) : Double = row_1 match {
        case i::rest => computeRow(row_1.tail, row_2.tail, acc + row_1.head * row_2.head)
        case Nil => acc
    } 
    
    // def dotProductHelper( matrix_1 : List[List[Double]], matrix_2 : List[List[Double]], acc : Double ) : Double = matrix_1 match {
    //     case row::rest => dotProductHelper( matrix_1.tail, matrix_2.tail, acc + computeRow(matrix_1.head, matrix_2.head, 0) )
    //     case Nil => acc
    // }

    def dotProductHelper( matrix_1 : List[List[Double]], matrix_2 : List[List[Double]], acc : Double ) : Double = {
        if ( matrix_1.isEmpty ) acc
        else dotProductHelper(matrix_1.tail, matrix_2.tail, acc + computeRow(matrix_1.head, matrix_2.head, 0))
    }                                               

    def dotProduct( matrix_1 : List[List[Double]], matrix_2 : List[List[Double]] ) : Double = {
        dotProductHelper(matrix_1, matrix_2, 0)
    }

    def drop(l: List[Double], n: Int): List[Double] = {
        if(n==0) l
        else drop(l.tail, n-1)
    }

    def getFirstN(l: List[Double], n: Int): List[Double] = n match {
        case 0 => Nil
        case _ => l.head :: getFirstN(l.tail, n-1)
    }

    def getLastElement(l : List[Int]) : Int = l match {
        case i :: Nil => i
        case _ => getLastElement(l.tail)
    }

    def slice_Image( Image : List[List[Double]], kernelSize : List[Int], xcoord : Int) : List[List[Double]] = {
        Image match {
            case row::rest => getFirstN( drop(row, xcoord) , kernelSize.head) :: slice_Image( Image.tail, kernelSize, xcoord)
            case Nil => Nil
        }
    }

    def convoluteRow( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int], xcoord : Int) : List[Double] = {
        if(xcoord == getLastElement(imageSize) - getLastElement(kernelSize) + 1) Nil
        else dotProduct( Kernel, slice_Image(Image, kernelSize, xcoord)) :: convoluteRow(Image, Kernel, imageSize, kernelSize, xcoord + 1)
    }

    // def convoluteHelper( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int], convolutedImage : List[List[Double]]) : List[List[Double]] = {
    //     Image match {
    //         case row::rest => convoluteHelper(Image.tail, Kernel, imageSize, kernelSize, convoluteRow(Image, Kernel, imageSize, kernelSize, 0)::convolutedImage)
    //         case Nil => convolutedImage
    //     }
    // }

    def convoluteHelper( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int], convolutedImage : List[List[Double]], ycoord : Int) : List[List[Double]] = {
        if(ycoord == getLastElement(imageSize) - getLastElement(kernelSize) + 1) convolutedImage
        else convoluteHelper(Image.tail, Kernel, imageSize, kernelSize, List.concat(convolutedImage, List(convoluteRow(Image, Kernel, imageSize, kernelSize, 0))), ycoord + 1)
    }

    def convolute( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int]) : List[List[Double]] = {
        convoluteHelper(Image, Kernel, imageSize, kernelSize, List(), 0)
    }

    def computeActivationRow(row : List[Double], activationFunc : Double => Double, activatedRow : List[Double] ) : List[Double] = {
        if(row.isEmpty) activatedRow
        else computeActivationRow( row.tail, activationFunc, List.concat(activatedRow, List(activationFunc(row.head))))
    }

    def activationHelper( activationFunc : Double => Double, Image : List[List[Double]], answer: List[List[Double]]) : List[List[Double]] = {
        if(Image.isEmpty) answer
        else activationHelper(activationFunc, Image.tail, List.concat(answer, List(computeActivationRow(Image.head, activationFunc, List()))))
    }

    def activationLayer( activationFunc : Double => Double, Image : List[List[Double]] ) : List[List[Double]] = {
        activationHelper( activationFunc, Image : List[List[Double]], List())
    }

}