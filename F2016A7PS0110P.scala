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

    def drop[A](l: List[A], n: Int): List[A] = {
        if(n==0) l
        else drop(l.tail, n-1)
    }

    // def getFirstN(l: List[Double], n: Int): List[Double] = n match{
    //     case 0 => Nil
    //     case _ => l.head :: getFirstN(l.tail, n-1)
    // }

    def getFirstN[A](l: List[A], n: Int): List[A] = {
        if(l.isEmpty) l
        else if (n == 0) Nil
        else l.head :: getFirstN(l.tail, n-1)
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

    def slicer( Image : List[List[Double]], K : Int, xcoord : Int) : List[Double] = {
        Image match {
            case row::rest => getFirstN( drop(row, xcoord) , K) ::: slicer( Image.tail, K, xcoord)
            case Nil => Nil
        }
    }
    
    def singlePoolingHelper( poolingFunc : List[Double] => Double, Image : List[List[Double]], K : Int, answer : List[Double], xcoord : Int) : List[Double] = {
        if( slicer(Image, K, xcoord).isEmpty ) answer
        else singlePoolingHelper(poolingFunc, Image, K, List.concat(answer, List(poolingFunc(slicer(Image, K, xcoord)))), xcoord + K)
    }
    
    def singlePooling( poolingFunc : List[Double] => Double, Image : List[List[Double]], K : Int) : List[Double] = {
        singlePoolingHelper(poolingFunc, Image, K, List(), 0)
    }

    def poolingLayerHelper( poolingFunc : List[Double] => Double, Image : List[List[Double]], K : Int , answer: List[List[Double]], ycoord : Int, originalSize : Int) : List[List[Double]] = {
        if(ycoord == originalSize) answer
        else poolingLayerHelper( poolingFunc, drop(Image, K), K, List.concat(answer, List(singlePooling(poolingFunc, getFirstN(Image, K), K))), ycoord + K, originalSize)
    }
    
    def getSize[A](l : List[List[A]]) : Int = l match{
        case first::rest => 1+getSize(l.tail)
        case Nil => 0
    }

    def poolingLayer( poolingFunc : List[Double] => Double, Image : List[List[Double]], K : Int) : List[List[Double]] = {
        poolingLayerHelper( poolingFunc, Image, K, List(), 0, getSize(Image))
    }

    def flatten(Image : List[List[Double]]) : List[Double] = {
        if(Image.isEmpty) Nil
        else Image.head ::: flatten(Image.tail)
    }

    def getMaxInRow(l: List[Double]): Double = l match {
        case first :: rest => if(first > getMaxInRow(rest)) first
                              else getMaxInRow(rest)
        case Nil => -100000000  
    }

    def getMinInRow(l: List[Double]): Double = l match {
        case first :: rest => if(first < getMinInRow(rest)) first
                              else getMinInRow(rest)
        case Nil => 100000000  
    }    

    def getMax(l : List[List[Double]], answer : Double) : Double = l match {
        case row::rest => if(answer > getMaxInRow(row)) getMax( l.tail, answer)
                          else getMax(l.tail, getMaxInRow(row))
        case Nil => answer   
    }

    def getMin(l : List[List[Double]], answer : Double) : Double = l match {
        case row::rest => if(answer < getMinInRow(row)) getMin( l.tail, answer)
                          else getMin(l.tail, getMinInRow(row))
        case Nil => answer   
    }

    def computeNormalisedRow(row : List[Double], maxi : Double, mini : Double, normalisedRow : List[Int]) : List[Int] = {
        if(row.isEmpty) normalisedRow
        else computeNormalisedRow( row.tail, maxi, mini, List.concat(normalisedRow, List( Math.round( (255.0 * (row.head - mini) / (maxi - mini))).toInt)))
    }

    def normaliseHelper(Image : List[List[Double]], answer : List[List[Int]], maxi : Double, mini : Double) : List[List[Int]] = {
        if ( Image.isEmpty ) answer
        else normaliseHelper(Image.tail, List.concat(answer, List(computeNormalisedRow(Image.head, maxi, mini, List()))), maxi, mini)
    }

    def normalise( Image : List[List[Double]] ) : List[List[Int]] = {
        normaliseHelper(Image, List(), getMax(Image, -100000000.0), getMin(Image, 100000000.0))
    }    

    def mixedLayer( Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int], activationFunc:Double => Double, poolingFunc:List[Double]=>Double, K:Int ) : List[List[Double]] = {
        poolingLayer(poolingFunc, activationLayer( activationFunc, convolute( Image, Kernel, imageSize, kernelSize)), K)
    }
    
    def assembly( Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,Kernel1:List[List[Double]],kernelSize1:List[Int],Kernel2:List[List[Double]],kernelSize2:List[Int],Kernel3:List[List[Double]],kernelSize3:List[Int],Size: Int) : List[List[Int]] = {

    }

}