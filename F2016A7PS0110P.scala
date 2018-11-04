package pplAssignment

object F2016A7PS0110P {
     
    def computeRow(row_1 : List[Double], row_2 : List[Double], acc: Double) : Double =  {
        if(row_1.isEmpty) acc
        else computeRow(row_1.tail, row_2.tail, acc + row_1.head * row_2.head)
    }

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

    def getFirstN[A](l: List[A], n: Int): List[A] = {
        if(l.isEmpty) l
        else if (n == 0) Nil
        else l.head :: getFirstN(l.tail, n-1)
    }

    def getLastElement(l : List[Int]) : Int = l match {
        case i :: Nil => i
        case _ => getLastElement(l.tail)
    }

    def slice_Image( Image : List[List[Double]], kernelSize : List[Int], xcoord : Int, rowsTaken : Int) : List[List[Double]] = {
        if(rowsTaken == kernelSize.head) Nil
        else getFirstN( drop(Image.head, xcoord) , getLastElement(kernelSize)) :: slice_Image( Image.tail, kernelSize, xcoord, rowsTaken + 1)
    }

    def convoluteRow( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int], xcoord : Int) : List[Double] = {
        if(xcoord == getLastElement(imageSize) - getLastElement(kernelSize) + 1) Nil
        else dotProduct(Kernel, slice_Image(Image, kernelSize, xcoord, 0)) :: convoluteRow(Image, Kernel, imageSize, kernelSize, xcoord + 1)
    }

    def convoluteHelper( Image : List[List[Double]], Kernel : List[List[Double]], imageSize : List[Int], kernelSize : List[Int], convolutedImage : List[List[Double]], ycoord : Int) : List[List[Double]] = {
        if(ycoord == imageSize.head - kernelSize.head + 1) convolutedImage
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
    
    def addBiasToRow(b : Double, row : List[Double], biasedRow : List[Double]) : List[Double] = {
        if ( row.isEmpty ) biasedRow
        else addBiasToRow( b, row.tail, List.concat(biasedRow, List( b + row.head)))
    }

    def addBiasToMatrix(b : Double, matrix : List[List[Double]], answer : List[List[Double]]) : List[List[Double]] = {
        if ( matrix.isEmpty ) answer
        else addBiasToMatrix(b, matrix.tail, List.concat(answer, List(addBiasToRow(b, matrix.head, List() ))))
    }

    def scalarMultiplyRow(w : Double, row : List[Double], multipliedRow : List[Double]) : List[Double] = {
        if ( row.isEmpty ) multipliedRow
        else scalarMultiplyRow(w, row.tail, List.concat(multipliedRow, List( w * row.head)))
    }

    def scalarMultiply(w : Double, matrix : List[List[Double]], answer : List[List[Double]]) : List[List[Double]] = {
        if ( matrix.isEmpty ) answer
        else scalarMultiply(w, matrix.tail, List.concat(answer, List(scalarMultiplyRow(w, matrix.head, List() ))))
    }

    def addRows(row1 : List[Double], row2 : List[Double], addedRow : List[Double]) : List[Double] = {
        if ( row1.isEmpty ) addedRow
        else addRows( row1.tail, row2.tail, List.concat(addedRow, List( row1.head + row2.head)))
    }

    def addMatrices(matrix1 : List[List[Double]], matrix2 : List[List[Double]], answer : List[List[Double]]) : List[List[Double]] = {
        if ( matrix1.isEmpty ) answer
        else addMatrices(matrix1.tail, matrix2.tail, List.concat(answer, List(addRows(matrix1.head, matrix2.head, List() ))))
    }

    def poolingFunctionAverage(l : List[Double]) : Double = l match {
        case Nil => 0
        case x::tail => (x + poolingFunctionAverage(tail) * tail.length) / (l.length)
    }

    def assembly( Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]], kernelSize1:List[Int], Kernel2:List[List[Double]], kernelSize2:List[Int], Kernel3:List[List[Double]], kernelSize3:List[Int], Size: Int) : List[List[Int]] = {
        
        val intermediate1 = mixedLayer(Image, Kernel1, imageSize, kernelSize1, (x:Double) => if(x>0) x else 0, poolingFunctionAverage, Size )
        val intermediate2 = mixedLayer(Image, Kernel2, imageSize, kernelSize2, (x:Double) => if(x>0) x else 0, poolingFunctionAverage, Size )
        
        val scalarMultiplied1 = scalarMultiply(w1, intermediate1, List() )
        val scalarMultiplied2 = scalarMultiply(w2, intermediate2, List() )
        
        val intermediate3 = addBiasToMatrix(b, addMatrices( scalarMultiplied1, scalarMultiplied2, List() ), List() )
        
        normalise( mixedLayer( intermediate3, Kernel3, List(intermediate1.length,intermediate1.head.length), kernelSize3, (x:Double) => if(x>0) x else 0.5 * x, getMaxInRow, Size ) )
    }

}