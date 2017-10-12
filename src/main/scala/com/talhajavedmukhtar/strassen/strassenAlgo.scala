package com.talhajavedmukhtar.strassen

import scala.util.Random

object strassenAlgo extends App{
  type Matrix = List[List[Int]]

  def getRandomMatrix(size: Int, maxElement: Int = 10): Matrix
    = List.tabulate(size)(n => List.fill(size)(Random.nextInt(maxElement)))

  def getRow(matrix: Matrix, rowNo: Int, size: Int, pos: Int): List[Int] = {  //get (sub)row of rowNo of (size) starting at (pos)
    List.tabulate(size)(n => matrix(rowNo)(pos + n))
  }

  def getCol(matrix: Matrix, colNo: Int, size: Int, pos: Int): List[Int] = {  //get (sub)column of (size) starting at (pos)
    List.tabulate(size)(n => getRow(matrix,n,size,0)(pos + colNo))
  }

  def getSubMatrix(origMatrix: Matrix, size: Int, startRow: Int, startCol: Int, rowsCovered: Int = 0): Matrix = {  //topleft is (startRow,startCol)
    if (rowsCovered == size - 1)
      List(getRow(origMatrix,startRow,size,startCol))
    else{
      val currentRow = List(getRow(origMatrix,startRow,size,startCol))
      currentRow ::: getSubMatrix(origMatrix,size,startRow+1,startCol,rowsCovered+1)
    }

  }

  def splitInto4(origMatrix: Matrix, origSize: Int): (Matrix,Matrix,Matrix,Matrix) = {
    val newSize = origSize/2

    val matrix00 = getSubMatrix(origMatrix,newSize,0,0)
    val matrix01 = getSubMatrix(origMatrix,newSize,0,newSize)
    val matrix10 = getSubMatrix(origMatrix,newSize,newSize,0)
    val matrix11 = getSubMatrix(origMatrix,newSize,newSize,newSize)

    (matrix00,matrix01,matrix10,matrix11)
  }

  def combineCols(a: Matrix, b: Matrix, size: Int, rowNo: Int = 0): Matrix = {
    if (rowNo == size - 1)
      List(getRow(a,rowNo,size,0) ::: getRow(b,rowNo,size,0))
    else{
      val currentRow = List(getRow(a,rowNo,size,0) ::: getRow(b,rowNo,size,0))
      currentRow ::: combineCols(a,b,size,rowNo + 1)
    }
  }

  def combineRows(a: Matrix, b: Matrix): Matrix = a ::: b

  def combineTo1(pos00: Matrix, pos01: Matrix, pos10: Matrix, pos11: Matrix, finalSize: Int): Matrix = {
    val subSize = finalSize/2

    // [p00] + [p01] => [p00 p01]
    val temp1 = combineCols(pos00,pos01,subSize)
    // [p10] + [p11] => [p10 p11]
    val temp2 = combineCols(pos10,pos11,subSize)

    // [temp1]          [temp1]
    //    +       =>    [     ]
    // [temp2]          [temp2]
    combineRows(temp1,temp2)
  }

  def mul2by2(matrixA: Matrix, matrixB: Matrix): Matrix = {
    val pos00 = (matrixA(0)(0) * matrixB(0)(0)) + (matrixA(0)(1) * matrixB(1)(0))
    val pos01 = (matrixA(0)(0) * matrixB(0)(1)) + (matrixA(0)(1) * matrixB(1)(1))
    val pos10 = (matrixA(1)(0) * matrixB(0)(0)) + (matrixA(1)(1) * matrixB(1)(0))
    val pos11 = (matrixA(1)(0) * matrixB(0)(1)) + (matrixA(1)(1) * matrixB(1)(1))

    List(List(pos00,pos01),List(pos10,pos11))
  }

  def addRow(a: List[Int], b: List[Int]): List[Int] = (a,b).zipped.map((x,y)=> x + y)

  def subRow(a: List[Int], b: List[Int]): List[Int] = (a,b).zipped.map((x,y)=> x - y)

  def addMats(a: Matrix, b: Matrix, size: Int, rowNo: Int = 0): Matrix = {
    if(rowNo == size - 1)
      List(addRow(a(rowNo),b(rowNo)))
    else{
      val currentRow = List(addRow(a(rowNo),b(rowNo)))
      currentRow ::: addMats(a,b,size,rowNo+1)
    }
  }

  def subMats(a: Matrix, b: Matrix, size: Int, rowNo: Int = 0): Matrix = {
    if(rowNo == size - 1)
      List(subRow(a(rowNo),b(rowNo)))
    else{
      val currentRow = List(subRow(a(rowNo),b(rowNo)))
      currentRow ::: subMats(a,b,size,rowNo+1)
    }
  }

  def mulStras(matrixA: Matrix, matrixB: Matrix, size: Int): Matrix = {
    if (size <= 2)
      mul2by2(matrixA,matrixB)
    else {
      val newSize = size/2

      val (a,b,c,d) = splitInto4(matrixA,size)
      val (e,f,g,h) = splitInto4(matrixB,size)

      val p1 = mulStras(a,subMats(f,h,newSize),newSize)
      val p2 = mulStras(addMats(a,b,newSize),h,newSize)
      val p3 = mulStras(addMats(c,d,newSize),e,newSize)
      val p4 = mulStras(d,subMats(g,e,newSize),newSize)
      val p5 = mulStras(addMats(a,d,newSize),addMats(e,h,newSize),newSize)
      val p6 = mulStras(subMats(b,d,newSize),addMats(g,h,newSize),newSize)
      val p7 = mulStras(subMats(a,c,newSize),addMats(e,f,newSize),newSize)

      val c00 = addMats(addMats(p5,p4,newSize),subMats(p6,p2,newSize),newSize)
      val c01 = addMats(p1,p2,newSize)
      val c10 = addMats(p3,p4,newSize)
      val c11 = subMats(addMats(p1,p5,newSize),addMats(p3,p7,newSize),newSize)

      combineTo1(c00,c01,c10,c11,size)
    }
  }

  def printMatrix(matrix: Matrix) = {
    println("--------------")
    matrix.map(x => println(x))
    println("--------------")
  }
  val m1 = getRandomMatrix(16)
  val m2 = getRandomMatrix(16)
  printMatrix(m1)
  printMatrix(m2)
  printMatrix(mulStras(m1,m2,16))
}
