module ProjectA

open System
open LinAlgDat.Core

type BasicOps = class
  /// <summary>
  /// This function creates an augmented Matrix given a Matrix A, and a
  /// right-hand side Vector v.
  /// </summary>
  ///
  /// <remarks>
  /// See page 12 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="v">An M-size Vector.</param>
  ///
  /// <returns>An M-by-(N+1) augmented Matrix [A | v].</returns>
  static member AugmentRight (A : Matrix) (v : Vector) : Matrix =
    let m_rows = A.M_Rows
    let n_cols = A.N_Cols

    let retval = Array2D.zeroCreate m_rows (n_cols + 1)

    for i in 0..m_rows-1 do
        for j in 0..n_cols-1 do
            retval.[i, j] <- A.[i, j]
        retval.[i, n_cols] <- v.[i]
    Matrix retval

  /// <summary>
  /// This function computes the Matrix-Vector product of a Matrix A,
  /// and a column Vector v.
  /// </summary>
  ///
  /// <remarks>
  /// See page 68 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="v">An N-size Vector.</param>
  ///
  /// <returns>An M-size Vector b such that b = A * v.</returns>
  static member MatVecProduct (A : Matrix) (v : Vector) : Vector =
     let m_rows = A.M_Rows
     let n_cols = A.N_Cols
     let mutable b =  Vector(m_rows)

     ///Iterate over the rows of A, for each row calculate the carry - ie b_i = sum_{j} a_ij*v_i 
     for i in 0..m_rows-1 do
          let mutable carry = 0.0
          for j in 0..n_cols-1 do
              carry <- carry + (A.[i, j] * v.[j])
          b.[i] <- carry
     b

  /// <summary>
  /// This function computes the Matrix product of two given matrices
  /// A and B.
  /// </summary>
  ///
  /// <remarks>
  /// See page 58 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarls>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="B">An N-by-P Matrix.</param>
  ///
  /// <returns>The M-by-P Matrix A * B.</returns>
  static member MatrixProduct (A : Matrix) (B : Matrix) : Matrix =
    let m_rows = A.M_Rows
    let n_cols = A.N_Cols
    let p_cols = B.N_Cols
    let mutable retval = Matrix(m_rows, p_cols)

    ///We iterate over each row in A, for each row we calculate the carry - ie c_ij = sum_{j, k} a_ij * b_kj
    for i in 0..m_rows-1 do
        for j in 0..p_cols-1 do
            let mutable carry = 0.0
            for k in 0..n_cols-1 do
                carry <- carry + (A.[i, k] * B.[k, j])
            retval.[i, j] <- carry

    retval
    // An attempt to utilize the previous functions,  as a way to try and avoid "column major behavior" on "row major" matrix 'object'.
    ///let m_rows = A.M_Rows
    ///let n_cols = A.N_Cols
    ///let p_cols = B.N_Cols
    ///let mutable retval = MatVecProduct(A, B.column[0])
    ///let mutable carry  = Vector(m_rows)
    ///
    ///for i in 1..p_cols-1 do
    ///      carry <- MatVecProduct(A, B.column[i])
    ///      retval <- AugmentRight(retval, carry)
    ///retval
    
  /// <summary>
  /// This function computes the transpose of a given Matrix.
  /// </summary>
  ///
  /// <remarks>
  /// See page 69 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  ///
  /// <returns>The N-by-M Matrix B such that B = A^T.</returns>
  static member Transpose (A : Matrix) : Matrix =
    let m_rows = A.M_Rows
    let n_cols = A.N_Cols
    let mutable retval =  Matrix(n_cols, m_rows)

    /// We iterate over each item and set the corresponding in the mutable. An attempt was made to utilize row-first indexing
    for i in 0..m_rows-1 do
        for j in 0..n_cols-1 do
            retval.[j, i] <- A.[i, j]
    retval

  /// <summary>
  /// This function computes the Euclidean Vector norm of a given
  /// Vector.
  /// </summary>
  ///
  /// <remarks>
  /// See page 197 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="v">An N-dimensional Vector.</param>
  ///
  /// <returns>The Euclidean norm of the Vector.</returns>
  static member VectorNorm (v : Vector) : float =
    let size = v.Size
    let mutable carry = 0.0

    /// we iterate over each item in the vector, squaring them and storing them in a temporary mutable, before returning the result of the sqrt function.
    for i in 0..size-1 do
      carry <- carry + (float)v.[i]**2.0

    sqrt(carry)

  /// <summary>
  /// This function creates the square submatrix given a square
  /// Matrix A, as well as row, and column indices to remove from A.
  /// </summary>
  ///
  /// <remarks>
  /// See "square submatrix" on page 246-247 in "Linear Algebra for
  /// Engineers and Scientists" by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">A square Matrix.</param>
  /// <param name="i">The index of the row to remove from A</param>
  /// <param name="j">The index of the column to remove from A</param>
  ///
  /// <returns>A square submatrix of the Matrix A.</returns>
  static member SquareSubMatrix (A : Matrix) (i : int) (j : int) : Matrix =
     let mutable M =  Matrix(1,1)
     M

end
