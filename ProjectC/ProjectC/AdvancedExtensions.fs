module ProjectC

open System
open LinAlgDat.Core

type AdvancedOps = class

    /// <summary>
    ///     This function creates the square submatrix given a square matrix as
    ///     well as row and column indices to remove from it.
    /// </summary>
    /// <remarks>
    ///     See page 246-247 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <param name="A">An N-by-N matrix.</param>
    /// <param name="i">The index of the row to remove.</param>
    /// <param name="j">The index of the column to remove.</param>
    /// <returns>The resulting (N - 1)-by-(N - 1) submatrix.</returns>
    static member SquareSubMatrix (A : Matrix) (i : int) (j : int) : Matrix =
        let m_rows = A.M_Rows
        let n_cols = A.N_Cols

        ///The submatrix skaffolding to copy the values into.
        let retval = Array2D.zeroCreate (m_rows-1) (n_cols-1)
        ///The indices into the submatrix, which can differ from the original matrix indices.
        let mutable r_index = 0
        let mutable c_index = 0

        ///Loop over each row of the original matrix, if it is a valid row (i.e not one to be removed), loop over the columns and
        /// If it is a valid column (i.e not one to be removed) copy the value onto the submatrix
        for k in 0..m_rows-1 do
            if (k < i || k > i) then
                for l in 0..n_cols-1 do
                    if ((l < j) || (l > j)) then
                        retval.[r_index, c_index] <- A.[k, l]
                        c_index <- (c_index + 1)
                r_index <- (r_index + 1)
        Matrix retval          

        
    /// <summary>
    ///     This function computes the determinant of a given square matrix.
    /// </summary>
    /// <remarks>
    ///     See page 247 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <remarks>
    ///     Hint: Use SquareSubMatrix.
    /// </remarks>
    /// <param name="A">An N-by-N matrix.</param>
    /// <returns>The determinant of the matrix</returns>
    static member Determinant (A : Matrix) : float =
        raise (NotImplementedException())


    /// <summary>
    ///     This function computes the Euclidean norm of a Vector. This has been implemented
    ///     in Project A and is provided here for convenience
    /// </summary>
    /// <param name="v">
    ///    A Vector
    /// </param>
    /// <returns>
    ///     Euclidean norm, i.e. (\sum v[i]^2)^0.5
    /// </returns>
    static member VectorNorm (v : Vector) =
        let mutable n2 = 0.0
        for i in 0..v.Size-1 do
            n2 <- n2 + v.[i] * v.[i]
        sqrt n2
    


    /// <summary>
    ///     This function copies Vector 'v' as a column of matrix 'A'
    ///     at column position j.
    /// </summary>
    /// <param name="A">
    ///     An M-by-N matrix.
    /// </param>
    /// <param name="v">
    ///     Vector objects that must be copied in A.
    /// </param>
    /// <param name="j">
    ///     column number.
    /// </param>
    /// <returns>
    ///     An M-by-N matrix after modification.
    /// </returns>
    /// <exception cref="ArgumentException"></exception>
    static member SetColumn (A : Matrix) (v : Vector) (j : int) =
        raise (NotImplementedException())


    /// <summary>
    ///     This function computes the Gram-Schmidt process on a given matrix.
    /// </summary>
    /// <remarks>
    ///     See page 229 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <param name="A">
    ///     An M-by-N matrix. All columns are implicitly assumed linear
    ///     independent.
    /// </param>
    /// <returns>
    ///     A tuple (Q,R) where Q is a M-by-N orthonormal matrix and R is an
    ///     N-by-N upper triangular matrix.
    /// </returns>
    static member GramSchmidt (A : Matrix) : Matrix * Matrix =
        raise (NotImplementedException())
end

























