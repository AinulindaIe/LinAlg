module ProjectB

open System
open LinAlgDat.Core

type GaussOps = class

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
                retval.[i,j] <- A.[i,j]
            retval.[i,n_cols] <- v.[i]
        Matrix retval

    /// <summary>
    /// This function computes the elementary row replacement operation on
    /// the given matrix.
    /// </summary>
    ///
    /// <remarks>
    /// Note that we add the row (as in the lectures) instead of subtracting
    /// the row (as in the textbook).
    /// </remarks>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">
    /// The index of the row to replace.
    /// </param>
    /// <param name="m">
    /// The multiple of row j to add to row i.
    /// </param>
    /// <param name="j">
    /// The index of the row whose mutiple is added to row i.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowReplacement (A : Matrix) (i : int) (m : float) (j : int) : Matrix =
        let m_rows = A.M_Rows
        let n_cols = A.N_Cols

        for k in 0..n_cols-1 do
            A.[i, k] <- A.[i,k] + m*A.[j, k]

        Matrix A    

    /// <summary>
    /// This function computes the elementary row interchange operation on
    /// the given matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">
    /// The index of the first row of the rows to interchange.
    /// </param>
    /// <param name="j">
    /// The index of the second row of the rows to interchange.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowInterchange (A : Matrix) (i : int) (j : int) : Matrix =
        let m_rows = A.M_Rows
        let n_cols = A.N_Cols

        let placeholder = Array2D.zeroCreate 1 n_cols

        for k in 0..n_cols-1 do
            placeholder.[0, k] <- A.[i, k]
            A.[i, k] <- A.[j, k]
            A.[j, k] <- placeholder.[0, k]

        Matrix A

    /// <summary>
    /// This function computes the elementary row scaling operation on the
    /// given matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">The index of the row to scale.</param>
    /// <param name="c">The value to scale the row by.</param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowScaling (A : Matrix) (i : int) (c : float) : Matrix =
        let n_cols = A.N_Cols

        for k in 0..n_cols-1 do
            A.[i, k] <- A.[i, k]*c
        
        Matrix A

    /// <summary>
    /// This function executes the forward reduction algorithm provided in
    /// the assignment text to achieve row Echelon form of a given
    /// augmented matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix, augmented (or not).
    /// </param>
    ///
    /// <returns>
    /// An M-by-N matrix that is the row Echelon form.
    /// </returns>
    static member ForwardReduction (M : Matrix) : Matrix =

        // One does simply not compare a float number with 0.0
        // A not-so-scientific way, but quite sufficient to this course,
        //// is to have a threshold value, which is defined as below.
        ////
        //let tolerance = 0.00000001

        //let m_rows = M.M_Rows
        //let n_cols = M.N_Cols
        //let mutable cur_row = 0
        //let mutable cur_col = 0
        //let mutable pivot = -1 

        //let mutable finished = (m_rows < 1) || (n_cols < 1)
        //
        //while (not finished) do
        //    for r in cur_row..m_rows-1 do
        //        if (M.[r, cur_col] )
        //            pivot <- r
        //            break
        //    if pivot < 0
        //        continue
        //    
        //    M.ElementaryRowInterchange(pivot, cur_row)

        //    for r in cur_row+1..m_rows-1 do
        //        if Math.Abs(M.[r, cur_col]) > tolerance
        //            let factor = - (M.[r, cur_col] / M.[cur_row, cur_col])
        //            M.ElementaryRowReplacement(r, factor, cur_row)

        //    pivot <- -1
        //    cur_col <- cur_col + 1
        //    cur_row <- cur_row + 1

        //    finished <- (cur_row == m_rows) || (cur_col == n_cols)
        //
        //Matrix M
        raise (NotImplementedException())

    /// <summary>
    /// This function executes the backward reduction algorithm provided in
    /// the assignment text given an augmented matrix in row Echelon form.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N augmented matrix in row Echelon form.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after executing the algorithm.
    /// </returns>
    static member BackwardReduction (A : Matrix) : Matrix =
        let tolerance = 0.00000001
        raise (NotImplementedException())

    /// <summary>
    /// This function performs Gauss elimination of a linear system
    /// given in matrix form by a coefficient matrix and a right hand side
    /// vector. It is assumed that the corresponding linear system is
    /// consistent and has exactly one solution.
    /// </summary>
    ///
    /// <remarks>
    /// Hint: Combine ForwardReduction and BackwardReduction.
    /// </remarks>
    ///
    /// <param name="A">An M-by-N Matrix.</param>
    /// <param name="b">An M-size Vector.</param>
    ///
    /// <returns>The N-sized vector x such that A * x = b.</returns>
    static member GaussElimination (A : Matrix) (b : Vector) : Vector =
        raise (NotImplementedException())

end
