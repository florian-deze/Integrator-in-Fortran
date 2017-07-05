* Le probl�me de l'exponentielle

      SUBROUTINE PROBLEM_INIT (N, X0, XEND, Y0)
*
* N     R�sultat. INTEGER. La dimension de Y0
* X0    R�sultat. DOUBLE PRECISION. Borne inf. de l'intervalle d'int�gration
* XEND  R�sultat. DOUBLE PRECISION. Borne sup. de l'intervalle d'int�gration
* Y0    R�sultat. DOUBLE PRECISION Y0(N). Les conditions initiales
*
      INTEGER N
      DOUBLE PRECISION X0, XEND, Y0(*)
*
      X0 = 0D0
      XEND = 1D0
      Y0(1) = 1D0
      N = 1
      END SUBROUTINE

      SUBROUTINE PROBLEM_F (N, X, Y, F)
      INTEGER N
      DOUBLE PRECISION X, Y(*), F(*)
*
* N     Donn�e.   INTEGER. La dimension de Y et de F
* X     Donn�e.   DOUBLE PRECISION. La variable ind�pendante
* Y     Donn�e.   DOUBLE PRECISION Y(N). Les valeurs des variables d�pendantes
* F     R�sultat. DOUBLE PRECISION F(N). Les vitesses des variables d�pendantes
*
      F(1) = Y(1)
      END SUBROUTINE

      SUBROUTINE PROBLEM_ERR (N, Y, ERROR)
*
* N     Donn�e.   INTEGER. La dimension de Y
* Y     Donn�e.   DOUBLE PRECISION Y(N). Les variables d�pendantes en X = XEND
* ERROR R�sultat. DOUBLE PRECISION. L'erreur relative globale. -1 si inconnue
*
      INTEGER N
      DOUBLE PRECISION Y(*), ERROR
*
      ERROR = ABS(EXP(1D0) - Y(1))/EXP(1D0)
      END SUBROUTINE

