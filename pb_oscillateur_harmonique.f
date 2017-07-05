* L'oscillateur harmonique (pb_oscillateur_harmonique.f)

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
      N = 2
      X0 = 0D0
      XEND = 10D0
      Y0(1) = -1D0
      Y0(2) = 0D0
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
* L'oscillateur, vu comme un probl�me masse-ressort sans frottement
* K = constante de raideur
* M = masse
* C = constante de frottement
      DOUBLE PRECISION K, M, C
      PARAMETER (K = 1D0, M = 1D0, C = 0D0)
*
      F(1) = Y(2)
      F(2) = -K/M*Y(1) - C/M*Y(2)
      END SUBROUTINE

      SUBROUTINE PROBLEM_ERR (N, Y, ERROR)
*
* N     Donn�e.   INTEGER. La dimension de Y
* Y     Donn�e.   DOUBLE PRECISION Y(N). Les variables d�pendantes en X = XEND
* ERROR R�sultat. DOUBLE PRECISION. L'erreur relative globale. -1 si inconnue
*
      INTEGER N
      DOUBLE PRECISION Y(*), ERROR
* Variables locales
      DOUBLE PRECISION XEND, Y_EXACT(2), ERR_ABS(2), 
     $                 NRM_Y_EXACT, NRM_ERR_ABS
* Y_EXACT = la solution exacte (-cos(xend), sin(xend))
      XEND = 10D0
      Y_EXACT(1) = -COS(XEND)
      Y_EXACT(2) =  SIN(XEND)
* La norme 2 de Y_EXACT
      NRM_Y_EXACT = SQRT (Y_EXACT(1)**2 + Y_EXACT(2)**2)
* ERR_ABS = l'erreur absolue = Y_EXACT - Y
      ERR_ABS(1) = Y_EXACT(1) - Y(1)
      ERR_ABS(2) = Y_EXACT(2) - Y(2)
* La norme 2 de l'erreur absolue
      NRM_ERR_ABS = SQRT (ERR_ABS(1)**2 + ERR_ABS(2)**2)
* L'erreur relative
      ERROR = NRM_ERR_ABS / NRM_Y_EXACT
      END SUBROUTINE

